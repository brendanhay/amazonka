{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.ResolveRoom
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Determines the details for the room from which a skill request was
-- invoked. This operation is used by skill developers.
--
-- To query ResolveRoom from an Alexa skill, the skill ID needs to be
-- authorized. When the skill is using an AWS Lambda function, the skill is
-- automatically authorized when you publish your skill as a private skill
-- to your AWS account. Skills that are hosted using a custom web service
-- must be manually authorized. To get your skill authorized, contact AWS
-- Support with your AWS account ID that queries the ResolveRoom API and
-- skill ID.
module Network.AWS.AlexaBusiness.ResolveRoom
  ( -- * Creating a Request
    ResolveRoom (..),
    newResolveRoom,

    -- * Request Lenses
    resolveRoom_userId,
    resolveRoom_skillId,

    -- * Destructuring the Response
    ResolveRoomResponse (..),
    newResolveRoomResponse,

    -- * Response Lenses
    resolveRoomResponse_roomSkillParameters,
    resolveRoomResponse_roomArn,
    resolveRoomResponse_roomName,
    resolveRoomResponse_httpStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newResolveRoom' smart constructor.
data ResolveRoom = ResolveRoom'
  { -- | The ARN of the user. Required.
    userId :: Prelude.Text,
    -- | The ARN of the skill that was requested. Required.
    skillId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResolveRoom' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userId', 'resolveRoom_userId' - The ARN of the user. Required.
--
-- 'skillId', 'resolveRoom_skillId' - The ARN of the skill that was requested. Required.
newResolveRoom ::
  -- | 'userId'
  Prelude.Text ->
  -- | 'skillId'
  Prelude.Text ->
  ResolveRoom
newResolveRoom pUserId_ pSkillId_ =
  ResolveRoom'
    { userId = pUserId_,
      skillId = pSkillId_
    }

-- | The ARN of the user. Required.
resolveRoom_userId :: Lens.Lens' ResolveRoom Prelude.Text
resolveRoom_userId = Lens.lens (\ResolveRoom' {userId} -> userId) (\s@ResolveRoom' {} a -> s {userId = a} :: ResolveRoom)

-- | The ARN of the skill that was requested. Required.
resolveRoom_skillId :: Lens.Lens' ResolveRoom Prelude.Text
resolveRoom_skillId = Lens.lens (\ResolveRoom' {skillId} -> skillId) (\s@ResolveRoom' {} a -> s {skillId = a} :: ResolveRoom)

instance Core.AWSRequest ResolveRoom where
  type AWSResponse ResolveRoom = ResolveRoomResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ResolveRoomResponse'
            Prelude.<$> ( x Core..?> "RoomSkillParameters"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "RoomArn")
            Prelude.<*> (x Core..?> "RoomName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ResolveRoom

instance Prelude.NFData ResolveRoom

instance Core.ToHeaders ResolveRoom where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AlexaForBusiness.ResolveRoom" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ResolveRoom where
  toJSON ResolveRoom' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("UserId" Core..= userId),
            Prelude.Just ("SkillId" Core..= skillId)
          ]
      )

instance Core.ToPath ResolveRoom where
  toPath = Prelude.const "/"

instance Core.ToQuery ResolveRoom where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newResolveRoomResponse' smart constructor.
data ResolveRoomResponse = ResolveRoomResponse'
  { -- | Response to get the room profile request. Required.
    roomSkillParameters :: Prelude.Maybe [RoomSkillParameter],
    -- | The ARN of the room from which the skill request was invoked.
    roomArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the room from which the skill request was invoked.
    roomName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResolveRoomResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roomSkillParameters', 'resolveRoomResponse_roomSkillParameters' - Response to get the room profile request. Required.
--
-- 'roomArn', 'resolveRoomResponse_roomArn' - The ARN of the room from which the skill request was invoked.
--
-- 'roomName', 'resolveRoomResponse_roomName' - The name of the room from which the skill request was invoked.
--
-- 'httpStatus', 'resolveRoomResponse_httpStatus' - The response's http status code.
newResolveRoomResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ResolveRoomResponse
newResolveRoomResponse pHttpStatus_ =
  ResolveRoomResponse'
    { roomSkillParameters =
        Prelude.Nothing,
      roomArn = Prelude.Nothing,
      roomName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Response to get the room profile request. Required.
resolveRoomResponse_roomSkillParameters :: Lens.Lens' ResolveRoomResponse (Prelude.Maybe [RoomSkillParameter])
resolveRoomResponse_roomSkillParameters = Lens.lens (\ResolveRoomResponse' {roomSkillParameters} -> roomSkillParameters) (\s@ResolveRoomResponse' {} a -> s {roomSkillParameters = a} :: ResolveRoomResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The ARN of the room from which the skill request was invoked.
resolveRoomResponse_roomArn :: Lens.Lens' ResolveRoomResponse (Prelude.Maybe Prelude.Text)
resolveRoomResponse_roomArn = Lens.lens (\ResolveRoomResponse' {roomArn} -> roomArn) (\s@ResolveRoomResponse' {} a -> s {roomArn = a} :: ResolveRoomResponse)

-- | The name of the room from which the skill request was invoked.
resolveRoomResponse_roomName :: Lens.Lens' ResolveRoomResponse (Prelude.Maybe Prelude.Text)
resolveRoomResponse_roomName = Lens.lens (\ResolveRoomResponse' {roomName} -> roomName) (\s@ResolveRoomResponse' {} a -> s {roomName = a} :: ResolveRoomResponse)

-- | The response's http status code.
resolveRoomResponse_httpStatus :: Lens.Lens' ResolveRoomResponse Prelude.Int
resolveRoomResponse_httpStatus = Lens.lens (\ResolveRoomResponse' {httpStatus} -> httpStatus) (\s@ResolveRoomResponse' {} a -> s {httpStatus = a} :: ResolveRoomResponse)

instance Prelude.NFData ResolveRoomResponse
