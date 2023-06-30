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
-- Module      : Amazonka.AlexaBusiness.ResolveRoom
-- Copyright   : (c) 2013-2023 Brendan Hay
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
module Amazonka.AlexaBusiness.ResolveRoom
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
    resolveRoomResponse_roomArn,
    resolveRoomResponse_roomName,
    resolveRoomResponse_roomSkillParameters,
    resolveRoomResponse_httpStatus,
  )
where

import Amazonka.AlexaBusiness.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ResolveRoomResponse'
            Prelude.<$> (x Data..?> "RoomArn")
            Prelude.<*> (x Data..?> "RoomName")
            Prelude.<*> ( x
                            Data..?> "RoomSkillParameters"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ResolveRoom where
  hashWithSalt _salt ResolveRoom' {..} =
    _salt
      `Prelude.hashWithSalt` userId
      `Prelude.hashWithSalt` skillId

instance Prelude.NFData ResolveRoom where
  rnf ResolveRoom' {..} =
    Prelude.rnf userId
      `Prelude.seq` Prelude.rnf skillId

instance Data.ToHeaders ResolveRoom where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AlexaForBusiness.ResolveRoom" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ResolveRoom where
  toJSON ResolveRoom' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("UserId" Data..= userId),
            Prelude.Just ("SkillId" Data..= skillId)
          ]
      )

instance Data.ToPath ResolveRoom where
  toPath = Prelude.const "/"

instance Data.ToQuery ResolveRoom where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newResolveRoomResponse' smart constructor.
data ResolveRoomResponse = ResolveRoomResponse'
  { -- | The ARN of the room from which the skill request was invoked.
    roomArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the room from which the skill request was invoked.
    roomName :: Prelude.Maybe Prelude.Text,
    -- | Response to get the room profile request. Required.
    roomSkillParameters :: Prelude.Maybe [RoomSkillParameter],
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
-- 'roomArn', 'resolveRoomResponse_roomArn' - The ARN of the room from which the skill request was invoked.
--
-- 'roomName', 'resolveRoomResponse_roomName' - The name of the room from which the skill request was invoked.
--
-- 'roomSkillParameters', 'resolveRoomResponse_roomSkillParameters' - Response to get the room profile request. Required.
--
-- 'httpStatus', 'resolveRoomResponse_httpStatus' - The response's http status code.
newResolveRoomResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ResolveRoomResponse
newResolveRoomResponse pHttpStatus_ =
  ResolveRoomResponse'
    { roomArn = Prelude.Nothing,
      roomName = Prelude.Nothing,
      roomSkillParameters = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the room from which the skill request was invoked.
resolveRoomResponse_roomArn :: Lens.Lens' ResolveRoomResponse (Prelude.Maybe Prelude.Text)
resolveRoomResponse_roomArn = Lens.lens (\ResolveRoomResponse' {roomArn} -> roomArn) (\s@ResolveRoomResponse' {} a -> s {roomArn = a} :: ResolveRoomResponse)

-- | The name of the room from which the skill request was invoked.
resolveRoomResponse_roomName :: Lens.Lens' ResolveRoomResponse (Prelude.Maybe Prelude.Text)
resolveRoomResponse_roomName = Lens.lens (\ResolveRoomResponse' {roomName} -> roomName) (\s@ResolveRoomResponse' {} a -> s {roomName = a} :: ResolveRoomResponse)

-- | Response to get the room profile request. Required.
resolveRoomResponse_roomSkillParameters :: Lens.Lens' ResolveRoomResponse (Prelude.Maybe [RoomSkillParameter])
resolveRoomResponse_roomSkillParameters = Lens.lens (\ResolveRoomResponse' {roomSkillParameters} -> roomSkillParameters) (\s@ResolveRoomResponse' {} a -> s {roomSkillParameters = a} :: ResolveRoomResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
resolveRoomResponse_httpStatus :: Lens.Lens' ResolveRoomResponse Prelude.Int
resolveRoomResponse_httpStatus = Lens.lens (\ResolveRoomResponse' {httpStatus} -> httpStatus) (\s@ResolveRoomResponse' {} a -> s {httpStatus = a} :: ResolveRoomResponse)

instance Prelude.NFData ResolveRoomResponse where
  rnf ResolveRoomResponse' {..} =
    Prelude.rnf roomArn
      `Prelude.seq` Prelude.rnf roomName
      `Prelude.seq` Prelude.rnf roomSkillParameters
      `Prelude.seq` Prelude.rnf httpStatus
