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
-- Module      : Network.AWS.AlexaBusiness.AssociateSkillGroupWithRoom
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a skill group with a given room. This enables all skills in
-- the associated skill group on all devices in the room.
module Network.AWS.AlexaBusiness.AssociateSkillGroupWithRoom
  ( -- * Creating a Request
    AssociateSkillGroupWithRoom (..),
    newAssociateSkillGroupWithRoom,

    -- * Request Lenses
    associateSkillGroupWithRoom_roomArn,
    associateSkillGroupWithRoom_skillGroupArn,

    -- * Destructuring the Response
    AssociateSkillGroupWithRoomResponse (..),
    newAssociateSkillGroupWithRoomResponse,

    -- * Response Lenses
    associateSkillGroupWithRoomResponse_httpStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAssociateSkillGroupWithRoom' smart constructor.
data AssociateSkillGroupWithRoom = AssociateSkillGroupWithRoom'
  { -- | The ARN of the room with which to associate the skill group. Required.
    roomArn :: Core.Maybe Core.Text,
    -- | The ARN of the skill group to associate with a room. Required.
    skillGroupArn :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AssociateSkillGroupWithRoom' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roomArn', 'associateSkillGroupWithRoom_roomArn' - The ARN of the room with which to associate the skill group. Required.
--
-- 'skillGroupArn', 'associateSkillGroupWithRoom_skillGroupArn' - The ARN of the skill group to associate with a room. Required.
newAssociateSkillGroupWithRoom ::
  AssociateSkillGroupWithRoom
newAssociateSkillGroupWithRoom =
  AssociateSkillGroupWithRoom'
    { roomArn =
        Core.Nothing,
      skillGroupArn = Core.Nothing
    }

-- | The ARN of the room with which to associate the skill group. Required.
associateSkillGroupWithRoom_roomArn :: Lens.Lens' AssociateSkillGroupWithRoom (Core.Maybe Core.Text)
associateSkillGroupWithRoom_roomArn = Lens.lens (\AssociateSkillGroupWithRoom' {roomArn} -> roomArn) (\s@AssociateSkillGroupWithRoom' {} a -> s {roomArn = a} :: AssociateSkillGroupWithRoom)

-- | The ARN of the skill group to associate with a room. Required.
associateSkillGroupWithRoom_skillGroupArn :: Lens.Lens' AssociateSkillGroupWithRoom (Core.Maybe Core.Text)
associateSkillGroupWithRoom_skillGroupArn = Lens.lens (\AssociateSkillGroupWithRoom' {skillGroupArn} -> skillGroupArn) (\s@AssociateSkillGroupWithRoom' {} a -> s {skillGroupArn = a} :: AssociateSkillGroupWithRoom)

instance Core.AWSRequest AssociateSkillGroupWithRoom where
  type
    AWSResponse AssociateSkillGroupWithRoom =
      AssociateSkillGroupWithRoomResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          AssociateSkillGroupWithRoomResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable AssociateSkillGroupWithRoom

instance Core.NFData AssociateSkillGroupWithRoom

instance Core.ToHeaders AssociateSkillGroupWithRoom where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AlexaForBusiness.AssociateSkillGroupWithRoom" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON AssociateSkillGroupWithRoom where
  toJSON AssociateSkillGroupWithRoom' {..} =
    Core.object
      ( Core.catMaybes
          [ ("RoomArn" Core..=) Core.<$> roomArn,
            ("SkillGroupArn" Core..=) Core.<$> skillGroupArn
          ]
      )

instance Core.ToPath AssociateSkillGroupWithRoom where
  toPath = Core.const "/"

instance Core.ToQuery AssociateSkillGroupWithRoom where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newAssociateSkillGroupWithRoomResponse' smart constructor.
data AssociateSkillGroupWithRoomResponse = AssociateSkillGroupWithRoomResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AssociateSkillGroupWithRoomResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'associateSkillGroupWithRoomResponse_httpStatus' - The response's http status code.
newAssociateSkillGroupWithRoomResponse ::
  -- | 'httpStatus'
  Core.Int ->
  AssociateSkillGroupWithRoomResponse
newAssociateSkillGroupWithRoomResponse pHttpStatus_ =
  AssociateSkillGroupWithRoomResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
associateSkillGroupWithRoomResponse_httpStatus :: Lens.Lens' AssociateSkillGroupWithRoomResponse Core.Int
associateSkillGroupWithRoomResponse_httpStatus = Lens.lens (\AssociateSkillGroupWithRoomResponse' {httpStatus} -> httpStatus) (\s@AssociateSkillGroupWithRoomResponse' {} a -> s {httpStatus = a} :: AssociateSkillGroupWithRoomResponse)

instance
  Core.NFData
    AssociateSkillGroupWithRoomResponse
