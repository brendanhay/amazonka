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
-- Module      : Network.AWS.AlexaBusiness.DisassociateSkillGroupFromRoom
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a skill group from a specified room. This disables all
-- skills in the skill group on all devices in the room.
module Network.AWS.AlexaBusiness.DisassociateSkillGroupFromRoom
  ( -- * Creating a Request
    DisassociateSkillGroupFromRoom (..),
    newDisassociateSkillGroupFromRoom,

    -- * Request Lenses
    disassociateSkillGroupFromRoom_roomArn,
    disassociateSkillGroupFromRoom_skillGroupArn,

    -- * Destructuring the Response
    DisassociateSkillGroupFromRoomResponse (..),
    newDisassociateSkillGroupFromRoomResponse,

    -- * Response Lenses
    disassociateSkillGroupFromRoomResponse_httpStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDisassociateSkillGroupFromRoom' smart constructor.
data DisassociateSkillGroupFromRoom = DisassociateSkillGroupFromRoom'
  { -- | The ARN of the room from which the skill group is to be disassociated.
    -- Required.
    roomArn :: Core.Maybe Core.Text,
    -- | The ARN of the skill group to disassociate from a room. Required.
    skillGroupArn :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DisassociateSkillGroupFromRoom' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roomArn', 'disassociateSkillGroupFromRoom_roomArn' - The ARN of the room from which the skill group is to be disassociated.
-- Required.
--
-- 'skillGroupArn', 'disassociateSkillGroupFromRoom_skillGroupArn' - The ARN of the skill group to disassociate from a room. Required.
newDisassociateSkillGroupFromRoom ::
  DisassociateSkillGroupFromRoom
newDisassociateSkillGroupFromRoom =
  DisassociateSkillGroupFromRoom'
    { roomArn =
        Core.Nothing,
      skillGroupArn = Core.Nothing
    }

-- | The ARN of the room from which the skill group is to be disassociated.
-- Required.
disassociateSkillGroupFromRoom_roomArn :: Lens.Lens' DisassociateSkillGroupFromRoom (Core.Maybe Core.Text)
disassociateSkillGroupFromRoom_roomArn = Lens.lens (\DisassociateSkillGroupFromRoom' {roomArn} -> roomArn) (\s@DisassociateSkillGroupFromRoom' {} a -> s {roomArn = a} :: DisassociateSkillGroupFromRoom)

-- | The ARN of the skill group to disassociate from a room. Required.
disassociateSkillGroupFromRoom_skillGroupArn :: Lens.Lens' DisassociateSkillGroupFromRoom (Core.Maybe Core.Text)
disassociateSkillGroupFromRoom_skillGroupArn = Lens.lens (\DisassociateSkillGroupFromRoom' {skillGroupArn} -> skillGroupArn) (\s@DisassociateSkillGroupFromRoom' {} a -> s {skillGroupArn = a} :: DisassociateSkillGroupFromRoom)

instance
  Core.AWSRequest
    DisassociateSkillGroupFromRoom
  where
  type
    AWSResponse DisassociateSkillGroupFromRoom =
      DisassociateSkillGroupFromRoomResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociateSkillGroupFromRoomResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DisassociateSkillGroupFromRoom

instance Core.NFData DisassociateSkillGroupFromRoom

instance
  Core.ToHeaders
    DisassociateSkillGroupFromRoom
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AlexaForBusiness.DisassociateSkillGroupFromRoom" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DisassociateSkillGroupFromRoom where
  toJSON DisassociateSkillGroupFromRoom' {..} =
    Core.object
      ( Core.catMaybes
          [ ("RoomArn" Core..=) Core.<$> roomArn,
            ("SkillGroupArn" Core..=) Core.<$> skillGroupArn
          ]
      )

instance Core.ToPath DisassociateSkillGroupFromRoom where
  toPath = Core.const "/"

instance Core.ToQuery DisassociateSkillGroupFromRoom where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDisassociateSkillGroupFromRoomResponse' smart constructor.
data DisassociateSkillGroupFromRoomResponse = DisassociateSkillGroupFromRoomResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DisassociateSkillGroupFromRoomResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'disassociateSkillGroupFromRoomResponse_httpStatus' - The response's http status code.
newDisassociateSkillGroupFromRoomResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DisassociateSkillGroupFromRoomResponse
newDisassociateSkillGroupFromRoomResponse
  pHttpStatus_ =
    DisassociateSkillGroupFromRoomResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
disassociateSkillGroupFromRoomResponse_httpStatus :: Lens.Lens' DisassociateSkillGroupFromRoomResponse Core.Int
disassociateSkillGroupFromRoomResponse_httpStatus = Lens.lens (\DisassociateSkillGroupFromRoomResponse' {httpStatus} -> httpStatus) (\s@DisassociateSkillGroupFromRoomResponse' {} a -> s {httpStatus = a} :: DisassociateSkillGroupFromRoomResponse)

instance
  Core.NFData
    DisassociateSkillGroupFromRoomResponse
