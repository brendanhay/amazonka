{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDisassociateSkillGroupFromRoom' smart constructor.
data DisassociateSkillGroupFromRoom = DisassociateSkillGroupFromRoom'
  { -- | The ARN of the room from which the skill group is to be disassociated.
    -- Required.
    roomArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the skill group to disassociate from a room. Required.
    skillGroupArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      skillGroupArn = Prelude.Nothing
    }

-- | The ARN of the room from which the skill group is to be disassociated.
-- Required.
disassociateSkillGroupFromRoom_roomArn :: Lens.Lens' DisassociateSkillGroupFromRoom (Prelude.Maybe Prelude.Text)
disassociateSkillGroupFromRoom_roomArn = Lens.lens (\DisassociateSkillGroupFromRoom' {roomArn} -> roomArn) (\s@DisassociateSkillGroupFromRoom' {} a -> s {roomArn = a} :: DisassociateSkillGroupFromRoom)

-- | The ARN of the skill group to disassociate from a room. Required.
disassociateSkillGroupFromRoom_skillGroupArn :: Lens.Lens' DisassociateSkillGroupFromRoom (Prelude.Maybe Prelude.Text)
disassociateSkillGroupFromRoom_skillGroupArn = Lens.lens (\DisassociateSkillGroupFromRoom' {skillGroupArn} -> skillGroupArn) (\s@DisassociateSkillGroupFromRoom' {} a -> s {skillGroupArn = a} :: DisassociateSkillGroupFromRoom)

instance
  Prelude.AWSRequest
    DisassociateSkillGroupFromRoom
  where
  type
    Rs DisassociateSkillGroupFromRoom =
      DisassociateSkillGroupFromRoomResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociateSkillGroupFromRoomResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DisassociateSkillGroupFromRoom

instance
  Prelude.NFData
    DisassociateSkillGroupFromRoom

instance
  Prelude.ToHeaders
    DisassociateSkillGroupFromRoom
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AlexaForBusiness.DisassociateSkillGroupFromRoom" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance
  Prelude.ToJSON
    DisassociateSkillGroupFromRoom
  where
  toJSON DisassociateSkillGroupFromRoom' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("RoomArn" Prelude..=) Prelude.<$> roomArn,
            ("SkillGroupArn" Prelude..=)
              Prelude.<$> skillGroupArn
          ]
      )

instance
  Prelude.ToPath
    DisassociateSkillGroupFromRoom
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    DisassociateSkillGroupFromRoom
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateSkillGroupFromRoomResponse' smart constructor.
data DisassociateSkillGroupFromRoomResponse = DisassociateSkillGroupFromRoomResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DisassociateSkillGroupFromRoomResponse
newDisassociateSkillGroupFromRoomResponse
  pHttpStatus_ =
    DisassociateSkillGroupFromRoomResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
disassociateSkillGroupFromRoomResponse_httpStatus :: Lens.Lens' DisassociateSkillGroupFromRoomResponse Prelude.Int
disassociateSkillGroupFromRoomResponse_httpStatus = Lens.lens (\DisassociateSkillGroupFromRoomResponse' {httpStatus} -> httpStatus) (\s@DisassociateSkillGroupFromRoomResponse' {} a -> s {httpStatus = a} :: DisassociateSkillGroupFromRoomResponse)

instance
  Prelude.NFData
    DisassociateSkillGroupFromRoomResponse
