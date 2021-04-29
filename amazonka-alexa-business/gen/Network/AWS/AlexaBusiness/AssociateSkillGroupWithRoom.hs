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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAssociateSkillGroupWithRoom' smart constructor.
data AssociateSkillGroupWithRoom = AssociateSkillGroupWithRoom'
  { -- | The ARN of the room with which to associate the skill group. Required.
    roomArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the skill group to associate with a room. Required.
    skillGroupArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      skillGroupArn = Prelude.Nothing
    }

-- | The ARN of the room with which to associate the skill group. Required.
associateSkillGroupWithRoom_roomArn :: Lens.Lens' AssociateSkillGroupWithRoom (Prelude.Maybe Prelude.Text)
associateSkillGroupWithRoom_roomArn = Lens.lens (\AssociateSkillGroupWithRoom' {roomArn} -> roomArn) (\s@AssociateSkillGroupWithRoom' {} a -> s {roomArn = a} :: AssociateSkillGroupWithRoom)

-- | The ARN of the skill group to associate with a room. Required.
associateSkillGroupWithRoom_skillGroupArn :: Lens.Lens' AssociateSkillGroupWithRoom (Prelude.Maybe Prelude.Text)
associateSkillGroupWithRoom_skillGroupArn = Lens.lens (\AssociateSkillGroupWithRoom' {skillGroupArn} -> skillGroupArn) (\s@AssociateSkillGroupWithRoom' {} a -> s {skillGroupArn = a} :: AssociateSkillGroupWithRoom)

instance
  Prelude.AWSRequest
    AssociateSkillGroupWithRoom
  where
  type
    Rs AssociateSkillGroupWithRoom =
      AssociateSkillGroupWithRoomResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          AssociateSkillGroupWithRoomResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AssociateSkillGroupWithRoom

instance Prelude.NFData AssociateSkillGroupWithRoom

instance
  Prelude.ToHeaders
    AssociateSkillGroupWithRoom
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AlexaForBusiness.AssociateSkillGroupWithRoom" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON AssociateSkillGroupWithRoom where
  toJSON AssociateSkillGroupWithRoom' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("RoomArn" Prelude..=) Prelude.<$> roomArn,
            ("SkillGroupArn" Prelude..=)
              Prelude.<$> skillGroupArn
          ]
      )

instance Prelude.ToPath AssociateSkillGroupWithRoom where
  toPath = Prelude.const "/"

instance Prelude.ToQuery AssociateSkillGroupWithRoom where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateSkillGroupWithRoomResponse' smart constructor.
data AssociateSkillGroupWithRoomResponse = AssociateSkillGroupWithRoomResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  AssociateSkillGroupWithRoomResponse
newAssociateSkillGroupWithRoomResponse pHttpStatus_ =
  AssociateSkillGroupWithRoomResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
associateSkillGroupWithRoomResponse_httpStatus :: Lens.Lens' AssociateSkillGroupWithRoomResponse Prelude.Int
associateSkillGroupWithRoomResponse_httpStatus = Lens.lens (\AssociateSkillGroupWithRoomResponse' {httpStatus} -> httpStatus) (\s@AssociateSkillGroupWithRoomResponse' {} a -> s {httpStatus = a} :: AssociateSkillGroupWithRoomResponse)

instance
  Prelude.NFData
    AssociateSkillGroupWithRoomResponse
