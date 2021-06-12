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
-- Module      : Network.AWS.AlexaBusiness.PutRoomSkillParameter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates room skill parameter details by room, skill, and parameter key
-- ID. Not all skills have a room skill parameter.
module Network.AWS.AlexaBusiness.PutRoomSkillParameter
  ( -- * Creating a Request
    PutRoomSkillParameter (..),
    newPutRoomSkillParameter,

    -- * Request Lenses
    putRoomSkillParameter_roomArn,
    putRoomSkillParameter_skillId,
    putRoomSkillParameter_roomSkillParameter,

    -- * Destructuring the Response
    PutRoomSkillParameterResponse (..),
    newPutRoomSkillParameterResponse,

    -- * Response Lenses
    putRoomSkillParameterResponse_httpStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPutRoomSkillParameter' smart constructor.
data PutRoomSkillParameter = PutRoomSkillParameter'
  { -- | The ARN of the room associated with the room skill parameter. Required.
    roomArn :: Core.Maybe Core.Text,
    -- | The ARN of the skill associated with the room skill parameter. Required.
    skillId :: Core.Text,
    -- | The updated room skill parameter. Required.
    roomSkillParameter :: RoomSkillParameter
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutRoomSkillParameter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roomArn', 'putRoomSkillParameter_roomArn' - The ARN of the room associated with the room skill parameter. Required.
--
-- 'skillId', 'putRoomSkillParameter_skillId' - The ARN of the skill associated with the room skill parameter. Required.
--
-- 'roomSkillParameter', 'putRoomSkillParameter_roomSkillParameter' - The updated room skill parameter. Required.
newPutRoomSkillParameter ::
  -- | 'skillId'
  Core.Text ->
  -- | 'roomSkillParameter'
  RoomSkillParameter ->
  PutRoomSkillParameter
newPutRoomSkillParameter
  pSkillId_
  pRoomSkillParameter_ =
    PutRoomSkillParameter'
      { roomArn = Core.Nothing,
        skillId = pSkillId_,
        roomSkillParameter = pRoomSkillParameter_
      }

-- | The ARN of the room associated with the room skill parameter. Required.
putRoomSkillParameter_roomArn :: Lens.Lens' PutRoomSkillParameter (Core.Maybe Core.Text)
putRoomSkillParameter_roomArn = Lens.lens (\PutRoomSkillParameter' {roomArn} -> roomArn) (\s@PutRoomSkillParameter' {} a -> s {roomArn = a} :: PutRoomSkillParameter)

-- | The ARN of the skill associated with the room skill parameter. Required.
putRoomSkillParameter_skillId :: Lens.Lens' PutRoomSkillParameter Core.Text
putRoomSkillParameter_skillId = Lens.lens (\PutRoomSkillParameter' {skillId} -> skillId) (\s@PutRoomSkillParameter' {} a -> s {skillId = a} :: PutRoomSkillParameter)

-- | The updated room skill parameter. Required.
putRoomSkillParameter_roomSkillParameter :: Lens.Lens' PutRoomSkillParameter RoomSkillParameter
putRoomSkillParameter_roomSkillParameter = Lens.lens (\PutRoomSkillParameter' {roomSkillParameter} -> roomSkillParameter) (\s@PutRoomSkillParameter' {} a -> s {roomSkillParameter = a} :: PutRoomSkillParameter)

instance Core.AWSRequest PutRoomSkillParameter where
  type
    AWSResponse PutRoomSkillParameter =
      PutRoomSkillParameterResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutRoomSkillParameterResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable PutRoomSkillParameter

instance Core.NFData PutRoomSkillParameter

instance Core.ToHeaders PutRoomSkillParameter where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AlexaForBusiness.PutRoomSkillParameter" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON PutRoomSkillParameter where
  toJSON PutRoomSkillParameter' {..} =
    Core.object
      ( Core.catMaybes
          [ ("RoomArn" Core..=) Core.<$> roomArn,
            Core.Just ("SkillId" Core..= skillId),
            Core.Just
              ("RoomSkillParameter" Core..= roomSkillParameter)
          ]
      )

instance Core.ToPath PutRoomSkillParameter where
  toPath = Core.const "/"

instance Core.ToQuery PutRoomSkillParameter where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newPutRoomSkillParameterResponse' smart constructor.
data PutRoomSkillParameterResponse = PutRoomSkillParameterResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutRoomSkillParameterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putRoomSkillParameterResponse_httpStatus' - The response's http status code.
newPutRoomSkillParameterResponse ::
  -- | 'httpStatus'
  Core.Int ->
  PutRoomSkillParameterResponse
newPutRoomSkillParameterResponse pHttpStatus_ =
  PutRoomSkillParameterResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
putRoomSkillParameterResponse_httpStatus :: Lens.Lens' PutRoomSkillParameterResponse Core.Int
putRoomSkillParameterResponse_httpStatus = Lens.lens (\PutRoomSkillParameterResponse' {httpStatus} -> httpStatus) (\s@PutRoomSkillParameterResponse' {} a -> s {httpStatus = a} :: PutRoomSkillParameterResponse)

instance Core.NFData PutRoomSkillParameterResponse
