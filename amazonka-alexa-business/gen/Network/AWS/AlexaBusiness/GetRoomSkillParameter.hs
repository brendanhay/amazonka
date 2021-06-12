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
-- Module      : Network.AWS.AlexaBusiness.GetRoomSkillParameter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets room skill parameter details by room, skill, and parameter key ARN.
module Network.AWS.AlexaBusiness.GetRoomSkillParameter
  ( -- * Creating a Request
    GetRoomSkillParameter (..),
    newGetRoomSkillParameter,

    -- * Request Lenses
    getRoomSkillParameter_roomArn,
    getRoomSkillParameter_skillId,
    getRoomSkillParameter_parameterKey,

    -- * Destructuring the Response
    GetRoomSkillParameterResponse (..),
    newGetRoomSkillParameterResponse,

    -- * Response Lenses
    getRoomSkillParameterResponse_roomSkillParameter,
    getRoomSkillParameterResponse_httpStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetRoomSkillParameter' smart constructor.
data GetRoomSkillParameter = GetRoomSkillParameter'
  { -- | The ARN of the room from which to get the room skill parameter details.
    roomArn :: Core.Maybe Core.Text,
    -- | The ARN of the skill from which to get the room skill parameter details.
    -- Required.
    skillId :: Core.Text,
    -- | The room skill parameter key for which to get details. Required.
    parameterKey :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetRoomSkillParameter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roomArn', 'getRoomSkillParameter_roomArn' - The ARN of the room from which to get the room skill parameter details.
--
-- 'skillId', 'getRoomSkillParameter_skillId' - The ARN of the skill from which to get the room skill parameter details.
-- Required.
--
-- 'parameterKey', 'getRoomSkillParameter_parameterKey' - The room skill parameter key for which to get details. Required.
newGetRoomSkillParameter ::
  -- | 'skillId'
  Core.Text ->
  -- | 'parameterKey'
  Core.Text ->
  GetRoomSkillParameter
newGetRoomSkillParameter pSkillId_ pParameterKey_ =
  GetRoomSkillParameter'
    { roomArn = Core.Nothing,
      skillId = pSkillId_,
      parameterKey = pParameterKey_
    }

-- | The ARN of the room from which to get the room skill parameter details.
getRoomSkillParameter_roomArn :: Lens.Lens' GetRoomSkillParameter (Core.Maybe Core.Text)
getRoomSkillParameter_roomArn = Lens.lens (\GetRoomSkillParameter' {roomArn} -> roomArn) (\s@GetRoomSkillParameter' {} a -> s {roomArn = a} :: GetRoomSkillParameter)

-- | The ARN of the skill from which to get the room skill parameter details.
-- Required.
getRoomSkillParameter_skillId :: Lens.Lens' GetRoomSkillParameter Core.Text
getRoomSkillParameter_skillId = Lens.lens (\GetRoomSkillParameter' {skillId} -> skillId) (\s@GetRoomSkillParameter' {} a -> s {skillId = a} :: GetRoomSkillParameter)

-- | The room skill parameter key for which to get details. Required.
getRoomSkillParameter_parameterKey :: Lens.Lens' GetRoomSkillParameter Core.Text
getRoomSkillParameter_parameterKey = Lens.lens (\GetRoomSkillParameter' {parameterKey} -> parameterKey) (\s@GetRoomSkillParameter' {} a -> s {parameterKey = a} :: GetRoomSkillParameter)

instance Core.AWSRequest GetRoomSkillParameter where
  type
    AWSResponse GetRoomSkillParameter =
      GetRoomSkillParameterResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRoomSkillParameterResponse'
            Core.<$> (x Core..?> "RoomSkillParameter")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetRoomSkillParameter

instance Core.NFData GetRoomSkillParameter

instance Core.ToHeaders GetRoomSkillParameter where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AlexaForBusiness.GetRoomSkillParameter" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetRoomSkillParameter where
  toJSON GetRoomSkillParameter' {..} =
    Core.object
      ( Core.catMaybes
          [ ("RoomArn" Core..=) Core.<$> roomArn,
            Core.Just ("SkillId" Core..= skillId),
            Core.Just ("ParameterKey" Core..= parameterKey)
          ]
      )

instance Core.ToPath GetRoomSkillParameter where
  toPath = Core.const "/"

instance Core.ToQuery GetRoomSkillParameter where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetRoomSkillParameterResponse' smart constructor.
data GetRoomSkillParameterResponse = GetRoomSkillParameterResponse'
  { -- | The details of the room skill parameter requested. Required.
    roomSkillParameter :: Core.Maybe RoomSkillParameter,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetRoomSkillParameterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roomSkillParameter', 'getRoomSkillParameterResponse_roomSkillParameter' - The details of the room skill parameter requested. Required.
--
-- 'httpStatus', 'getRoomSkillParameterResponse_httpStatus' - The response's http status code.
newGetRoomSkillParameterResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetRoomSkillParameterResponse
newGetRoomSkillParameterResponse pHttpStatus_ =
  GetRoomSkillParameterResponse'
    { roomSkillParameter =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The details of the room skill parameter requested. Required.
getRoomSkillParameterResponse_roomSkillParameter :: Lens.Lens' GetRoomSkillParameterResponse (Core.Maybe RoomSkillParameter)
getRoomSkillParameterResponse_roomSkillParameter = Lens.lens (\GetRoomSkillParameterResponse' {roomSkillParameter} -> roomSkillParameter) (\s@GetRoomSkillParameterResponse' {} a -> s {roomSkillParameter = a} :: GetRoomSkillParameterResponse)

-- | The response's http status code.
getRoomSkillParameterResponse_httpStatus :: Lens.Lens' GetRoomSkillParameterResponse Core.Int
getRoomSkillParameterResponse_httpStatus = Lens.lens (\GetRoomSkillParameterResponse' {httpStatus} -> httpStatus) (\s@GetRoomSkillParameterResponse' {} a -> s {httpStatus = a} :: GetRoomSkillParameterResponse)

instance Core.NFData GetRoomSkillParameterResponse
