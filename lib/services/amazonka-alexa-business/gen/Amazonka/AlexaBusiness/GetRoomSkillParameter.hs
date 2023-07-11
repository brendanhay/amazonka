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
-- Module      : Amazonka.AlexaBusiness.GetRoomSkillParameter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets room skill parameter details by room, skill, and parameter key ARN.
module Amazonka.AlexaBusiness.GetRoomSkillParameter
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

import Amazonka.AlexaBusiness.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetRoomSkillParameter' smart constructor.
data GetRoomSkillParameter = GetRoomSkillParameter'
  { -- | The ARN of the room from which to get the room skill parameter details.
    roomArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the skill from which to get the room skill parameter details.
    -- Required.
    skillId :: Prelude.Text,
    -- | The room skill parameter key for which to get details. Required.
    parameterKey :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'parameterKey'
  Prelude.Text ->
  GetRoomSkillParameter
newGetRoomSkillParameter pSkillId_ pParameterKey_ =
  GetRoomSkillParameter'
    { roomArn = Prelude.Nothing,
      skillId = pSkillId_,
      parameterKey = pParameterKey_
    }

-- | The ARN of the room from which to get the room skill parameter details.
getRoomSkillParameter_roomArn :: Lens.Lens' GetRoomSkillParameter (Prelude.Maybe Prelude.Text)
getRoomSkillParameter_roomArn = Lens.lens (\GetRoomSkillParameter' {roomArn} -> roomArn) (\s@GetRoomSkillParameter' {} a -> s {roomArn = a} :: GetRoomSkillParameter)

-- | The ARN of the skill from which to get the room skill parameter details.
-- Required.
getRoomSkillParameter_skillId :: Lens.Lens' GetRoomSkillParameter Prelude.Text
getRoomSkillParameter_skillId = Lens.lens (\GetRoomSkillParameter' {skillId} -> skillId) (\s@GetRoomSkillParameter' {} a -> s {skillId = a} :: GetRoomSkillParameter)

-- | The room skill parameter key for which to get details. Required.
getRoomSkillParameter_parameterKey :: Lens.Lens' GetRoomSkillParameter Prelude.Text
getRoomSkillParameter_parameterKey = Lens.lens (\GetRoomSkillParameter' {parameterKey} -> parameterKey) (\s@GetRoomSkillParameter' {} a -> s {parameterKey = a} :: GetRoomSkillParameter)

instance Core.AWSRequest GetRoomSkillParameter where
  type
    AWSResponse GetRoomSkillParameter =
      GetRoomSkillParameterResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRoomSkillParameterResponse'
            Prelude.<$> (x Data..?> "RoomSkillParameter")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetRoomSkillParameter where
  hashWithSalt _salt GetRoomSkillParameter' {..} =
    _salt
      `Prelude.hashWithSalt` roomArn
      `Prelude.hashWithSalt` skillId
      `Prelude.hashWithSalt` parameterKey

instance Prelude.NFData GetRoomSkillParameter where
  rnf GetRoomSkillParameter' {..} =
    Prelude.rnf roomArn
      `Prelude.seq` Prelude.rnf skillId
      `Prelude.seq` Prelude.rnf parameterKey

instance Data.ToHeaders GetRoomSkillParameter where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AlexaForBusiness.GetRoomSkillParameter" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetRoomSkillParameter where
  toJSON GetRoomSkillParameter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("RoomArn" Data..=) Prelude.<$> roomArn,
            Prelude.Just ("SkillId" Data..= skillId),
            Prelude.Just ("ParameterKey" Data..= parameterKey)
          ]
      )

instance Data.ToPath GetRoomSkillParameter where
  toPath = Prelude.const "/"

instance Data.ToQuery GetRoomSkillParameter where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetRoomSkillParameterResponse' smart constructor.
data GetRoomSkillParameterResponse = GetRoomSkillParameterResponse'
  { -- | The details of the room skill parameter requested. Required.
    roomSkillParameter :: Prelude.Maybe RoomSkillParameter,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  GetRoomSkillParameterResponse
newGetRoomSkillParameterResponse pHttpStatus_ =
  GetRoomSkillParameterResponse'
    { roomSkillParameter =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The details of the room skill parameter requested. Required.
getRoomSkillParameterResponse_roomSkillParameter :: Lens.Lens' GetRoomSkillParameterResponse (Prelude.Maybe RoomSkillParameter)
getRoomSkillParameterResponse_roomSkillParameter = Lens.lens (\GetRoomSkillParameterResponse' {roomSkillParameter} -> roomSkillParameter) (\s@GetRoomSkillParameterResponse' {} a -> s {roomSkillParameter = a} :: GetRoomSkillParameterResponse)

-- | The response's http status code.
getRoomSkillParameterResponse_httpStatus :: Lens.Lens' GetRoomSkillParameterResponse Prelude.Int
getRoomSkillParameterResponse_httpStatus = Lens.lens (\GetRoomSkillParameterResponse' {httpStatus} -> httpStatus) (\s@GetRoomSkillParameterResponse' {} a -> s {httpStatus = a} :: GetRoomSkillParameterResponse)

instance Prelude.NFData GetRoomSkillParameterResponse where
  rnf GetRoomSkillParameterResponse' {..} =
    Prelude.rnf roomSkillParameter
      `Prelude.seq` Prelude.rnf httpStatus
