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
-- Module      : Amazonka.AlexaBusiness.DeleteRoomSkillParameter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes room skill parameter details by room, skill, and parameter key
-- ID.
module Amazonka.AlexaBusiness.DeleteRoomSkillParameter
  ( -- * Creating a Request
    DeleteRoomSkillParameter (..),
    newDeleteRoomSkillParameter,

    -- * Request Lenses
    deleteRoomSkillParameter_roomArn,
    deleteRoomSkillParameter_skillId,
    deleteRoomSkillParameter_parameterKey,

    -- * Destructuring the Response
    DeleteRoomSkillParameterResponse (..),
    newDeleteRoomSkillParameterResponse,

    -- * Response Lenses
    deleteRoomSkillParameterResponse_httpStatus,
  )
where

import Amazonka.AlexaBusiness.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteRoomSkillParameter' smart constructor.
data DeleteRoomSkillParameter = DeleteRoomSkillParameter'
  { -- | The ARN of the room from which to remove the room skill parameter
    -- details.
    roomArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the skill from which to remove the room skill parameter
    -- details.
    skillId :: Prelude.Text,
    -- | The room skill parameter key for which to remove details.
    parameterKey :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRoomSkillParameter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roomArn', 'deleteRoomSkillParameter_roomArn' - The ARN of the room from which to remove the room skill parameter
-- details.
--
-- 'skillId', 'deleteRoomSkillParameter_skillId' - The ID of the skill from which to remove the room skill parameter
-- details.
--
-- 'parameterKey', 'deleteRoomSkillParameter_parameterKey' - The room skill parameter key for which to remove details.
newDeleteRoomSkillParameter ::
  -- | 'skillId'
  Prelude.Text ->
  -- | 'parameterKey'
  Prelude.Text ->
  DeleteRoomSkillParameter
newDeleteRoomSkillParameter pSkillId_ pParameterKey_ =
  DeleteRoomSkillParameter'
    { roomArn =
        Prelude.Nothing,
      skillId = pSkillId_,
      parameterKey = pParameterKey_
    }

-- | The ARN of the room from which to remove the room skill parameter
-- details.
deleteRoomSkillParameter_roomArn :: Lens.Lens' DeleteRoomSkillParameter (Prelude.Maybe Prelude.Text)
deleteRoomSkillParameter_roomArn = Lens.lens (\DeleteRoomSkillParameter' {roomArn} -> roomArn) (\s@DeleteRoomSkillParameter' {} a -> s {roomArn = a} :: DeleteRoomSkillParameter)

-- | The ID of the skill from which to remove the room skill parameter
-- details.
deleteRoomSkillParameter_skillId :: Lens.Lens' DeleteRoomSkillParameter Prelude.Text
deleteRoomSkillParameter_skillId = Lens.lens (\DeleteRoomSkillParameter' {skillId} -> skillId) (\s@DeleteRoomSkillParameter' {} a -> s {skillId = a} :: DeleteRoomSkillParameter)

-- | The room skill parameter key for which to remove details.
deleteRoomSkillParameter_parameterKey :: Lens.Lens' DeleteRoomSkillParameter Prelude.Text
deleteRoomSkillParameter_parameterKey = Lens.lens (\DeleteRoomSkillParameter' {parameterKey} -> parameterKey) (\s@DeleteRoomSkillParameter' {} a -> s {parameterKey = a} :: DeleteRoomSkillParameter)

instance Core.AWSRequest DeleteRoomSkillParameter where
  type
    AWSResponse DeleteRoomSkillParameter =
      DeleteRoomSkillParameterResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteRoomSkillParameterResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteRoomSkillParameter where
  hashWithSalt _salt DeleteRoomSkillParameter' {..} =
    _salt `Prelude.hashWithSalt` roomArn
      `Prelude.hashWithSalt` skillId
      `Prelude.hashWithSalt` parameterKey

instance Prelude.NFData DeleteRoomSkillParameter where
  rnf DeleteRoomSkillParameter' {..} =
    Prelude.rnf roomArn
      `Prelude.seq` Prelude.rnf skillId
      `Prelude.seq` Prelude.rnf parameterKey

instance Data.ToHeaders DeleteRoomSkillParameter where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AlexaForBusiness.DeleteRoomSkillParameter" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteRoomSkillParameter where
  toJSON DeleteRoomSkillParameter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("RoomArn" Data..=) Prelude.<$> roomArn,
            Prelude.Just ("SkillId" Data..= skillId),
            Prelude.Just ("ParameterKey" Data..= parameterKey)
          ]
      )

instance Data.ToPath DeleteRoomSkillParameter where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteRoomSkillParameter where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteRoomSkillParameterResponse' smart constructor.
data DeleteRoomSkillParameterResponse = DeleteRoomSkillParameterResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRoomSkillParameterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteRoomSkillParameterResponse_httpStatus' - The response's http status code.
newDeleteRoomSkillParameterResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteRoomSkillParameterResponse
newDeleteRoomSkillParameterResponse pHttpStatus_ =
  DeleteRoomSkillParameterResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteRoomSkillParameterResponse_httpStatus :: Lens.Lens' DeleteRoomSkillParameterResponse Prelude.Int
deleteRoomSkillParameterResponse_httpStatus = Lens.lens (\DeleteRoomSkillParameterResponse' {httpStatus} -> httpStatus) (\s@DeleteRoomSkillParameterResponse' {} a -> s {httpStatus = a} :: DeleteRoomSkillParameterResponse)

instance
  Prelude.NFData
    DeleteRoomSkillParameterResponse
  where
  rnf DeleteRoomSkillParameterResponse' {..} =
    Prelude.rnf httpStatus
