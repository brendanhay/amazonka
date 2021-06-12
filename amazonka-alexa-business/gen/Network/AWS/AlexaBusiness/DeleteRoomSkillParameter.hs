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
-- Module      : Network.AWS.AlexaBusiness.DeleteRoomSkillParameter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes room skill parameter details by room, skill, and parameter key
-- ID.
module Network.AWS.AlexaBusiness.DeleteRoomSkillParameter
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

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteRoomSkillParameter' smart constructor.
data DeleteRoomSkillParameter = DeleteRoomSkillParameter'
  { -- | The ARN of the room from which to remove the room skill parameter
    -- details.
    roomArn :: Core.Maybe Core.Text,
    -- | The ID of the skill from which to remove the room skill parameter
    -- details.
    skillId :: Core.Text,
    -- | The room skill parameter key for which to remove details.
    parameterKey :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'parameterKey'
  Core.Text ->
  DeleteRoomSkillParameter
newDeleteRoomSkillParameter pSkillId_ pParameterKey_ =
  DeleteRoomSkillParameter'
    { roomArn = Core.Nothing,
      skillId = pSkillId_,
      parameterKey = pParameterKey_
    }

-- | The ARN of the room from which to remove the room skill parameter
-- details.
deleteRoomSkillParameter_roomArn :: Lens.Lens' DeleteRoomSkillParameter (Core.Maybe Core.Text)
deleteRoomSkillParameter_roomArn = Lens.lens (\DeleteRoomSkillParameter' {roomArn} -> roomArn) (\s@DeleteRoomSkillParameter' {} a -> s {roomArn = a} :: DeleteRoomSkillParameter)

-- | The ID of the skill from which to remove the room skill parameter
-- details.
deleteRoomSkillParameter_skillId :: Lens.Lens' DeleteRoomSkillParameter Core.Text
deleteRoomSkillParameter_skillId = Lens.lens (\DeleteRoomSkillParameter' {skillId} -> skillId) (\s@DeleteRoomSkillParameter' {} a -> s {skillId = a} :: DeleteRoomSkillParameter)

-- | The room skill parameter key for which to remove details.
deleteRoomSkillParameter_parameterKey :: Lens.Lens' DeleteRoomSkillParameter Core.Text
deleteRoomSkillParameter_parameterKey = Lens.lens (\DeleteRoomSkillParameter' {parameterKey} -> parameterKey) (\s@DeleteRoomSkillParameter' {} a -> s {parameterKey = a} :: DeleteRoomSkillParameter)

instance Core.AWSRequest DeleteRoomSkillParameter where
  type
    AWSResponse DeleteRoomSkillParameter =
      DeleteRoomSkillParameterResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteRoomSkillParameterResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteRoomSkillParameter

instance Core.NFData DeleteRoomSkillParameter

instance Core.ToHeaders DeleteRoomSkillParameter where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AlexaForBusiness.DeleteRoomSkillParameter" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteRoomSkillParameter where
  toJSON DeleteRoomSkillParameter' {..} =
    Core.object
      ( Core.catMaybes
          [ ("RoomArn" Core..=) Core.<$> roomArn,
            Core.Just ("SkillId" Core..= skillId),
            Core.Just ("ParameterKey" Core..= parameterKey)
          ]
      )

instance Core.ToPath DeleteRoomSkillParameter where
  toPath = Core.const "/"

instance Core.ToQuery DeleteRoomSkillParameter where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteRoomSkillParameterResponse' smart constructor.
data DeleteRoomSkillParameterResponse = DeleteRoomSkillParameterResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DeleteRoomSkillParameterResponse
newDeleteRoomSkillParameterResponse pHttpStatus_ =
  DeleteRoomSkillParameterResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteRoomSkillParameterResponse_httpStatus :: Lens.Lens' DeleteRoomSkillParameterResponse Core.Int
deleteRoomSkillParameterResponse_httpStatus = Lens.lens (\DeleteRoomSkillParameterResponse' {httpStatus} -> httpStatus) (\s@DeleteRoomSkillParameterResponse' {} a -> s {httpStatus = a} :: DeleteRoomSkillParameterResponse)

instance Core.NFData DeleteRoomSkillParameterResponse
