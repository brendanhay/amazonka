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
-- Module      : Network.AWS.AlexaBusiness.DeleteSkillAuthorization
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Unlinks a third-party account from a skill.
module Network.AWS.AlexaBusiness.DeleteSkillAuthorization
  ( -- * Creating a Request
    DeleteSkillAuthorization (..),
    newDeleteSkillAuthorization,

    -- * Request Lenses
    deleteSkillAuthorization_roomArn,
    deleteSkillAuthorization_skillId,

    -- * Destructuring the Response
    DeleteSkillAuthorizationResponse (..),
    newDeleteSkillAuthorizationResponse,

    -- * Response Lenses
    deleteSkillAuthorizationResponse_httpStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteSkillAuthorization' smart constructor.
data DeleteSkillAuthorization = DeleteSkillAuthorization'
  { -- | The room that the skill is authorized for.
    roomArn :: Core.Maybe Core.Text,
    -- | The unique identifier of a skill.
    skillId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteSkillAuthorization' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roomArn', 'deleteSkillAuthorization_roomArn' - The room that the skill is authorized for.
--
-- 'skillId', 'deleteSkillAuthorization_skillId' - The unique identifier of a skill.
newDeleteSkillAuthorization ::
  -- | 'skillId'
  Core.Text ->
  DeleteSkillAuthorization
newDeleteSkillAuthorization pSkillId_ =
  DeleteSkillAuthorization'
    { roomArn = Core.Nothing,
      skillId = pSkillId_
    }

-- | The room that the skill is authorized for.
deleteSkillAuthorization_roomArn :: Lens.Lens' DeleteSkillAuthorization (Core.Maybe Core.Text)
deleteSkillAuthorization_roomArn = Lens.lens (\DeleteSkillAuthorization' {roomArn} -> roomArn) (\s@DeleteSkillAuthorization' {} a -> s {roomArn = a} :: DeleteSkillAuthorization)

-- | The unique identifier of a skill.
deleteSkillAuthorization_skillId :: Lens.Lens' DeleteSkillAuthorization Core.Text
deleteSkillAuthorization_skillId = Lens.lens (\DeleteSkillAuthorization' {skillId} -> skillId) (\s@DeleteSkillAuthorization' {} a -> s {skillId = a} :: DeleteSkillAuthorization)

instance Core.AWSRequest DeleteSkillAuthorization where
  type
    AWSResponse DeleteSkillAuthorization =
      DeleteSkillAuthorizationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteSkillAuthorizationResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteSkillAuthorization

instance Core.NFData DeleteSkillAuthorization

instance Core.ToHeaders DeleteSkillAuthorization where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AlexaForBusiness.DeleteSkillAuthorization" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteSkillAuthorization where
  toJSON DeleteSkillAuthorization' {..} =
    Core.object
      ( Core.catMaybes
          [ ("RoomArn" Core..=) Core.<$> roomArn,
            Core.Just ("SkillId" Core..= skillId)
          ]
      )

instance Core.ToPath DeleteSkillAuthorization where
  toPath = Core.const "/"

instance Core.ToQuery DeleteSkillAuthorization where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteSkillAuthorizationResponse' smart constructor.
data DeleteSkillAuthorizationResponse = DeleteSkillAuthorizationResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteSkillAuthorizationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteSkillAuthorizationResponse_httpStatus' - The response's http status code.
newDeleteSkillAuthorizationResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteSkillAuthorizationResponse
newDeleteSkillAuthorizationResponse pHttpStatus_ =
  DeleteSkillAuthorizationResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteSkillAuthorizationResponse_httpStatus :: Lens.Lens' DeleteSkillAuthorizationResponse Core.Int
deleteSkillAuthorizationResponse_httpStatus = Lens.lens (\DeleteSkillAuthorizationResponse' {httpStatus} -> httpStatus) (\s@DeleteSkillAuthorizationResponse' {} a -> s {httpStatus = a} :: DeleteSkillAuthorizationResponse)

instance Core.NFData DeleteSkillAuthorizationResponse
