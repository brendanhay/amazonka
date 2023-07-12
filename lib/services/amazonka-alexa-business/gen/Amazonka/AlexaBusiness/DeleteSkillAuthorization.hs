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
-- Module      : Amazonka.AlexaBusiness.DeleteSkillAuthorization
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Unlinks a third-party account from a skill.
module Amazonka.AlexaBusiness.DeleteSkillAuthorization
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

import Amazonka.AlexaBusiness.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteSkillAuthorization' smart constructor.
data DeleteSkillAuthorization = DeleteSkillAuthorization'
  { -- | The room that the skill is authorized for.
    roomArn :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of a skill.
    skillId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DeleteSkillAuthorization
newDeleteSkillAuthorization pSkillId_ =
  DeleteSkillAuthorization'
    { roomArn =
        Prelude.Nothing,
      skillId = pSkillId_
    }

-- | The room that the skill is authorized for.
deleteSkillAuthorization_roomArn :: Lens.Lens' DeleteSkillAuthorization (Prelude.Maybe Prelude.Text)
deleteSkillAuthorization_roomArn = Lens.lens (\DeleteSkillAuthorization' {roomArn} -> roomArn) (\s@DeleteSkillAuthorization' {} a -> s {roomArn = a} :: DeleteSkillAuthorization)

-- | The unique identifier of a skill.
deleteSkillAuthorization_skillId :: Lens.Lens' DeleteSkillAuthorization Prelude.Text
deleteSkillAuthorization_skillId = Lens.lens (\DeleteSkillAuthorization' {skillId} -> skillId) (\s@DeleteSkillAuthorization' {} a -> s {skillId = a} :: DeleteSkillAuthorization)

instance Core.AWSRequest DeleteSkillAuthorization where
  type
    AWSResponse DeleteSkillAuthorization =
      DeleteSkillAuthorizationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteSkillAuthorizationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteSkillAuthorization where
  hashWithSalt _salt DeleteSkillAuthorization' {..} =
    _salt
      `Prelude.hashWithSalt` roomArn
      `Prelude.hashWithSalt` skillId

instance Prelude.NFData DeleteSkillAuthorization where
  rnf DeleteSkillAuthorization' {..} =
    Prelude.rnf roomArn
      `Prelude.seq` Prelude.rnf skillId

instance Data.ToHeaders DeleteSkillAuthorization where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AlexaForBusiness.DeleteSkillAuthorization" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteSkillAuthorization where
  toJSON DeleteSkillAuthorization' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("RoomArn" Data..=) Prelude.<$> roomArn,
            Prelude.Just ("SkillId" Data..= skillId)
          ]
      )

instance Data.ToPath DeleteSkillAuthorization where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteSkillAuthorization where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteSkillAuthorizationResponse' smart constructor.
data DeleteSkillAuthorizationResponse = DeleteSkillAuthorizationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DeleteSkillAuthorizationResponse
newDeleteSkillAuthorizationResponse pHttpStatus_ =
  DeleteSkillAuthorizationResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteSkillAuthorizationResponse_httpStatus :: Lens.Lens' DeleteSkillAuthorizationResponse Prelude.Int
deleteSkillAuthorizationResponse_httpStatus = Lens.lens (\DeleteSkillAuthorizationResponse' {httpStatus} -> httpStatus) (\s@DeleteSkillAuthorizationResponse' {} a -> s {httpStatus = a} :: DeleteSkillAuthorizationResponse)

instance
  Prelude.NFData
    DeleteSkillAuthorizationResponse
  where
  rnf DeleteSkillAuthorizationResponse' {..} =
    Prelude.rnf httpStatus
