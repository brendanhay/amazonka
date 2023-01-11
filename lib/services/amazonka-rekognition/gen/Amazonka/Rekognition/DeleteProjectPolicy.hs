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
-- Module      : Amazonka.Rekognition.DeleteProjectPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing project policy.
--
-- To get a list of project policies attached to a project, call
-- ListProjectPolicies. To attach a project policy to a project, call
-- PutProjectPolicy.
module Amazonka.Rekognition.DeleteProjectPolicy
  ( -- * Creating a Request
    DeleteProjectPolicy (..),
    newDeleteProjectPolicy,

    -- * Request Lenses
    deleteProjectPolicy_policyRevisionId,
    deleteProjectPolicy_projectArn,
    deleteProjectPolicy_policyName,

    -- * Destructuring the Response
    DeleteProjectPolicyResponse (..),
    newDeleteProjectPolicyResponse,

    -- * Response Lenses
    deleteProjectPolicyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteProjectPolicy' smart constructor.
data DeleteProjectPolicy = DeleteProjectPolicy'
  { -- | The ID of the project policy revision that you want to delete.
    policyRevisionId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the project that the project policy
    -- you want to delete is attached to.
    projectArn :: Prelude.Text,
    -- | The name of the policy that you want to delete.
    policyName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteProjectPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyRevisionId', 'deleteProjectPolicy_policyRevisionId' - The ID of the project policy revision that you want to delete.
--
-- 'projectArn', 'deleteProjectPolicy_projectArn' - The Amazon Resource Name (ARN) of the project that the project policy
-- you want to delete is attached to.
--
-- 'policyName', 'deleteProjectPolicy_policyName' - The name of the policy that you want to delete.
newDeleteProjectPolicy ::
  -- | 'projectArn'
  Prelude.Text ->
  -- | 'policyName'
  Prelude.Text ->
  DeleteProjectPolicy
newDeleteProjectPolicy pProjectArn_ pPolicyName_ =
  DeleteProjectPolicy'
    { policyRevisionId =
        Prelude.Nothing,
      projectArn = pProjectArn_,
      policyName = pPolicyName_
    }

-- | The ID of the project policy revision that you want to delete.
deleteProjectPolicy_policyRevisionId :: Lens.Lens' DeleteProjectPolicy (Prelude.Maybe Prelude.Text)
deleteProjectPolicy_policyRevisionId = Lens.lens (\DeleteProjectPolicy' {policyRevisionId} -> policyRevisionId) (\s@DeleteProjectPolicy' {} a -> s {policyRevisionId = a} :: DeleteProjectPolicy)

-- | The Amazon Resource Name (ARN) of the project that the project policy
-- you want to delete is attached to.
deleteProjectPolicy_projectArn :: Lens.Lens' DeleteProjectPolicy Prelude.Text
deleteProjectPolicy_projectArn = Lens.lens (\DeleteProjectPolicy' {projectArn} -> projectArn) (\s@DeleteProjectPolicy' {} a -> s {projectArn = a} :: DeleteProjectPolicy)

-- | The name of the policy that you want to delete.
deleteProjectPolicy_policyName :: Lens.Lens' DeleteProjectPolicy Prelude.Text
deleteProjectPolicy_policyName = Lens.lens (\DeleteProjectPolicy' {policyName} -> policyName) (\s@DeleteProjectPolicy' {} a -> s {policyName = a} :: DeleteProjectPolicy)

instance Core.AWSRequest DeleteProjectPolicy where
  type
    AWSResponse DeleteProjectPolicy =
      DeleteProjectPolicyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteProjectPolicyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteProjectPolicy where
  hashWithSalt _salt DeleteProjectPolicy' {..} =
    _salt `Prelude.hashWithSalt` policyRevisionId
      `Prelude.hashWithSalt` projectArn
      `Prelude.hashWithSalt` policyName

instance Prelude.NFData DeleteProjectPolicy where
  rnf DeleteProjectPolicy' {..} =
    Prelude.rnf policyRevisionId
      `Prelude.seq` Prelude.rnf projectArn
      `Prelude.seq` Prelude.rnf policyName

instance Data.ToHeaders DeleteProjectPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "RekognitionService.DeleteProjectPolicy" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteProjectPolicy where
  toJSON DeleteProjectPolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("PolicyRevisionId" Data..=)
              Prelude.<$> policyRevisionId,
            Prelude.Just ("ProjectArn" Data..= projectArn),
            Prelude.Just ("PolicyName" Data..= policyName)
          ]
      )

instance Data.ToPath DeleteProjectPolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteProjectPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteProjectPolicyResponse' smart constructor.
data DeleteProjectPolicyResponse = DeleteProjectPolicyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteProjectPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteProjectPolicyResponse_httpStatus' - The response's http status code.
newDeleteProjectPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteProjectPolicyResponse
newDeleteProjectPolicyResponse pHttpStatus_ =
  DeleteProjectPolicyResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteProjectPolicyResponse_httpStatus :: Lens.Lens' DeleteProjectPolicyResponse Prelude.Int
deleteProjectPolicyResponse_httpStatus = Lens.lens (\DeleteProjectPolicyResponse' {httpStatus} -> httpStatus) (\s@DeleteProjectPolicyResponse' {} a -> s {httpStatus = a} :: DeleteProjectPolicyResponse)

instance Prelude.NFData DeleteProjectPolicyResponse where
  rnf DeleteProjectPolicyResponse' {..} =
    Prelude.rnf httpStatus
