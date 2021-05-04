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
-- Module      : Network.AWS.ECR.DeleteRepositoryPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the repository policy associated with the specified repository.
module Network.AWS.ECR.DeleteRepositoryPolicy
  ( -- * Creating a Request
    DeleteRepositoryPolicy (..),
    newDeleteRepositoryPolicy,

    -- * Request Lenses
    deleteRepositoryPolicy_registryId,
    deleteRepositoryPolicy_repositoryName,

    -- * Destructuring the Response
    DeleteRepositoryPolicyResponse (..),
    newDeleteRepositoryPolicyResponse,

    -- * Response Lenses
    deleteRepositoryPolicyResponse_registryId,
    deleteRepositoryPolicyResponse_policyText,
    deleteRepositoryPolicyResponse_repositoryName,
    deleteRepositoryPolicyResponse_httpStatus,
  )
where

import Network.AWS.ECR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteRepositoryPolicy' smart constructor.
data DeleteRepositoryPolicy = DeleteRepositoryPolicy'
  { -- | The AWS account ID associated with the registry that contains the
    -- repository policy to delete. If you do not specify a registry, the
    -- default registry is assumed.
    registryId :: Prelude.Maybe Prelude.Text,
    -- | The name of the repository that is associated with the repository policy
    -- to delete.
    repositoryName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteRepositoryPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'registryId', 'deleteRepositoryPolicy_registryId' - The AWS account ID associated with the registry that contains the
-- repository policy to delete. If you do not specify a registry, the
-- default registry is assumed.
--
-- 'repositoryName', 'deleteRepositoryPolicy_repositoryName' - The name of the repository that is associated with the repository policy
-- to delete.
newDeleteRepositoryPolicy ::
  -- | 'repositoryName'
  Prelude.Text ->
  DeleteRepositoryPolicy
newDeleteRepositoryPolicy pRepositoryName_ =
  DeleteRepositoryPolicy'
    { registryId =
        Prelude.Nothing,
      repositoryName = pRepositoryName_
    }

-- | The AWS account ID associated with the registry that contains the
-- repository policy to delete. If you do not specify a registry, the
-- default registry is assumed.
deleteRepositoryPolicy_registryId :: Lens.Lens' DeleteRepositoryPolicy (Prelude.Maybe Prelude.Text)
deleteRepositoryPolicy_registryId = Lens.lens (\DeleteRepositoryPolicy' {registryId} -> registryId) (\s@DeleteRepositoryPolicy' {} a -> s {registryId = a} :: DeleteRepositoryPolicy)

-- | The name of the repository that is associated with the repository policy
-- to delete.
deleteRepositoryPolicy_repositoryName :: Lens.Lens' DeleteRepositoryPolicy Prelude.Text
deleteRepositoryPolicy_repositoryName = Lens.lens (\DeleteRepositoryPolicy' {repositoryName} -> repositoryName) (\s@DeleteRepositoryPolicy' {} a -> s {repositoryName = a} :: DeleteRepositoryPolicy)

instance Prelude.AWSRequest DeleteRepositoryPolicy where
  type
    Rs DeleteRepositoryPolicy =
      DeleteRepositoryPolicyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteRepositoryPolicyResponse'
            Prelude.<$> (x Prelude..?> "registryId")
            Prelude.<*> (x Prelude..?> "policyText")
            Prelude.<*> (x Prelude..?> "repositoryName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteRepositoryPolicy

instance Prelude.NFData DeleteRepositoryPolicy

instance Prelude.ToHeaders DeleteRepositoryPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AmazonEC2ContainerRegistry_V20150921.DeleteRepositoryPolicy" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteRepositoryPolicy where
  toJSON DeleteRepositoryPolicy' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("registryId" Prelude..=) Prelude.<$> registryId,
            Prelude.Just
              ("repositoryName" Prelude..= repositoryName)
          ]
      )

instance Prelude.ToPath DeleteRepositoryPolicy where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteRepositoryPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteRepositoryPolicyResponse' smart constructor.
data DeleteRepositoryPolicyResponse = DeleteRepositoryPolicyResponse'
  { -- | The registry ID associated with the request.
    registryId :: Prelude.Maybe Prelude.Text,
    -- | The JSON repository policy that was deleted from the repository.
    policyText :: Prelude.Maybe Prelude.Text,
    -- | The repository name associated with the request.
    repositoryName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteRepositoryPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'registryId', 'deleteRepositoryPolicyResponse_registryId' - The registry ID associated with the request.
--
-- 'policyText', 'deleteRepositoryPolicyResponse_policyText' - The JSON repository policy that was deleted from the repository.
--
-- 'repositoryName', 'deleteRepositoryPolicyResponse_repositoryName' - The repository name associated with the request.
--
-- 'httpStatus', 'deleteRepositoryPolicyResponse_httpStatus' - The response's http status code.
newDeleteRepositoryPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteRepositoryPolicyResponse
newDeleteRepositoryPolicyResponse pHttpStatus_ =
  DeleteRepositoryPolicyResponse'
    { registryId =
        Prelude.Nothing,
      policyText = Prelude.Nothing,
      repositoryName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The registry ID associated with the request.
deleteRepositoryPolicyResponse_registryId :: Lens.Lens' DeleteRepositoryPolicyResponse (Prelude.Maybe Prelude.Text)
deleteRepositoryPolicyResponse_registryId = Lens.lens (\DeleteRepositoryPolicyResponse' {registryId} -> registryId) (\s@DeleteRepositoryPolicyResponse' {} a -> s {registryId = a} :: DeleteRepositoryPolicyResponse)

-- | The JSON repository policy that was deleted from the repository.
deleteRepositoryPolicyResponse_policyText :: Lens.Lens' DeleteRepositoryPolicyResponse (Prelude.Maybe Prelude.Text)
deleteRepositoryPolicyResponse_policyText = Lens.lens (\DeleteRepositoryPolicyResponse' {policyText} -> policyText) (\s@DeleteRepositoryPolicyResponse' {} a -> s {policyText = a} :: DeleteRepositoryPolicyResponse)

-- | The repository name associated with the request.
deleteRepositoryPolicyResponse_repositoryName :: Lens.Lens' DeleteRepositoryPolicyResponse (Prelude.Maybe Prelude.Text)
deleteRepositoryPolicyResponse_repositoryName = Lens.lens (\DeleteRepositoryPolicyResponse' {repositoryName} -> repositoryName) (\s@DeleteRepositoryPolicyResponse' {} a -> s {repositoryName = a} :: DeleteRepositoryPolicyResponse)

-- | The response's http status code.
deleteRepositoryPolicyResponse_httpStatus :: Lens.Lens' DeleteRepositoryPolicyResponse Prelude.Int
deleteRepositoryPolicyResponse_httpStatus = Lens.lens (\DeleteRepositoryPolicyResponse' {httpStatus} -> httpStatus) (\s@DeleteRepositoryPolicyResponse' {} a -> s {httpStatus = a} :: DeleteRepositoryPolicyResponse)

instance
  Prelude.NFData
    DeleteRepositoryPolicyResponse
