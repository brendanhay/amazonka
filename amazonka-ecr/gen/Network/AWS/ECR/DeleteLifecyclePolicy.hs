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
-- Module      : Network.AWS.ECR.DeleteLifecyclePolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the lifecycle policy associated with the specified repository.
module Network.AWS.ECR.DeleteLifecyclePolicy
  ( -- * Creating a Request
    DeleteLifecyclePolicy (..),
    newDeleteLifecyclePolicy,

    -- * Request Lenses
    deleteLifecyclePolicy_registryId,
    deleteLifecyclePolicy_repositoryName,

    -- * Destructuring the Response
    DeleteLifecyclePolicyResponse (..),
    newDeleteLifecyclePolicyResponse,

    -- * Response Lenses
    deleteLifecyclePolicyResponse_registryId,
    deleteLifecyclePolicyResponse_repositoryName,
    deleteLifecyclePolicyResponse_lifecyclePolicyText,
    deleteLifecyclePolicyResponse_lastEvaluatedAt,
    deleteLifecyclePolicyResponse_httpStatus,
  )
where

import Network.AWS.ECR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteLifecyclePolicy' smart constructor.
data DeleteLifecyclePolicy = DeleteLifecyclePolicy'
  { -- | The AWS account ID associated with the registry that contains the
    -- repository. If you do not specify a registry, the default registry is
    -- assumed.
    registryId :: Prelude.Maybe Prelude.Text,
    -- | The name of the repository.
    repositoryName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteLifecyclePolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'registryId', 'deleteLifecyclePolicy_registryId' - The AWS account ID associated with the registry that contains the
-- repository. If you do not specify a registry, the default registry is
-- assumed.
--
-- 'repositoryName', 'deleteLifecyclePolicy_repositoryName' - The name of the repository.
newDeleteLifecyclePolicy ::
  -- | 'repositoryName'
  Prelude.Text ->
  DeleteLifecyclePolicy
newDeleteLifecyclePolicy pRepositoryName_ =
  DeleteLifecyclePolicy'
    { registryId =
        Prelude.Nothing,
      repositoryName = pRepositoryName_
    }

-- | The AWS account ID associated with the registry that contains the
-- repository. If you do not specify a registry, the default registry is
-- assumed.
deleteLifecyclePolicy_registryId :: Lens.Lens' DeleteLifecyclePolicy (Prelude.Maybe Prelude.Text)
deleteLifecyclePolicy_registryId = Lens.lens (\DeleteLifecyclePolicy' {registryId} -> registryId) (\s@DeleteLifecyclePolicy' {} a -> s {registryId = a} :: DeleteLifecyclePolicy)

-- | The name of the repository.
deleteLifecyclePolicy_repositoryName :: Lens.Lens' DeleteLifecyclePolicy Prelude.Text
deleteLifecyclePolicy_repositoryName = Lens.lens (\DeleteLifecyclePolicy' {repositoryName} -> repositoryName) (\s@DeleteLifecyclePolicy' {} a -> s {repositoryName = a} :: DeleteLifecyclePolicy)

instance Prelude.AWSRequest DeleteLifecyclePolicy where
  type
    Rs DeleteLifecyclePolicy =
      DeleteLifecyclePolicyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteLifecyclePolicyResponse'
            Prelude.<$> (x Prelude..?> "registryId")
            Prelude.<*> (x Prelude..?> "repositoryName")
            Prelude.<*> (x Prelude..?> "lifecyclePolicyText")
            Prelude.<*> (x Prelude..?> "lastEvaluatedAt")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteLifecyclePolicy

instance Prelude.NFData DeleteLifecyclePolicy

instance Prelude.ToHeaders DeleteLifecyclePolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AmazonEC2ContainerRegistry_V20150921.DeleteLifecyclePolicy" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteLifecyclePolicy where
  toJSON DeleteLifecyclePolicy' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("registryId" Prelude..=) Prelude.<$> registryId,
            Prelude.Just
              ("repositoryName" Prelude..= repositoryName)
          ]
      )

instance Prelude.ToPath DeleteLifecyclePolicy where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteLifecyclePolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteLifecyclePolicyResponse' smart constructor.
data DeleteLifecyclePolicyResponse = DeleteLifecyclePolicyResponse'
  { -- | The registry ID associated with the request.
    registryId :: Prelude.Maybe Prelude.Text,
    -- | The repository name associated with the request.
    repositoryName :: Prelude.Maybe Prelude.Text,
    -- | The JSON lifecycle policy text.
    lifecyclePolicyText :: Prelude.Maybe Prelude.Text,
    -- | The time stamp of the last time that the lifecycle policy was run.
    lastEvaluatedAt :: Prelude.Maybe Prelude.POSIX,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteLifecyclePolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'registryId', 'deleteLifecyclePolicyResponse_registryId' - The registry ID associated with the request.
--
-- 'repositoryName', 'deleteLifecyclePolicyResponse_repositoryName' - The repository name associated with the request.
--
-- 'lifecyclePolicyText', 'deleteLifecyclePolicyResponse_lifecyclePolicyText' - The JSON lifecycle policy text.
--
-- 'lastEvaluatedAt', 'deleteLifecyclePolicyResponse_lastEvaluatedAt' - The time stamp of the last time that the lifecycle policy was run.
--
-- 'httpStatus', 'deleteLifecyclePolicyResponse_httpStatus' - The response's http status code.
newDeleteLifecyclePolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteLifecyclePolicyResponse
newDeleteLifecyclePolicyResponse pHttpStatus_ =
  DeleteLifecyclePolicyResponse'
    { registryId =
        Prelude.Nothing,
      repositoryName = Prelude.Nothing,
      lifecyclePolicyText = Prelude.Nothing,
      lastEvaluatedAt = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The registry ID associated with the request.
deleteLifecyclePolicyResponse_registryId :: Lens.Lens' DeleteLifecyclePolicyResponse (Prelude.Maybe Prelude.Text)
deleteLifecyclePolicyResponse_registryId = Lens.lens (\DeleteLifecyclePolicyResponse' {registryId} -> registryId) (\s@DeleteLifecyclePolicyResponse' {} a -> s {registryId = a} :: DeleteLifecyclePolicyResponse)

-- | The repository name associated with the request.
deleteLifecyclePolicyResponse_repositoryName :: Lens.Lens' DeleteLifecyclePolicyResponse (Prelude.Maybe Prelude.Text)
deleteLifecyclePolicyResponse_repositoryName = Lens.lens (\DeleteLifecyclePolicyResponse' {repositoryName} -> repositoryName) (\s@DeleteLifecyclePolicyResponse' {} a -> s {repositoryName = a} :: DeleteLifecyclePolicyResponse)

-- | The JSON lifecycle policy text.
deleteLifecyclePolicyResponse_lifecyclePolicyText :: Lens.Lens' DeleteLifecyclePolicyResponse (Prelude.Maybe Prelude.Text)
deleteLifecyclePolicyResponse_lifecyclePolicyText = Lens.lens (\DeleteLifecyclePolicyResponse' {lifecyclePolicyText} -> lifecyclePolicyText) (\s@DeleteLifecyclePolicyResponse' {} a -> s {lifecyclePolicyText = a} :: DeleteLifecyclePolicyResponse)

-- | The time stamp of the last time that the lifecycle policy was run.
deleteLifecyclePolicyResponse_lastEvaluatedAt :: Lens.Lens' DeleteLifecyclePolicyResponse (Prelude.Maybe Prelude.UTCTime)
deleteLifecyclePolicyResponse_lastEvaluatedAt = Lens.lens (\DeleteLifecyclePolicyResponse' {lastEvaluatedAt} -> lastEvaluatedAt) (\s@DeleteLifecyclePolicyResponse' {} a -> s {lastEvaluatedAt = a} :: DeleteLifecyclePolicyResponse) Prelude.. Lens.mapping Prelude._Time

-- | The response's http status code.
deleteLifecyclePolicyResponse_httpStatus :: Lens.Lens' DeleteLifecyclePolicyResponse Prelude.Int
deleteLifecyclePolicyResponse_httpStatus = Lens.lens (\DeleteLifecyclePolicyResponse' {httpStatus} -> httpStatus) (\s@DeleteLifecyclePolicyResponse' {} a -> s {httpStatus = a} :: DeleteLifecyclePolicyResponse)

instance Prelude.NFData DeleteLifecyclePolicyResponse
