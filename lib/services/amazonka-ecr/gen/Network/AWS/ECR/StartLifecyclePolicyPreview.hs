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
-- Module      : Network.AWS.ECR.StartLifecyclePolicyPreview
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a preview of a lifecycle policy for the specified repository.
-- This allows you to see the results before associating the lifecycle
-- policy with the repository.
module Network.AWS.ECR.StartLifecyclePolicyPreview
  ( -- * Creating a Request
    StartLifecyclePolicyPreview (..),
    newStartLifecyclePolicyPreview,

    -- * Request Lenses
    startLifecyclePolicyPreview_registryId,
    startLifecyclePolicyPreview_lifecyclePolicyText,
    startLifecyclePolicyPreview_repositoryName,

    -- * Destructuring the Response
    StartLifecyclePolicyPreviewResponse (..),
    newStartLifecyclePolicyPreviewResponse,

    -- * Response Lenses
    startLifecyclePolicyPreviewResponse_status,
    startLifecyclePolicyPreviewResponse_registryId,
    startLifecyclePolicyPreviewResponse_lifecyclePolicyText,
    startLifecyclePolicyPreviewResponse_repositoryName,
    startLifecyclePolicyPreviewResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ECR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStartLifecyclePolicyPreview' smart constructor.
data StartLifecyclePolicyPreview = StartLifecyclePolicyPreview'
  { -- | The Amazon Web Services account ID associated with the registry that
    -- contains the repository. If you do not specify a registry, the default
    -- registry is assumed.
    registryId :: Prelude.Maybe Prelude.Text,
    -- | The policy to be evaluated against. If you do not specify a policy, the
    -- current policy for the repository is used.
    lifecyclePolicyText :: Prelude.Maybe Prelude.Text,
    -- | The name of the repository to be evaluated.
    repositoryName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartLifecyclePolicyPreview' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'registryId', 'startLifecyclePolicyPreview_registryId' - The Amazon Web Services account ID associated with the registry that
-- contains the repository. If you do not specify a registry, the default
-- registry is assumed.
--
-- 'lifecyclePolicyText', 'startLifecyclePolicyPreview_lifecyclePolicyText' - The policy to be evaluated against. If you do not specify a policy, the
-- current policy for the repository is used.
--
-- 'repositoryName', 'startLifecyclePolicyPreview_repositoryName' - The name of the repository to be evaluated.
newStartLifecyclePolicyPreview ::
  -- | 'repositoryName'
  Prelude.Text ->
  StartLifecyclePolicyPreview
newStartLifecyclePolicyPreview pRepositoryName_ =
  StartLifecyclePolicyPreview'
    { registryId =
        Prelude.Nothing,
      lifecyclePolicyText = Prelude.Nothing,
      repositoryName = pRepositoryName_
    }

-- | The Amazon Web Services account ID associated with the registry that
-- contains the repository. If you do not specify a registry, the default
-- registry is assumed.
startLifecyclePolicyPreview_registryId :: Lens.Lens' StartLifecyclePolicyPreview (Prelude.Maybe Prelude.Text)
startLifecyclePolicyPreview_registryId = Lens.lens (\StartLifecyclePolicyPreview' {registryId} -> registryId) (\s@StartLifecyclePolicyPreview' {} a -> s {registryId = a} :: StartLifecyclePolicyPreview)

-- | The policy to be evaluated against. If you do not specify a policy, the
-- current policy for the repository is used.
startLifecyclePolicyPreview_lifecyclePolicyText :: Lens.Lens' StartLifecyclePolicyPreview (Prelude.Maybe Prelude.Text)
startLifecyclePolicyPreview_lifecyclePolicyText = Lens.lens (\StartLifecyclePolicyPreview' {lifecyclePolicyText} -> lifecyclePolicyText) (\s@StartLifecyclePolicyPreview' {} a -> s {lifecyclePolicyText = a} :: StartLifecyclePolicyPreview)

-- | The name of the repository to be evaluated.
startLifecyclePolicyPreview_repositoryName :: Lens.Lens' StartLifecyclePolicyPreview Prelude.Text
startLifecyclePolicyPreview_repositoryName = Lens.lens (\StartLifecyclePolicyPreview' {repositoryName} -> repositoryName) (\s@StartLifecyclePolicyPreview' {} a -> s {repositoryName = a} :: StartLifecyclePolicyPreview)

instance Core.AWSRequest StartLifecyclePolicyPreview where
  type
    AWSResponse StartLifecyclePolicyPreview =
      StartLifecyclePolicyPreviewResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StartLifecyclePolicyPreviewResponse'
            Prelude.<$> (x Core..?> "status")
            Prelude.<*> (x Core..?> "registryId")
            Prelude.<*> (x Core..?> "lifecyclePolicyText")
            Prelude.<*> (x Core..?> "repositoryName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartLifecyclePolicyPreview

instance Prelude.NFData StartLifecyclePolicyPreview

instance Core.ToHeaders StartLifecyclePolicyPreview where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonEC2ContainerRegistry_V20150921.StartLifecyclePolicyPreview" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON StartLifecyclePolicyPreview where
  toJSON StartLifecyclePolicyPreview' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("registryId" Core..=) Prelude.<$> registryId,
            ("lifecyclePolicyText" Core..=)
              Prelude.<$> lifecyclePolicyText,
            Prelude.Just
              ("repositoryName" Core..= repositoryName)
          ]
      )

instance Core.ToPath StartLifecyclePolicyPreview where
  toPath = Prelude.const "/"

instance Core.ToQuery StartLifecyclePolicyPreview where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartLifecyclePolicyPreviewResponse' smart constructor.
data StartLifecyclePolicyPreviewResponse = StartLifecyclePolicyPreviewResponse'
  { -- | The status of the lifecycle policy preview request.
    status :: Prelude.Maybe LifecyclePolicyPreviewStatus,
    -- | The registry ID associated with the request.
    registryId :: Prelude.Maybe Prelude.Text,
    -- | The JSON repository policy text.
    lifecyclePolicyText :: Prelude.Maybe Prelude.Text,
    -- | The repository name associated with the request.
    repositoryName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartLifecyclePolicyPreviewResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'startLifecyclePolicyPreviewResponse_status' - The status of the lifecycle policy preview request.
--
-- 'registryId', 'startLifecyclePolicyPreviewResponse_registryId' - The registry ID associated with the request.
--
-- 'lifecyclePolicyText', 'startLifecyclePolicyPreviewResponse_lifecyclePolicyText' - The JSON repository policy text.
--
-- 'repositoryName', 'startLifecyclePolicyPreviewResponse_repositoryName' - The repository name associated with the request.
--
-- 'httpStatus', 'startLifecyclePolicyPreviewResponse_httpStatus' - The response's http status code.
newStartLifecyclePolicyPreviewResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartLifecyclePolicyPreviewResponse
newStartLifecyclePolicyPreviewResponse pHttpStatus_ =
  StartLifecyclePolicyPreviewResponse'
    { status =
        Prelude.Nothing,
      registryId = Prelude.Nothing,
      lifecyclePolicyText = Prelude.Nothing,
      repositoryName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The status of the lifecycle policy preview request.
startLifecyclePolicyPreviewResponse_status :: Lens.Lens' StartLifecyclePolicyPreviewResponse (Prelude.Maybe LifecyclePolicyPreviewStatus)
startLifecyclePolicyPreviewResponse_status = Lens.lens (\StartLifecyclePolicyPreviewResponse' {status} -> status) (\s@StartLifecyclePolicyPreviewResponse' {} a -> s {status = a} :: StartLifecyclePolicyPreviewResponse)

-- | The registry ID associated with the request.
startLifecyclePolicyPreviewResponse_registryId :: Lens.Lens' StartLifecyclePolicyPreviewResponse (Prelude.Maybe Prelude.Text)
startLifecyclePolicyPreviewResponse_registryId = Lens.lens (\StartLifecyclePolicyPreviewResponse' {registryId} -> registryId) (\s@StartLifecyclePolicyPreviewResponse' {} a -> s {registryId = a} :: StartLifecyclePolicyPreviewResponse)

-- | The JSON repository policy text.
startLifecyclePolicyPreviewResponse_lifecyclePolicyText :: Lens.Lens' StartLifecyclePolicyPreviewResponse (Prelude.Maybe Prelude.Text)
startLifecyclePolicyPreviewResponse_lifecyclePolicyText = Lens.lens (\StartLifecyclePolicyPreviewResponse' {lifecyclePolicyText} -> lifecyclePolicyText) (\s@StartLifecyclePolicyPreviewResponse' {} a -> s {lifecyclePolicyText = a} :: StartLifecyclePolicyPreviewResponse)

-- | The repository name associated with the request.
startLifecyclePolicyPreviewResponse_repositoryName :: Lens.Lens' StartLifecyclePolicyPreviewResponse (Prelude.Maybe Prelude.Text)
startLifecyclePolicyPreviewResponse_repositoryName = Lens.lens (\StartLifecyclePolicyPreviewResponse' {repositoryName} -> repositoryName) (\s@StartLifecyclePolicyPreviewResponse' {} a -> s {repositoryName = a} :: StartLifecyclePolicyPreviewResponse)

-- | The response's http status code.
startLifecyclePolicyPreviewResponse_httpStatus :: Lens.Lens' StartLifecyclePolicyPreviewResponse Prelude.Int
startLifecyclePolicyPreviewResponse_httpStatus = Lens.lens (\StartLifecyclePolicyPreviewResponse' {httpStatus} -> httpStatus) (\s@StartLifecyclePolicyPreviewResponse' {} a -> s {httpStatus = a} :: StartLifecyclePolicyPreviewResponse)

instance
  Prelude.NFData
    StartLifecyclePolicyPreviewResponse
