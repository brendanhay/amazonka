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
    startLifecyclePolicyPreviewResponse_repositoryName,
    startLifecyclePolicyPreviewResponse_lifecyclePolicyText,
    startLifecyclePolicyPreviewResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ECR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStartLifecyclePolicyPreview' smart constructor.
data StartLifecyclePolicyPreview = StartLifecyclePolicyPreview'
  { -- | The AWS account ID associated with the registry that contains the
    -- repository. If you do not specify a registry, the default registry is
    -- assumed.
    registryId :: Core.Maybe Core.Text,
    -- | The policy to be evaluated against. If you do not specify a policy, the
    -- current policy for the repository is used.
    lifecyclePolicyText :: Core.Maybe Core.Text,
    -- | The name of the repository to be evaluated.
    repositoryName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StartLifecyclePolicyPreview' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'registryId', 'startLifecyclePolicyPreview_registryId' - The AWS account ID associated with the registry that contains the
-- repository. If you do not specify a registry, the default registry is
-- assumed.
--
-- 'lifecyclePolicyText', 'startLifecyclePolicyPreview_lifecyclePolicyText' - The policy to be evaluated against. If you do not specify a policy, the
-- current policy for the repository is used.
--
-- 'repositoryName', 'startLifecyclePolicyPreview_repositoryName' - The name of the repository to be evaluated.
newStartLifecyclePolicyPreview ::
  -- | 'repositoryName'
  Core.Text ->
  StartLifecyclePolicyPreview
newStartLifecyclePolicyPreview pRepositoryName_ =
  StartLifecyclePolicyPreview'
    { registryId =
        Core.Nothing,
      lifecyclePolicyText = Core.Nothing,
      repositoryName = pRepositoryName_
    }

-- | The AWS account ID associated with the registry that contains the
-- repository. If you do not specify a registry, the default registry is
-- assumed.
startLifecyclePolicyPreview_registryId :: Lens.Lens' StartLifecyclePolicyPreview (Core.Maybe Core.Text)
startLifecyclePolicyPreview_registryId = Lens.lens (\StartLifecyclePolicyPreview' {registryId} -> registryId) (\s@StartLifecyclePolicyPreview' {} a -> s {registryId = a} :: StartLifecyclePolicyPreview)

-- | The policy to be evaluated against. If you do not specify a policy, the
-- current policy for the repository is used.
startLifecyclePolicyPreview_lifecyclePolicyText :: Lens.Lens' StartLifecyclePolicyPreview (Core.Maybe Core.Text)
startLifecyclePolicyPreview_lifecyclePolicyText = Lens.lens (\StartLifecyclePolicyPreview' {lifecyclePolicyText} -> lifecyclePolicyText) (\s@StartLifecyclePolicyPreview' {} a -> s {lifecyclePolicyText = a} :: StartLifecyclePolicyPreview)

-- | The name of the repository to be evaluated.
startLifecyclePolicyPreview_repositoryName :: Lens.Lens' StartLifecyclePolicyPreview Core.Text
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
            Core.<$> (x Core..?> "status")
            Core.<*> (x Core..?> "registryId")
            Core.<*> (x Core..?> "repositoryName")
            Core.<*> (x Core..?> "lifecyclePolicyText")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable StartLifecyclePolicyPreview

instance Core.NFData StartLifecyclePolicyPreview

instance Core.ToHeaders StartLifecyclePolicyPreview where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonEC2ContainerRegistry_V20150921.StartLifecyclePolicyPreview" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON StartLifecyclePolicyPreview where
  toJSON StartLifecyclePolicyPreview' {..} =
    Core.object
      ( Core.catMaybes
          [ ("registryId" Core..=) Core.<$> registryId,
            ("lifecyclePolicyText" Core..=)
              Core.<$> lifecyclePolicyText,
            Core.Just ("repositoryName" Core..= repositoryName)
          ]
      )

instance Core.ToPath StartLifecyclePolicyPreview where
  toPath = Core.const "/"

instance Core.ToQuery StartLifecyclePolicyPreview where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newStartLifecyclePolicyPreviewResponse' smart constructor.
data StartLifecyclePolicyPreviewResponse = StartLifecyclePolicyPreviewResponse'
  { -- | The status of the lifecycle policy preview request.
    status :: Core.Maybe LifecyclePolicyPreviewStatus,
    -- | The registry ID associated with the request.
    registryId :: Core.Maybe Core.Text,
    -- | The repository name associated with the request.
    repositoryName :: Core.Maybe Core.Text,
    -- | The JSON repository policy text.
    lifecyclePolicyText :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'repositoryName', 'startLifecyclePolicyPreviewResponse_repositoryName' - The repository name associated with the request.
--
-- 'lifecyclePolicyText', 'startLifecyclePolicyPreviewResponse_lifecyclePolicyText' - The JSON repository policy text.
--
-- 'httpStatus', 'startLifecyclePolicyPreviewResponse_httpStatus' - The response's http status code.
newStartLifecyclePolicyPreviewResponse ::
  -- | 'httpStatus'
  Core.Int ->
  StartLifecyclePolicyPreviewResponse
newStartLifecyclePolicyPreviewResponse pHttpStatus_ =
  StartLifecyclePolicyPreviewResponse'
    { status =
        Core.Nothing,
      registryId = Core.Nothing,
      repositoryName = Core.Nothing,
      lifecyclePolicyText = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The status of the lifecycle policy preview request.
startLifecyclePolicyPreviewResponse_status :: Lens.Lens' StartLifecyclePolicyPreviewResponse (Core.Maybe LifecyclePolicyPreviewStatus)
startLifecyclePolicyPreviewResponse_status = Lens.lens (\StartLifecyclePolicyPreviewResponse' {status} -> status) (\s@StartLifecyclePolicyPreviewResponse' {} a -> s {status = a} :: StartLifecyclePolicyPreviewResponse)

-- | The registry ID associated with the request.
startLifecyclePolicyPreviewResponse_registryId :: Lens.Lens' StartLifecyclePolicyPreviewResponse (Core.Maybe Core.Text)
startLifecyclePolicyPreviewResponse_registryId = Lens.lens (\StartLifecyclePolicyPreviewResponse' {registryId} -> registryId) (\s@StartLifecyclePolicyPreviewResponse' {} a -> s {registryId = a} :: StartLifecyclePolicyPreviewResponse)

-- | The repository name associated with the request.
startLifecyclePolicyPreviewResponse_repositoryName :: Lens.Lens' StartLifecyclePolicyPreviewResponse (Core.Maybe Core.Text)
startLifecyclePolicyPreviewResponse_repositoryName = Lens.lens (\StartLifecyclePolicyPreviewResponse' {repositoryName} -> repositoryName) (\s@StartLifecyclePolicyPreviewResponse' {} a -> s {repositoryName = a} :: StartLifecyclePolicyPreviewResponse)

-- | The JSON repository policy text.
startLifecyclePolicyPreviewResponse_lifecyclePolicyText :: Lens.Lens' StartLifecyclePolicyPreviewResponse (Core.Maybe Core.Text)
startLifecyclePolicyPreviewResponse_lifecyclePolicyText = Lens.lens (\StartLifecyclePolicyPreviewResponse' {lifecyclePolicyText} -> lifecyclePolicyText) (\s@StartLifecyclePolicyPreviewResponse' {} a -> s {lifecyclePolicyText = a} :: StartLifecyclePolicyPreviewResponse)

-- | The response's http status code.
startLifecyclePolicyPreviewResponse_httpStatus :: Lens.Lens' StartLifecyclePolicyPreviewResponse Core.Int
startLifecyclePolicyPreviewResponse_httpStatus = Lens.lens (\StartLifecyclePolicyPreviewResponse' {httpStatus} -> httpStatus) (\s@StartLifecyclePolicyPreviewResponse' {} a -> s {httpStatus = a} :: StartLifecyclePolicyPreviewResponse)

instance
  Core.NFData
    StartLifecyclePolicyPreviewResponse
