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
-- Module      : Amazonka.ECR.StartLifecyclePolicyPreview
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a preview of a lifecycle policy for the specified repository.
-- This allows you to see the results before associating the lifecycle
-- policy with the repository.
module Amazonka.ECR.StartLifecyclePolicyPreview
  ( -- * Creating a Request
    StartLifecyclePolicyPreview (..),
    newStartLifecyclePolicyPreview,

    -- * Request Lenses
    startLifecyclePolicyPreview_lifecyclePolicyText,
    startLifecyclePolicyPreview_registryId,
    startLifecyclePolicyPreview_repositoryName,

    -- * Destructuring the Response
    StartLifecyclePolicyPreviewResponse (..),
    newStartLifecyclePolicyPreviewResponse,

    -- * Response Lenses
    startLifecyclePolicyPreviewResponse_lifecyclePolicyText,
    startLifecyclePolicyPreviewResponse_registryId,
    startLifecyclePolicyPreviewResponse_repositoryName,
    startLifecyclePolicyPreviewResponse_status,
    startLifecyclePolicyPreviewResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECR.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartLifecyclePolicyPreview' smart constructor.
data StartLifecyclePolicyPreview = StartLifecyclePolicyPreview'
  { -- | The policy to be evaluated against. If you do not specify a policy, the
    -- current policy for the repository is used.
    lifecyclePolicyText :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services account ID associated with the registry that
    -- contains the repository. If you do not specify a registry, the default
    -- registry is assumed.
    registryId :: Prelude.Maybe Prelude.Text,
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
-- 'lifecyclePolicyText', 'startLifecyclePolicyPreview_lifecyclePolicyText' - The policy to be evaluated against. If you do not specify a policy, the
-- current policy for the repository is used.
--
-- 'registryId', 'startLifecyclePolicyPreview_registryId' - The Amazon Web Services account ID associated with the registry that
-- contains the repository. If you do not specify a registry, the default
-- registry is assumed.
--
-- 'repositoryName', 'startLifecyclePolicyPreview_repositoryName' - The name of the repository to be evaluated.
newStartLifecyclePolicyPreview ::
  -- | 'repositoryName'
  Prelude.Text ->
  StartLifecyclePolicyPreview
newStartLifecyclePolicyPreview pRepositoryName_ =
  StartLifecyclePolicyPreview'
    { lifecyclePolicyText =
        Prelude.Nothing,
      registryId = Prelude.Nothing,
      repositoryName = pRepositoryName_
    }

-- | The policy to be evaluated against. If you do not specify a policy, the
-- current policy for the repository is used.
startLifecyclePolicyPreview_lifecyclePolicyText :: Lens.Lens' StartLifecyclePolicyPreview (Prelude.Maybe Prelude.Text)
startLifecyclePolicyPreview_lifecyclePolicyText = Lens.lens (\StartLifecyclePolicyPreview' {lifecyclePolicyText} -> lifecyclePolicyText) (\s@StartLifecyclePolicyPreview' {} a -> s {lifecyclePolicyText = a} :: StartLifecyclePolicyPreview)

-- | The Amazon Web Services account ID associated with the registry that
-- contains the repository. If you do not specify a registry, the default
-- registry is assumed.
startLifecyclePolicyPreview_registryId :: Lens.Lens' StartLifecyclePolicyPreview (Prelude.Maybe Prelude.Text)
startLifecyclePolicyPreview_registryId = Lens.lens (\StartLifecyclePolicyPreview' {registryId} -> registryId) (\s@StartLifecyclePolicyPreview' {} a -> s {registryId = a} :: StartLifecyclePolicyPreview)

-- | The name of the repository to be evaluated.
startLifecyclePolicyPreview_repositoryName :: Lens.Lens' StartLifecyclePolicyPreview Prelude.Text
startLifecyclePolicyPreview_repositoryName = Lens.lens (\StartLifecyclePolicyPreview' {repositoryName} -> repositoryName) (\s@StartLifecyclePolicyPreview' {} a -> s {repositoryName = a} :: StartLifecyclePolicyPreview)

instance Core.AWSRequest StartLifecyclePolicyPreview where
  type
    AWSResponse StartLifecyclePolicyPreview =
      StartLifecyclePolicyPreviewResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartLifecyclePolicyPreviewResponse'
            Prelude.<$> (x Data..?> "lifecyclePolicyText")
            Prelude.<*> (x Data..?> "registryId")
            Prelude.<*> (x Data..?> "repositoryName")
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartLifecyclePolicyPreview where
  hashWithSalt _salt StartLifecyclePolicyPreview' {..} =
    _salt
      `Prelude.hashWithSalt` lifecyclePolicyText
      `Prelude.hashWithSalt` registryId
      `Prelude.hashWithSalt` repositoryName

instance Prelude.NFData StartLifecyclePolicyPreview where
  rnf StartLifecyclePolicyPreview' {..} =
    Prelude.rnf lifecyclePolicyText
      `Prelude.seq` Prelude.rnf registryId
      `Prelude.seq` Prelude.rnf repositoryName

instance Data.ToHeaders StartLifecyclePolicyPreview where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonEC2ContainerRegistry_V20150921.StartLifecyclePolicyPreview" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartLifecyclePolicyPreview where
  toJSON StartLifecyclePolicyPreview' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("lifecyclePolicyText" Data..=)
              Prelude.<$> lifecyclePolicyText,
            ("registryId" Data..=) Prelude.<$> registryId,
            Prelude.Just
              ("repositoryName" Data..= repositoryName)
          ]
      )

instance Data.ToPath StartLifecyclePolicyPreview where
  toPath = Prelude.const "/"

instance Data.ToQuery StartLifecyclePolicyPreview where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartLifecyclePolicyPreviewResponse' smart constructor.
data StartLifecyclePolicyPreviewResponse = StartLifecyclePolicyPreviewResponse'
  { -- | The JSON repository policy text.
    lifecyclePolicyText :: Prelude.Maybe Prelude.Text,
    -- | The registry ID associated with the request.
    registryId :: Prelude.Maybe Prelude.Text,
    -- | The repository name associated with the request.
    repositoryName :: Prelude.Maybe Prelude.Text,
    -- | The status of the lifecycle policy preview request.
    status :: Prelude.Maybe LifecyclePolicyPreviewStatus,
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
-- 'lifecyclePolicyText', 'startLifecyclePolicyPreviewResponse_lifecyclePolicyText' - The JSON repository policy text.
--
-- 'registryId', 'startLifecyclePolicyPreviewResponse_registryId' - The registry ID associated with the request.
--
-- 'repositoryName', 'startLifecyclePolicyPreviewResponse_repositoryName' - The repository name associated with the request.
--
-- 'status', 'startLifecyclePolicyPreviewResponse_status' - The status of the lifecycle policy preview request.
--
-- 'httpStatus', 'startLifecyclePolicyPreviewResponse_httpStatus' - The response's http status code.
newStartLifecyclePolicyPreviewResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartLifecyclePolicyPreviewResponse
newStartLifecyclePolicyPreviewResponse pHttpStatus_ =
  StartLifecyclePolicyPreviewResponse'
    { lifecyclePolicyText =
        Prelude.Nothing,
      registryId = Prelude.Nothing,
      repositoryName = Prelude.Nothing,
      status = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The JSON repository policy text.
startLifecyclePolicyPreviewResponse_lifecyclePolicyText :: Lens.Lens' StartLifecyclePolicyPreviewResponse (Prelude.Maybe Prelude.Text)
startLifecyclePolicyPreviewResponse_lifecyclePolicyText = Lens.lens (\StartLifecyclePolicyPreviewResponse' {lifecyclePolicyText} -> lifecyclePolicyText) (\s@StartLifecyclePolicyPreviewResponse' {} a -> s {lifecyclePolicyText = a} :: StartLifecyclePolicyPreviewResponse)

-- | The registry ID associated with the request.
startLifecyclePolicyPreviewResponse_registryId :: Lens.Lens' StartLifecyclePolicyPreviewResponse (Prelude.Maybe Prelude.Text)
startLifecyclePolicyPreviewResponse_registryId = Lens.lens (\StartLifecyclePolicyPreviewResponse' {registryId} -> registryId) (\s@StartLifecyclePolicyPreviewResponse' {} a -> s {registryId = a} :: StartLifecyclePolicyPreviewResponse)

-- | The repository name associated with the request.
startLifecyclePolicyPreviewResponse_repositoryName :: Lens.Lens' StartLifecyclePolicyPreviewResponse (Prelude.Maybe Prelude.Text)
startLifecyclePolicyPreviewResponse_repositoryName = Lens.lens (\StartLifecyclePolicyPreviewResponse' {repositoryName} -> repositoryName) (\s@StartLifecyclePolicyPreviewResponse' {} a -> s {repositoryName = a} :: StartLifecyclePolicyPreviewResponse)

-- | The status of the lifecycle policy preview request.
startLifecyclePolicyPreviewResponse_status :: Lens.Lens' StartLifecyclePolicyPreviewResponse (Prelude.Maybe LifecyclePolicyPreviewStatus)
startLifecyclePolicyPreviewResponse_status = Lens.lens (\StartLifecyclePolicyPreviewResponse' {status} -> status) (\s@StartLifecyclePolicyPreviewResponse' {} a -> s {status = a} :: StartLifecyclePolicyPreviewResponse)

-- | The response's http status code.
startLifecyclePolicyPreviewResponse_httpStatus :: Lens.Lens' StartLifecyclePolicyPreviewResponse Prelude.Int
startLifecyclePolicyPreviewResponse_httpStatus = Lens.lens (\StartLifecyclePolicyPreviewResponse' {httpStatus} -> httpStatus) (\s@StartLifecyclePolicyPreviewResponse' {} a -> s {httpStatus = a} :: StartLifecyclePolicyPreviewResponse)

instance
  Prelude.NFData
    StartLifecyclePolicyPreviewResponse
  where
  rnf StartLifecyclePolicyPreviewResponse' {..} =
    Prelude.rnf lifecyclePolicyText
      `Prelude.seq` Prelude.rnf registryId
      `Prelude.seq` Prelude.rnf repositoryName
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus
