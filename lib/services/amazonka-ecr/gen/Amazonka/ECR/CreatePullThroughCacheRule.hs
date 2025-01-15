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
-- Module      : Amazonka.ECR.CreatePullThroughCacheRule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a pull through cache rule. A pull through cache rule provides a
-- way to cache images from an external public registry in your Amazon ECR
-- private registry.
module Amazonka.ECR.CreatePullThroughCacheRule
  ( -- * Creating a Request
    CreatePullThroughCacheRule (..),
    newCreatePullThroughCacheRule,

    -- * Request Lenses
    createPullThroughCacheRule_registryId,
    createPullThroughCacheRule_ecrRepositoryPrefix,
    createPullThroughCacheRule_upstreamRegistryUrl,

    -- * Destructuring the Response
    CreatePullThroughCacheRuleResponse (..),
    newCreatePullThroughCacheRuleResponse,

    -- * Response Lenses
    createPullThroughCacheRuleResponse_createdAt,
    createPullThroughCacheRuleResponse_ecrRepositoryPrefix,
    createPullThroughCacheRuleResponse_registryId,
    createPullThroughCacheRuleResponse_upstreamRegistryUrl,
    createPullThroughCacheRuleResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECR.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreatePullThroughCacheRule' smart constructor.
data CreatePullThroughCacheRule = CreatePullThroughCacheRule'
  { -- | The Amazon Web Services account ID associated with the registry to
    -- create the pull through cache rule for. If you do not specify a
    -- registry, the default registry is assumed.
    registryId :: Prelude.Maybe Prelude.Text,
    -- | The repository name prefix to use when caching images from the source
    -- registry.
    ecrRepositoryPrefix :: Prelude.Text,
    -- | The registry URL of the upstream public registry to use as the source
    -- for the pull through cache rule.
    upstreamRegistryUrl :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreatePullThroughCacheRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'registryId', 'createPullThroughCacheRule_registryId' - The Amazon Web Services account ID associated with the registry to
-- create the pull through cache rule for. If you do not specify a
-- registry, the default registry is assumed.
--
-- 'ecrRepositoryPrefix', 'createPullThroughCacheRule_ecrRepositoryPrefix' - The repository name prefix to use when caching images from the source
-- registry.
--
-- 'upstreamRegistryUrl', 'createPullThroughCacheRule_upstreamRegistryUrl' - The registry URL of the upstream public registry to use as the source
-- for the pull through cache rule.
newCreatePullThroughCacheRule ::
  -- | 'ecrRepositoryPrefix'
  Prelude.Text ->
  -- | 'upstreamRegistryUrl'
  Prelude.Text ->
  CreatePullThroughCacheRule
newCreatePullThroughCacheRule
  pEcrRepositoryPrefix_
  pUpstreamRegistryUrl_ =
    CreatePullThroughCacheRule'
      { registryId =
          Prelude.Nothing,
        ecrRepositoryPrefix = pEcrRepositoryPrefix_,
        upstreamRegistryUrl = pUpstreamRegistryUrl_
      }

-- | The Amazon Web Services account ID associated with the registry to
-- create the pull through cache rule for. If you do not specify a
-- registry, the default registry is assumed.
createPullThroughCacheRule_registryId :: Lens.Lens' CreatePullThroughCacheRule (Prelude.Maybe Prelude.Text)
createPullThroughCacheRule_registryId = Lens.lens (\CreatePullThroughCacheRule' {registryId} -> registryId) (\s@CreatePullThroughCacheRule' {} a -> s {registryId = a} :: CreatePullThroughCacheRule)

-- | The repository name prefix to use when caching images from the source
-- registry.
createPullThroughCacheRule_ecrRepositoryPrefix :: Lens.Lens' CreatePullThroughCacheRule Prelude.Text
createPullThroughCacheRule_ecrRepositoryPrefix = Lens.lens (\CreatePullThroughCacheRule' {ecrRepositoryPrefix} -> ecrRepositoryPrefix) (\s@CreatePullThroughCacheRule' {} a -> s {ecrRepositoryPrefix = a} :: CreatePullThroughCacheRule)

-- | The registry URL of the upstream public registry to use as the source
-- for the pull through cache rule.
createPullThroughCacheRule_upstreamRegistryUrl :: Lens.Lens' CreatePullThroughCacheRule Prelude.Text
createPullThroughCacheRule_upstreamRegistryUrl = Lens.lens (\CreatePullThroughCacheRule' {upstreamRegistryUrl} -> upstreamRegistryUrl) (\s@CreatePullThroughCacheRule' {} a -> s {upstreamRegistryUrl = a} :: CreatePullThroughCacheRule)

instance Core.AWSRequest CreatePullThroughCacheRule where
  type
    AWSResponse CreatePullThroughCacheRule =
      CreatePullThroughCacheRuleResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreatePullThroughCacheRuleResponse'
            Prelude.<$> (x Data..?> "createdAt")
            Prelude.<*> (x Data..?> "ecrRepositoryPrefix")
            Prelude.<*> (x Data..?> "registryId")
            Prelude.<*> (x Data..?> "upstreamRegistryUrl")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreatePullThroughCacheRule where
  hashWithSalt _salt CreatePullThroughCacheRule' {..} =
    _salt
      `Prelude.hashWithSalt` registryId
      `Prelude.hashWithSalt` ecrRepositoryPrefix
      `Prelude.hashWithSalt` upstreamRegistryUrl

instance Prelude.NFData CreatePullThroughCacheRule where
  rnf CreatePullThroughCacheRule' {..} =
    Prelude.rnf registryId `Prelude.seq`
      Prelude.rnf ecrRepositoryPrefix `Prelude.seq`
        Prelude.rnf upstreamRegistryUrl

instance Data.ToHeaders CreatePullThroughCacheRule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonEC2ContainerRegistry_V20150921.CreatePullThroughCacheRule" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreatePullThroughCacheRule where
  toJSON CreatePullThroughCacheRule' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("registryId" Data..=) Prelude.<$> registryId,
            Prelude.Just
              ("ecrRepositoryPrefix" Data..= ecrRepositoryPrefix),
            Prelude.Just
              ("upstreamRegistryUrl" Data..= upstreamRegistryUrl)
          ]
      )

instance Data.ToPath CreatePullThroughCacheRule where
  toPath = Prelude.const "/"

instance Data.ToQuery CreatePullThroughCacheRule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreatePullThroughCacheRuleResponse' smart constructor.
data CreatePullThroughCacheRuleResponse = CreatePullThroughCacheRuleResponse'
  { -- | The date and time, in JavaScript date format, when the pull through
    -- cache rule was created.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | The Amazon ECR repository prefix associated with the pull through cache
    -- rule.
    ecrRepositoryPrefix :: Prelude.Maybe Prelude.Text,
    -- | The registry ID associated with the request.
    registryId :: Prelude.Maybe Prelude.Text,
    -- | The upstream registry URL associated with the pull through cache rule.
    upstreamRegistryUrl :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreatePullThroughCacheRuleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdAt', 'createPullThroughCacheRuleResponse_createdAt' - The date and time, in JavaScript date format, when the pull through
-- cache rule was created.
--
-- 'ecrRepositoryPrefix', 'createPullThroughCacheRuleResponse_ecrRepositoryPrefix' - The Amazon ECR repository prefix associated with the pull through cache
-- rule.
--
-- 'registryId', 'createPullThroughCacheRuleResponse_registryId' - The registry ID associated with the request.
--
-- 'upstreamRegistryUrl', 'createPullThroughCacheRuleResponse_upstreamRegistryUrl' - The upstream registry URL associated with the pull through cache rule.
--
-- 'httpStatus', 'createPullThroughCacheRuleResponse_httpStatus' - The response's http status code.
newCreatePullThroughCacheRuleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreatePullThroughCacheRuleResponse
newCreatePullThroughCacheRuleResponse pHttpStatus_ =
  CreatePullThroughCacheRuleResponse'
    { createdAt =
        Prelude.Nothing,
      ecrRepositoryPrefix = Prelude.Nothing,
      registryId = Prelude.Nothing,
      upstreamRegistryUrl = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The date and time, in JavaScript date format, when the pull through
-- cache rule was created.
createPullThroughCacheRuleResponse_createdAt :: Lens.Lens' CreatePullThroughCacheRuleResponse (Prelude.Maybe Prelude.UTCTime)
createPullThroughCacheRuleResponse_createdAt = Lens.lens (\CreatePullThroughCacheRuleResponse' {createdAt} -> createdAt) (\s@CreatePullThroughCacheRuleResponse' {} a -> s {createdAt = a} :: CreatePullThroughCacheRuleResponse) Prelude.. Lens.mapping Data._Time

-- | The Amazon ECR repository prefix associated with the pull through cache
-- rule.
createPullThroughCacheRuleResponse_ecrRepositoryPrefix :: Lens.Lens' CreatePullThroughCacheRuleResponse (Prelude.Maybe Prelude.Text)
createPullThroughCacheRuleResponse_ecrRepositoryPrefix = Lens.lens (\CreatePullThroughCacheRuleResponse' {ecrRepositoryPrefix} -> ecrRepositoryPrefix) (\s@CreatePullThroughCacheRuleResponse' {} a -> s {ecrRepositoryPrefix = a} :: CreatePullThroughCacheRuleResponse)

-- | The registry ID associated with the request.
createPullThroughCacheRuleResponse_registryId :: Lens.Lens' CreatePullThroughCacheRuleResponse (Prelude.Maybe Prelude.Text)
createPullThroughCacheRuleResponse_registryId = Lens.lens (\CreatePullThroughCacheRuleResponse' {registryId} -> registryId) (\s@CreatePullThroughCacheRuleResponse' {} a -> s {registryId = a} :: CreatePullThroughCacheRuleResponse)

-- | The upstream registry URL associated with the pull through cache rule.
createPullThroughCacheRuleResponse_upstreamRegistryUrl :: Lens.Lens' CreatePullThroughCacheRuleResponse (Prelude.Maybe Prelude.Text)
createPullThroughCacheRuleResponse_upstreamRegistryUrl = Lens.lens (\CreatePullThroughCacheRuleResponse' {upstreamRegistryUrl} -> upstreamRegistryUrl) (\s@CreatePullThroughCacheRuleResponse' {} a -> s {upstreamRegistryUrl = a} :: CreatePullThroughCacheRuleResponse)

-- | The response's http status code.
createPullThroughCacheRuleResponse_httpStatus :: Lens.Lens' CreatePullThroughCacheRuleResponse Prelude.Int
createPullThroughCacheRuleResponse_httpStatus = Lens.lens (\CreatePullThroughCacheRuleResponse' {httpStatus} -> httpStatus) (\s@CreatePullThroughCacheRuleResponse' {} a -> s {httpStatus = a} :: CreatePullThroughCacheRuleResponse)

instance
  Prelude.NFData
    CreatePullThroughCacheRuleResponse
  where
  rnf CreatePullThroughCacheRuleResponse' {..} =
    Prelude.rnf createdAt `Prelude.seq`
      Prelude.rnf ecrRepositoryPrefix `Prelude.seq`
        Prelude.rnf registryId `Prelude.seq`
          Prelude.rnf upstreamRegistryUrl `Prelude.seq`
            Prelude.rnf httpStatus
