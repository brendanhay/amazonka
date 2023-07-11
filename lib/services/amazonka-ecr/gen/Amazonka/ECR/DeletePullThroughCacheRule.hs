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
-- Module      : Amazonka.ECR.DeletePullThroughCacheRule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a pull through cache rule.
module Amazonka.ECR.DeletePullThroughCacheRule
  ( -- * Creating a Request
    DeletePullThroughCacheRule (..),
    newDeletePullThroughCacheRule,

    -- * Request Lenses
    deletePullThroughCacheRule_registryId,
    deletePullThroughCacheRule_ecrRepositoryPrefix,

    -- * Destructuring the Response
    DeletePullThroughCacheRuleResponse (..),
    newDeletePullThroughCacheRuleResponse,

    -- * Response Lenses
    deletePullThroughCacheRuleResponse_createdAt,
    deletePullThroughCacheRuleResponse_ecrRepositoryPrefix,
    deletePullThroughCacheRuleResponse_registryId,
    deletePullThroughCacheRuleResponse_upstreamRegistryUrl,
    deletePullThroughCacheRuleResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECR.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeletePullThroughCacheRule' smart constructor.
data DeletePullThroughCacheRule = DeletePullThroughCacheRule'
  { -- | The Amazon Web Services account ID associated with the registry that
    -- contains the pull through cache rule. If you do not specify a registry,
    -- the default registry is assumed.
    registryId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon ECR repository prefix associated with the pull through cache
    -- rule to delete.
    ecrRepositoryPrefix :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeletePullThroughCacheRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'registryId', 'deletePullThroughCacheRule_registryId' - The Amazon Web Services account ID associated with the registry that
-- contains the pull through cache rule. If you do not specify a registry,
-- the default registry is assumed.
--
-- 'ecrRepositoryPrefix', 'deletePullThroughCacheRule_ecrRepositoryPrefix' - The Amazon ECR repository prefix associated with the pull through cache
-- rule to delete.
newDeletePullThroughCacheRule ::
  -- | 'ecrRepositoryPrefix'
  Prelude.Text ->
  DeletePullThroughCacheRule
newDeletePullThroughCacheRule pEcrRepositoryPrefix_ =
  DeletePullThroughCacheRule'
    { registryId =
        Prelude.Nothing,
      ecrRepositoryPrefix = pEcrRepositoryPrefix_
    }

-- | The Amazon Web Services account ID associated with the registry that
-- contains the pull through cache rule. If you do not specify a registry,
-- the default registry is assumed.
deletePullThroughCacheRule_registryId :: Lens.Lens' DeletePullThroughCacheRule (Prelude.Maybe Prelude.Text)
deletePullThroughCacheRule_registryId = Lens.lens (\DeletePullThroughCacheRule' {registryId} -> registryId) (\s@DeletePullThroughCacheRule' {} a -> s {registryId = a} :: DeletePullThroughCacheRule)

-- | The Amazon ECR repository prefix associated with the pull through cache
-- rule to delete.
deletePullThroughCacheRule_ecrRepositoryPrefix :: Lens.Lens' DeletePullThroughCacheRule Prelude.Text
deletePullThroughCacheRule_ecrRepositoryPrefix = Lens.lens (\DeletePullThroughCacheRule' {ecrRepositoryPrefix} -> ecrRepositoryPrefix) (\s@DeletePullThroughCacheRule' {} a -> s {ecrRepositoryPrefix = a} :: DeletePullThroughCacheRule)

instance Core.AWSRequest DeletePullThroughCacheRule where
  type
    AWSResponse DeletePullThroughCacheRule =
      DeletePullThroughCacheRuleResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeletePullThroughCacheRuleResponse'
            Prelude.<$> (x Data..?> "createdAt")
            Prelude.<*> (x Data..?> "ecrRepositoryPrefix")
            Prelude.<*> (x Data..?> "registryId")
            Prelude.<*> (x Data..?> "upstreamRegistryUrl")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeletePullThroughCacheRule where
  hashWithSalt _salt DeletePullThroughCacheRule' {..} =
    _salt
      `Prelude.hashWithSalt` registryId
      `Prelude.hashWithSalt` ecrRepositoryPrefix

instance Prelude.NFData DeletePullThroughCacheRule where
  rnf DeletePullThroughCacheRule' {..} =
    Prelude.rnf registryId
      `Prelude.seq` Prelude.rnf ecrRepositoryPrefix

instance Data.ToHeaders DeletePullThroughCacheRule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonEC2ContainerRegistry_V20150921.DeletePullThroughCacheRule" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeletePullThroughCacheRule where
  toJSON DeletePullThroughCacheRule' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("registryId" Data..=) Prelude.<$> registryId,
            Prelude.Just
              ("ecrRepositoryPrefix" Data..= ecrRepositoryPrefix)
          ]
      )

instance Data.ToPath DeletePullThroughCacheRule where
  toPath = Prelude.const "/"

instance Data.ToQuery DeletePullThroughCacheRule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeletePullThroughCacheRuleResponse' smart constructor.
data DeletePullThroughCacheRuleResponse = DeletePullThroughCacheRuleResponse'
  { -- | The timestamp associated with the pull through cache rule.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | The Amazon ECR repository prefix associated with the request.
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
-- Create a value of 'DeletePullThroughCacheRuleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdAt', 'deletePullThroughCacheRuleResponse_createdAt' - The timestamp associated with the pull through cache rule.
--
-- 'ecrRepositoryPrefix', 'deletePullThroughCacheRuleResponse_ecrRepositoryPrefix' - The Amazon ECR repository prefix associated with the request.
--
-- 'registryId', 'deletePullThroughCacheRuleResponse_registryId' - The registry ID associated with the request.
--
-- 'upstreamRegistryUrl', 'deletePullThroughCacheRuleResponse_upstreamRegistryUrl' - The upstream registry URL associated with the pull through cache rule.
--
-- 'httpStatus', 'deletePullThroughCacheRuleResponse_httpStatus' - The response's http status code.
newDeletePullThroughCacheRuleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeletePullThroughCacheRuleResponse
newDeletePullThroughCacheRuleResponse pHttpStatus_ =
  DeletePullThroughCacheRuleResponse'
    { createdAt =
        Prelude.Nothing,
      ecrRepositoryPrefix = Prelude.Nothing,
      registryId = Prelude.Nothing,
      upstreamRegistryUrl = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The timestamp associated with the pull through cache rule.
deletePullThroughCacheRuleResponse_createdAt :: Lens.Lens' DeletePullThroughCacheRuleResponse (Prelude.Maybe Prelude.UTCTime)
deletePullThroughCacheRuleResponse_createdAt = Lens.lens (\DeletePullThroughCacheRuleResponse' {createdAt} -> createdAt) (\s@DeletePullThroughCacheRuleResponse' {} a -> s {createdAt = a} :: DeletePullThroughCacheRuleResponse) Prelude.. Lens.mapping Data._Time

-- | The Amazon ECR repository prefix associated with the request.
deletePullThroughCacheRuleResponse_ecrRepositoryPrefix :: Lens.Lens' DeletePullThroughCacheRuleResponse (Prelude.Maybe Prelude.Text)
deletePullThroughCacheRuleResponse_ecrRepositoryPrefix = Lens.lens (\DeletePullThroughCacheRuleResponse' {ecrRepositoryPrefix} -> ecrRepositoryPrefix) (\s@DeletePullThroughCacheRuleResponse' {} a -> s {ecrRepositoryPrefix = a} :: DeletePullThroughCacheRuleResponse)

-- | The registry ID associated with the request.
deletePullThroughCacheRuleResponse_registryId :: Lens.Lens' DeletePullThroughCacheRuleResponse (Prelude.Maybe Prelude.Text)
deletePullThroughCacheRuleResponse_registryId = Lens.lens (\DeletePullThroughCacheRuleResponse' {registryId} -> registryId) (\s@DeletePullThroughCacheRuleResponse' {} a -> s {registryId = a} :: DeletePullThroughCacheRuleResponse)

-- | The upstream registry URL associated with the pull through cache rule.
deletePullThroughCacheRuleResponse_upstreamRegistryUrl :: Lens.Lens' DeletePullThroughCacheRuleResponse (Prelude.Maybe Prelude.Text)
deletePullThroughCacheRuleResponse_upstreamRegistryUrl = Lens.lens (\DeletePullThroughCacheRuleResponse' {upstreamRegistryUrl} -> upstreamRegistryUrl) (\s@DeletePullThroughCacheRuleResponse' {} a -> s {upstreamRegistryUrl = a} :: DeletePullThroughCacheRuleResponse)

-- | The response's http status code.
deletePullThroughCacheRuleResponse_httpStatus :: Lens.Lens' DeletePullThroughCacheRuleResponse Prelude.Int
deletePullThroughCacheRuleResponse_httpStatus = Lens.lens (\DeletePullThroughCacheRuleResponse' {httpStatus} -> httpStatus) (\s@DeletePullThroughCacheRuleResponse' {} a -> s {httpStatus = a} :: DeletePullThroughCacheRuleResponse)

instance
  Prelude.NFData
    DeletePullThroughCacheRuleResponse
  where
  rnf DeletePullThroughCacheRuleResponse' {..} =
    Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf ecrRepositoryPrefix
      `Prelude.seq` Prelude.rnf registryId
      `Prelude.seq` Prelude.rnf upstreamRegistryUrl
      `Prelude.seq` Prelude.rnf httpStatus
