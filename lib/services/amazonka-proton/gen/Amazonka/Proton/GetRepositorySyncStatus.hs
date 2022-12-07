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
-- Module      : Amazonka.Proton.GetRepositorySyncStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the sync status of a repository used for Proton template sync. For
-- more information about template sync, see .
--
-- A repository sync status isn\'t tied to the Proton Repository resource
-- (or any other Proton resource). Therefore, tags on an Proton Repository
-- resource have no effect on this action. Specifically, you can\'t use
-- these tags to control access to this action using Attribute-based access
-- control (ABAC).
--
-- For more information about ABAC, see
-- <https://docs.aws.amazon.com/proton/latest/userguide/security_iam_service-with-iam.html#security_iam_service-with-iam-tags ABAC>
-- in the /Proton User Guide/.
module Amazonka.Proton.GetRepositorySyncStatus
  ( -- * Creating a Request
    GetRepositorySyncStatus (..),
    newGetRepositorySyncStatus,

    -- * Request Lenses
    getRepositorySyncStatus_branch,
    getRepositorySyncStatus_repositoryName,
    getRepositorySyncStatus_repositoryProvider,
    getRepositorySyncStatus_syncType,

    -- * Destructuring the Response
    GetRepositorySyncStatusResponse (..),
    newGetRepositorySyncStatusResponse,

    -- * Response Lenses
    getRepositorySyncStatusResponse_latestSync,
    getRepositorySyncStatusResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetRepositorySyncStatus' smart constructor.
data GetRepositorySyncStatus = GetRepositorySyncStatus'
  { -- | The repository branch.
    branch :: Prelude.Text,
    -- | The repository name.
    repositoryName :: Prelude.Text,
    -- | The repository provider.
    repositoryProvider :: RepositoryProvider,
    -- | The repository sync type.
    syncType :: SyncType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRepositorySyncStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'branch', 'getRepositorySyncStatus_branch' - The repository branch.
--
-- 'repositoryName', 'getRepositorySyncStatus_repositoryName' - The repository name.
--
-- 'repositoryProvider', 'getRepositorySyncStatus_repositoryProvider' - The repository provider.
--
-- 'syncType', 'getRepositorySyncStatus_syncType' - The repository sync type.
newGetRepositorySyncStatus ::
  -- | 'branch'
  Prelude.Text ->
  -- | 'repositoryName'
  Prelude.Text ->
  -- | 'repositoryProvider'
  RepositoryProvider ->
  -- | 'syncType'
  SyncType ->
  GetRepositorySyncStatus
newGetRepositorySyncStatus
  pBranch_
  pRepositoryName_
  pRepositoryProvider_
  pSyncType_ =
    GetRepositorySyncStatus'
      { branch = pBranch_,
        repositoryName = pRepositoryName_,
        repositoryProvider = pRepositoryProvider_,
        syncType = pSyncType_
      }

-- | The repository branch.
getRepositorySyncStatus_branch :: Lens.Lens' GetRepositorySyncStatus Prelude.Text
getRepositorySyncStatus_branch = Lens.lens (\GetRepositorySyncStatus' {branch} -> branch) (\s@GetRepositorySyncStatus' {} a -> s {branch = a} :: GetRepositorySyncStatus)

-- | The repository name.
getRepositorySyncStatus_repositoryName :: Lens.Lens' GetRepositorySyncStatus Prelude.Text
getRepositorySyncStatus_repositoryName = Lens.lens (\GetRepositorySyncStatus' {repositoryName} -> repositoryName) (\s@GetRepositorySyncStatus' {} a -> s {repositoryName = a} :: GetRepositorySyncStatus)

-- | The repository provider.
getRepositorySyncStatus_repositoryProvider :: Lens.Lens' GetRepositorySyncStatus RepositoryProvider
getRepositorySyncStatus_repositoryProvider = Lens.lens (\GetRepositorySyncStatus' {repositoryProvider} -> repositoryProvider) (\s@GetRepositorySyncStatus' {} a -> s {repositoryProvider = a} :: GetRepositorySyncStatus)

-- | The repository sync type.
getRepositorySyncStatus_syncType :: Lens.Lens' GetRepositorySyncStatus SyncType
getRepositorySyncStatus_syncType = Lens.lens (\GetRepositorySyncStatus' {syncType} -> syncType) (\s@GetRepositorySyncStatus' {} a -> s {syncType = a} :: GetRepositorySyncStatus)

instance Core.AWSRequest GetRepositorySyncStatus where
  type
    AWSResponse GetRepositorySyncStatus =
      GetRepositorySyncStatusResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRepositorySyncStatusResponse'
            Prelude.<$> (x Data..?> "latestSync")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetRepositorySyncStatus where
  hashWithSalt _salt GetRepositorySyncStatus' {..} =
    _salt `Prelude.hashWithSalt` branch
      `Prelude.hashWithSalt` repositoryName
      `Prelude.hashWithSalt` repositoryProvider
      `Prelude.hashWithSalt` syncType

instance Prelude.NFData GetRepositorySyncStatus where
  rnf GetRepositorySyncStatus' {..} =
    Prelude.rnf branch
      `Prelude.seq` Prelude.rnf repositoryName
      `Prelude.seq` Prelude.rnf repositoryProvider
      `Prelude.seq` Prelude.rnf syncType

instance Data.ToHeaders GetRepositorySyncStatus where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AwsProton20200720.GetRepositorySyncStatus" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetRepositorySyncStatus where
  toJSON GetRepositorySyncStatus' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("branch" Data..= branch),
            Prelude.Just
              ("repositoryName" Data..= repositoryName),
            Prelude.Just
              ("repositoryProvider" Data..= repositoryProvider),
            Prelude.Just ("syncType" Data..= syncType)
          ]
      )

instance Data.ToPath GetRepositorySyncStatus where
  toPath = Prelude.const "/"

instance Data.ToQuery GetRepositorySyncStatus where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetRepositorySyncStatusResponse' smart constructor.
data GetRepositorySyncStatusResponse = GetRepositorySyncStatusResponse'
  { -- | The repository sync status detail data that\'s returned by Proton.
    latestSync :: Prelude.Maybe RepositorySyncAttempt,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRepositorySyncStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'latestSync', 'getRepositorySyncStatusResponse_latestSync' - The repository sync status detail data that\'s returned by Proton.
--
-- 'httpStatus', 'getRepositorySyncStatusResponse_httpStatus' - The response's http status code.
newGetRepositorySyncStatusResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetRepositorySyncStatusResponse
newGetRepositorySyncStatusResponse pHttpStatus_ =
  GetRepositorySyncStatusResponse'
    { latestSync =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The repository sync status detail data that\'s returned by Proton.
getRepositorySyncStatusResponse_latestSync :: Lens.Lens' GetRepositorySyncStatusResponse (Prelude.Maybe RepositorySyncAttempt)
getRepositorySyncStatusResponse_latestSync = Lens.lens (\GetRepositorySyncStatusResponse' {latestSync} -> latestSync) (\s@GetRepositorySyncStatusResponse' {} a -> s {latestSync = a} :: GetRepositorySyncStatusResponse)

-- | The response's http status code.
getRepositorySyncStatusResponse_httpStatus :: Lens.Lens' GetRepositorySyncStatusResponse Prelude.Int
getRepositorySyncStatusResponse_httpStatus = Lens.lens (\GetRepositorySyncStatusResponse' {httpStatus} -> httpStatus) (\s@GetRepositorySyncStatusResponse' {} a -> s {httpStatus = a} :: GetRepositorySyncStatusResponse)

instance
  Prelude.NFData
    GetRepositorySyncStatusResponse
  where
  rnf GetRepositorySyncStatusResponse' {..} =
    Prelude.rnf latestSync
      `Prelude.seq` Prelude.rnf httpStatus
