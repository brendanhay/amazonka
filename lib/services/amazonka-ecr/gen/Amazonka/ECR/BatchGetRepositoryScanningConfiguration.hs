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
-- Module      : Amazonka.ECR.BatchGetRepositoryScanningConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the scanning configuration for one or more repositories.
module Amazonka.ECR.BatchGetRepositoryScanningConfiguration
  ( -- * Creating a Request
    BatchGetRepositoryScanningConfiguration (..),
    newBatchGetRepositoryScanningConfiguration,

    -- * Request Lenses
    batchGetRepositoryScanningConfiguration_repositoryNames,

    -- * Destructuring the Response
    BatchGetRepositoryScanningConfigurationResponse (..),
    newBatchGetRepositoryScanningConfigurationResponse,

    -- * Response Lenses
    batchGetRepositoryScanningConfigurationResponse_scanningConfigurations,
    batchGetRepositoryScanningConfigurationResponse_failures,
    batchGetRepositoryScanningConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.ECR.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchGetRepositoryScanningConfiguration' smart constructor.
data BatchGetRepositoryScanningConfiguration = BatchGetRepositoryScanningConfiguration'
  { -- | One or more repository names to get the scanning configuration for.
    repositoryNames :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetRepositoryScanningConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'repositoryNames', 'batchGetRepositoryScanningConfiguration_repositoryNames' - One or more repository names to get the scanning configuration for.
newBatchGetRepositoryScanningConfiguration ::
  -- | 'repositoryNames'
  Prelude.NonEmpty Prelude.Text ->
  BatchGetRepositoryScanningConfiguration
newBatchGetRepositoryScanningConfiguration
  pRepositoryNames_ =
    BatchGetRepositoryScanningConfiguration'
      { repositoryNames =
          Lens.coerced
            Lens.# pRepositoryNames_
      }

-- | One or more repository names to get the scanning configuration for.
batchGetRepositoryScanningConfiguration_repositoryNames :: Lens.Lens' BatchGetRepositoryScanningConfiguration (Prelude.NonEmpty Prelude.Text)
batchGetRepositoryScanningConfiguration_repositoryNames = Lens.lens (\BatchGetRepositoryScanningConfiguration' {repositoryNames} -> repositoryNames) (\s@BatchGetRepositoryScanningConfiguration' {} a -> s {repositoryNames = a} :: BatchGetRepositoryScanningConfiguration) Prelude.. Lens.coerced

instance
  Core.AWSRequest
    BatchGetRepositoryScanningConfiguration
  where
  type
    AWSResponse
      BatchGetRepositoryScanningConfiguration =
      BatchGetRepositoryScanningConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchGetRepositoryScanningConfigurationResponse'
            Prelude.<$> ( x Core..?> "scanningConfigurations"
                            Core..!@ Prelude.mempty
                        )
              Prelude.<*> (x Core..?> "failures" Core..!@ Prelude.mempty)
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    BatchGetRepositoryScanningConfiguration
  where
  hashWithSalt
    _salt
    BatchGetRepositoryScanningConfiguration' {..} =
      _salt `Prelude.hashWithSalt` repositoryNames

instance
  Prelude.NFData
    BatchGetRepositoryScanningConfiguration
  where
  rnf BatchGetRepositoryScanningConfiguration' {..} =
    Prelude.rnf repositoryNames

instance
  Core.ToHeaders
    BatchGetRepositoryScanningConfiguration
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonEC2ContainerRegistry_V20150921.BatchGetRepositoryScanningConfiguration" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Core.ToJSON
    BatchGetRepositoryScanningConfiguration
  where
  toJSON BatchGetRepositoryScanningConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("repositoryNames" Core..= repositoryNames)
          ]
      )

instance
  Core.ToPath
    BatchGetRepositoryScanningConfiguration
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    BatchGetRepositoryScanningConfiguration
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchGetRepositoryScanningConfigurationResponse' smart constructor.
data BatchGetRepositoryScanningConfigurationResponse = BatchGetRepositoryScanningConfigurationResponse'
  { -- | The scanning configuration for the requested repositories.
    scanningConfigurations :: Prelude.Maybe [RepositoryScanningConfiguration],
    -- | Any failures associated with the call.
    failures :: Prelude.Maybe [RepositoryScanningConfigurationFailure],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetRepositoryScanningConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'scanningConfigurations', 'batchGetRepositoryScanningConfigurationResponse_scanningConfigurations' - The scanning configuration for the requested repositories.
--
-- 'failures', 'batchGetRepositoryScanningConfigurationResponse_failures' - Any failures associated with the call.
--
-- 'httpStatus', 'batchGetRepositoryScanningConfigurationResponse_httpStatus' - The response's http status code.
newBatchGetRepositoryScanningConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchGetRepositoryScanningConfigurationResponse
newBatchGetRepositoryScanningConfigurationResponse
  pHttpStatus_ =
    BatchGetRepositoryScanningConfigurationResponse'
      { scanningConfigurations =
          Prelude.Nothing,
        failures = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The scanning configuration for the requested repositories.
batchGetRepositoryScanningConfigurationResponse_scanningConfigurations :: Lens.Lens' BatchGetRepositoryScanningConfigurationResponse (Prelude.Maybe [RepositoryScanningConfiguration])
batchGetRepositoryScanningConfigurationResponse_scanningConfigurations = Lens.lens (\BatchGetRepositoryScanningConfigurationResponse' {scanningConfigurations} -> scanningConfigurations) (\s@BatchGetRepositoryScanningConfigurationResponse' {} a -> s {scanningConfigurations = a} :: BatchGetRepositoryScanningConfigurationResponse) Prelude.. Lens.mapping Lens.coerced

-- | Any failures associated with the call.
batchGetRepositoryScanningConfigurationResponse_failures :: Lens.Lens' BatchGetRepositoryScanningConfigurationResponse (Prelude.Maybe [RepositoryScanningConfigurationFailure])
batchGetRepositoryScanningConfigurationResponse_failures = Lens.lens (\BatchGetRepositoryScanningConfigurationResponse' {failures} -> failures) (\s@BatchGetRepositoryScanningConfigurationResponse' {} a -> s {failures = a} :: BatchGetRepositoryScanningConfigurationResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchGetRepositoryScanningConfigurationResponse_httpStatus :: Lens.Lens' BatchGetRepositoryScanningConfigurationResponse Prelude.Int
batchGetRepositoryScanningConfigurationResponse_httpStatus = Lens.lens (\BatchGetRepositoryScanningConfigurationResponse' {httpStatus} -> httpStatus) (\s@BatchGetRepositoryScanningConfigurationResponse' {} a -> s {httpStatus = a} :: BatchGetRepositoryScanningConfigurationResponse)

instance
  Prelude.NFData
    BatchGetRepositoryScanningConfigurationResponse
  where
  rnf
    BatchGetRepositoryScanningConfigurationResponse' {..} =
      Prelude.rnf scanningConfigurations
        `Prelude.seq` Prelude.rnf failures
        `Prelude.seq` Prelude.rnf httpStatus
