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
-- Module      : Amazonka.CloudFront.UpdateDistributionWithStagingConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Copies the staging distribution\'s configuration to its corresponding
-- primary distribution. The primary distribution retains its @Aliases@
-- (also known as alternate domain names or CNAMEs) and
-- @ContinuousDeploymentPolicyId@ value, but otherwise its configuration is
-- overwritten to match the staging distribution.
--
-- You can use this operation in a continuous deployment workflow after you
-- have tested configuration changes on the staging distribution. After
-- using a continuous deployment policy to move a portion of your domain
-- name\'s traffic to the staging distribution and verifying that it works
-- as intended, you can use this operation to copy the staging
-- distribution\'s configuration to the primary distribution. This action
-- will disable the continuous deployment policy and move your domain\'s
-- traffic back to the primary distribution.
module Amazonka.CloudFront.UpdateDistributionWithStagingConfig
  ( -- * Creating a Request
    UpdateDistributionWithStagingConfig (..),
    newUpdateDistributionWithStagingConfig,

    -- * Request Lenses
    updateDistributionWithStagingConfig_ifMatch,
    updateDistributionWithStagingConfig_stagingDistributionId,
    updateDistributionWithStagingConfig_id,

    -- * Destructuring the Response
    UpdateDistributionWithStagingConfigResponse (..),
    newUpdateDistributionWithStagingConfigResponse,

    -- * Response Lenses
    updateDistributionWithStagingConfigResponse_distribution,
    updateDistributionWithStagingConfigResponse_eTag,
    updateDistributionWithStagingConfigResponse_httpStatus,
  )
where

import Amazonka.CloudFront.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateDistributionWithStagingConfig' smart constructor.
data UpdateDistributionWithStagingConfig = UpdateDistributionWithStagingConfig'
  { -- | The current versions (@ETag@ values) of both primary and staging
    -- distributions. Provide these in the following format:
    --
    -- @\<primary ETag>, \<staging ETag>@
    ifMatch :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the staging distribution whose configuration you are
    -- copying to the primary distribution.
    stagingDistributionId :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the primary distribution to which you are copying a
    -- staging distribution\'s configuration.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDistributionWithStagingConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ifMatch', 'updateDistributionWithStagingConfig_ifMatch' - The current versions (@ETag@ values) of both primary and staging
-- distributions. Provide these in the following format:
--
-- @\<primary ETag>, \<staging ETag>@
--
-- 'stagingDistributionId', 'updateDistributionWithStagingConfig_stagingDistributionId' - The identifier of the staging distribution whose configuration you are
-- copying to the primary distribution.
--
-- 'id', 'updateDistributionWithStagingConfig_id' - The identifier of the primary distribution to which you are copying a
-- staging distribution\'s configuration.
newUpdateDistributionWithStagingConfig ::
  -- | 'id'
  Prelude.Text ->
  UpdateDistributionWithStagingConfig
newUpdateDistributionWithStagingConfig pId_ =
  UpdateDistributionWithStagingConfig'
    { ifMatch =
        Prelude.Nothing,
      stagingDistributionId =
        Prelude.Nothing,
      id = pId_
    }

-- | The current versions (@ETag@ values) of both primary and staging
-- distributions. Provide these in the following format:
--
-- @\<primary ETag>, \<staging ETag>@
updateDistributionWithStagingConfig_ifMatch :: Lens.Lens' UpdateDistributionWithStagingConfig (Prelude.Maybe Prelude.Text)
updateDistributionWithStagingConfig_ifMatch = Lens.lens (\UpdateDistributionWithStagingConfig' {ifMatch} -> ifMatch) (\s@UpdateDistributionWithStagingConfig' {} a -> s {ifMatch = a} :: UpdateDistributionWithStagingConfig)

-- | The identifier of the staging distribution whose configuration you are
-- copying to the primary distribution.
updateDistributionWithStagingConfig_stagingDistributionId :: Lens.Lens' UpdateDistributionWithStagingConfig (Prelude.Maybe Prelude.Text)
updateDistributionWithStagingConfig_stagingDistributionId = Lens.lens (\UpdateDistributionWithStagingConfig' {stagingDistributionId} -> stagingDistributionId) (\s@UpdateDistributionWithStagingConfig' {} a -> s {stagingDistributionId = a} :: UpdateDistributionWithStagingConfig)

-- | The identifier of the primary distribution to which you are copying a
-- staging distribution\'s configuration.
updateDistributionWithStagingConfig_id :: Lens.Lens' UpdateDistributionWithStagingConfig Prelude.Text
updateDistributionWithStagingConfig_id = Lens.lens (\UpdateDistributionWithStagingConfig' {id} -> id) (\s@UpdateDistributionWithStagingConfig' {} a -> s {id = a} :: UpdateDistributionWithStagingConfig)

instance
  Core.AWSRequest
    UpdateDistributionWithStagingConfig
  where
  type
    AWSResponse UpdateDistributionWithStagingConfig =
      UpdateDistributionWithStagingConfigResponse
  request overrides =
    Request.put (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          UpdateDistributionWithStagingConfigResponse'
            Prelude.<$> (Data.parseXML x)
            Prelude.<*> (h Data..#? "ETag")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateDistributionWithStagingConfig
  where
  hashWithSalt
    _salt
    UpdateDistributionWithStagingConfig' {..} =
      _salt
        `Prelude.hashWithSalt` ifMatch
        `Prelude.hashWithSalt` stagingDistributionId
        `Prelude.hashWithSalt` id

instance
  Prelude.NFData
    UpdateDistributionWithStagingConfig
  where
  rnf UpdateDistributionWithStagingConfig' {..} =
    Prelude.rnf ifMatch
      `Prelude.seq` Prelude.rnf stagingDistributionId
      `Prelude.seq` Prelude.rnf id

instance
  Data.ToHeaders
    UpdateDistributionWithStagingConfig
  where
  toHeaders UpdateDistributionWithStagingConfig' {..} =
    Prelude.mconcat ["If-Match" Data.=# ifMatch]

instance
  Data.ToPath
    UpdateDistributionWithStagingConfig
  where
  toPath UpdateDistributionWithStagingConfig' {..} =
    Prelude.mconcat
      [ "/2020-05-31/distribution/",
        Data.toBS id,
        "/promote-staging-config"
      ]

instance
  Data.ToQuery
    UpdateDistributionWithStagingConfig
  where
  toQuery UpdateDistributionWithStagingConfig' {..} =
    Prelude.mconcat
      [ "StagingDistributionId"
          Data.=: stagingDistributionId
      ]

-- | /See:/ 'newUpdateDistributionWithStagingConfigResponse' smart constructor.
data UpdateDistributionWithStagingConfigResponse = UpdateDistributionWithStagingConfigResponse'
  { distribution :: Prelude.Maybe Distribution,
    -- | The current version of the primary distribution (after it\'s updated).
    eTag :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDistributionWithStagingConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'distribution', 'updateDistributionWithStagingConfigResponse_distribution' - Undocumented member.
--
-- 'eTag', 'updateDistributionWithStagingConfigResponse_eTag' - The current version of the primary distribution (after it\'s updated).
--
-- 'httpStatus', 'updateDistributionWithStagingConfigResponse_httpStatus' - The response's http status code.
newUpdateDistributionWithStagingConfigResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateDistributionWithStagingConfigResponse
newUpdateDistributionWithStagingConfigResponse
  pHttpStatus_ =
    UpdateDistributionWithStagingConfigResponse'
      { distribution =
          Prelude.Nothing,
        eTag = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Undocumented member.
updateDistributionWithStagingConfigResponse_distribution :: Lens.Lens' UpdateDistributionWithStagingConfigResponse (Prelude.Maybe Distribution)
updateDistributionWithStagingConfigResponse_distribution = Lens.lens (\UpdateDistributionWithStagingConfigResponse' {distribution} -> distribution) (\s@UpdateDistributionWithStagingConfigResponse' {} a -> s {distribution = a} :: UpdateDistributionWithStagingConfigResponse)

-- | The current version of the primary distribution (after it\'s updated).
updateDistributionWithStagingConfigResponse_eTag :: Lens.Lens' UpdateDistributionWithStagingConfigResponse (Prelude.Maybe Prelude.Text)
updateDistributionWithStagingConfigResponse_eTag = Lens.lens (\UpdateDistributionWithStagingConfigResponse' {eTag} -> eTag) (\s@UpdateDistributionWithStagingConfigResponse' {} a -> s {eTag = a} :: UpdateDistributionWithStagingConfigResponse)

-- | The response's http status code.
updateDistributionWithStagingConfigResponse_httpStatus :: Lens.Lens' UpdateDistributionWithStagingConfigResponse Prelude.Int
updateDistributionWithStagingConfigResponse_httpStatus = Lens.lens (\UpdateDistributionWithStagingConfigResponse' {httpStatus} -> httpStatus) (\s@UpdateDistributionWithStagingConfigResponse' {} a -> s {httpStatus = a} :: UpdateDistributionWithStagingConfigResponse)

instance
  Prelude.NFData
    UpdateDistributionWithStagingConfigResponse
  where
  rnf UpdateDistributionWithStagingConfigResponse' {..} =
    Prelude.rnf distribution
      `Prelude.seq` Prelude.rnf eTag
      `Prelude.seq` Prelude.rnf httpStatus
