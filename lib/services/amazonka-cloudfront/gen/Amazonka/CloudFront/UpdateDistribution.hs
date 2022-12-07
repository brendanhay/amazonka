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
-- Module      : Amazonka.CloudFront.UpdateDistribution
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the configuration for a CloudFront distribution.
--
-- The update process includes getting the current distribution
-- configuration, updating it to make your changes, and then submitting an
-- @UpdateDistribution@ request to make the updates.
--
-- __To update a web distribution using the CloudFront API__
--
-- 1.  Use @GetDistributionConfig@ to get the current configuration,
--     including the version identifier (@ETag@).
--
-- 2.  Update the distribution configuration that was returned in the
--     response. Note the following important requirements and
--     restrictions:
--
--     -   You must rename the @ETag@ field to @IfMatch@, leaving the value
--         unchanged. (Set the value of @IfMatch@ to the value of @ETag@,
--         then remove the @ETag@ field.)
--
--     -   You can’t change the value of @CallerReference@.
--
-- 3.  Submit an @UpdateDistribution@ request, providing the distribution
--     configuration. The new configuration replaces the existing
--     configuration. The values that you specify in an
--     @UpdateDistribution@ request are not merged into your existing
--     configuration. Make sure to include all fields: the ones that you
--     modified and also the ones that you didn’t.
module Amazonka.CloudFront.UpdateDistribution
  ( -- * Creating a Request
    UpdateDistribution (..),
    newUpdateDistribution,

    -- * Request Lenses
    updateDistribution_ifMatch,
    updateDistribution_distributionConfig,
    updateDistribution_id,

    -- * Destructuring the Response
    UpdateDistributionResponse (..),
    newUpdateDistributionResponse,

    -- * Response Lenses
    updateDistributionResponse_distribution,
    updateDistributionResponse_eTag,
    updateDistributionResponse_httpStatus,
  )
where

import Amazonka.CloudFront.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The request to update a distribution.
--
-- /See:/ 'newUpdateDistribution' smart constructor.
data UpdateDistribution = UpdateDistribution'
  { -- | The value of the @ETag@ header that you received when retrieving the
    -- distribution\'s configuration. For example: @E2QWRUHAPOMQZL@.
    ifMatch :: Prelude.Maybe Prelude.Text,
    -- | The distribution\'s configuration information.
    distributionConfig :: DistributionConfig,
    -- | The distribution\'s id.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDistribution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ifMatch', 'updateDistribution_ifMatch' - The value of the @ETag@ header that you received when retrieving the
-- distribution\'s configuration. For example: @E2QWRUHAPOMQZL@.
--
-- 'distributionConfig', 'updateDistribution_distributionConfig' - The distribution\'s configuration information.
--
-- 'id', 'updateDistribution_id' - The distribution\'s id.
newUpdateDistribution ::
  -- | 'distributionConfig'
  DistributionConfig ->
  -- | 'id'
  Prelude.Text ->
  UpdateDistribution
newUpdateDistribution pDistributionConfig_ pId_ =
  UpdateDistribution'
    { ifMatch = Prelude.Nothing,
      distributionConfig = pDistributionConfig_,
      id = pId_
    }

-- | The value of the @ETag@ header that you received when retrieving the
-- distribution\'s configuration. For example: @E2QWRUHAPOMQZL@.
updateDistribution_ifMatch :: Lens.Lens' UpdateDistribution (Prelude.Maybe Prelude.Text)
updateDistribution_ifMatch = Lens.lens (\UpdateDistribution' {ifMatch} -> ifMatch) (\s@UpdateDistribution' {} a -> s {ifMatch = a} :: UpdateDistribution)

-- | The distribution\'s configuration information.
updateDistribution_distributionConfig :: Lens.Lens' UpdateDistribution DistributionConfig
updateDistribution_distributionConfig = Lens.lens (\UpdateDistribution' {distributionConfig} -> distributionConfig) (\s@UpdateDistribution' {} a -> s {distributionConfig = a} :: UpdateDistribution)

-- | The distribution\'s id.
updateDistribution_id :: Lens.Lens' UpdateDistribution Prelude.Text
updateDistribution_id = Lens.lens (\UpdateDistribution' {id} -> id) (\s@UpdateDistribution' {} a -> s {id = a} :: UpdateDistribution)

instance Core.AWSRequest UpdateDistribution where
  type
    AWSResponse UpdateDistribution =
      UpdateDistributionResponse
  request overrides =
    Request.putXML (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          UpdateDistributionResponse'
            Prelude.<$> (Data.parseXML x)
            Prelude.<*> (h Data..#? "ETag")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateDistribution where
  hashWithSalt _salt UpdateDistribution' {..} =
    _salt `Prelude.hashWithSalt` ifMatch
      `Prelude.hashWithSalt` distributionConfig
      `Prelude.hashWithSalt` id

instance Prelude.NFData UpdateDistribution where
  rnf UpdateDistribution' {..} =
    Prelude.rnf ifMatch
      `Prelude.seq` Prelude.rnf distributionConfig
      `Prelude.seq` Prelude.rnf id

instance Data.ToElement UpdateDistribution where
  toElement UpdateDistribution' {..} =
    Data.mkElement
      "{http://cloudfront.amazonaws.com/doc/2020-05-31/}DistributionConfig"
      distributionConfig

instance Data.ToHeaders UpdateDistribution where
  toHeaders UpdateDistribution' {..} =
    Prelude.mconcat ["If-Match" Data.=# ifMatch]

instance Data.ToPath UpdateDistribution where
  toPath UpdateDistribution' {..} =
    Prelude.mconcat
      [ "/2020-05-31/distribution/",
        Data.toBS id,
        "/config"
      ]

instance Data.ToQuery UpdateDistribution where
  toQuery = Prelude.const Prelude.mempty

-- | The returned result of the corresponding request.
--
-- /See:/ 'newUpdateDistributionResponse' smart constructor.
data UpdateDistributionResponse = UpdateDistributionResponse'
  { -- | The distribution\'s information.
    distribution :: Prelude.Maybe Distribution,
    -- | The current version of the configuration. For example: @E2QWRUHAPOMQZL@.
    eTag :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDistributionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'distribution', 'updateDistributionResponse_distribution' - The distribution\'s information.
--
-- 'eTag', 'updateDistributionResponse_eTag' - The current version of the configuration. For example: @E2QWRUHAPOMQZL@.
--
-- 'httpStatus', 'updateDistributionResponse_httpStatus' - The response's http status code.
newUpdateDistributionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateDistributionResponse
newUpdateDistributionResponse pHttpStatus_ =
  UpdateDistributionResponse'
    { distribution =
        Prelude.Nothing,
      eTag = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The distribution\'s information.
updateDistributionResponse_distribution :: Lens.Lens' UpdateDistributionResponse (Prelude.Maybe Distribution)
updateDistributionResponse_distribution = Lens.lens (\UpdateDistributionResponse' {distribution} -> distribution) (\s@UpdateDistributionResponse' {} a -> s {distribution = a} :: UpdateDistributionResponse)

-- | The current version of the configuration. For example: @E2QWRUHAPOMQZL@.
updateDistributionResponse_eTag :: Lens.Lens' UpdateDistributionResponse (Prelude.Maybe Prelude.Text)
updateDistributionResponse_eTag = Lens.lens (\UpdateDistributionResponse' {eTag} -> eTag) (\s@UpdateDistributionResponse' {} a -> s {eTag = a} :: UpdateDistributionResponse)

-- | The response's http status code.
updateDistributionResponse_httpStatus :: Lens.Lens' UpdateDistributionResponse Prelude.Int
updateDistributionResponse_httpStatus = Lens.lens (\UpdateDistributionResponse' {httpStatus} -> httpStatus) (\s@UpdateDistributionResponse' {} a -> s {httpStatus = a} :: UpdateDistributionResponse)

instance Prelude.NFData UpdateDistributionResponse where
  rnf UpdateDistributionResponse' {..} =
    Prelude.rnf distribution
      `Prelude.seq` Prelude.rnf eTag
      `Prelude.seq` Prelude.rnf httpStatus
