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
-- Module      : Network.AWS.CloudFront.UpdateDistribution
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the configuration for a web distribution.
--
-- When you update a distribution, there are more required fields than when
-- you create a distribution. When you update your distribution by using
-- this API action, follow the steps here to get the current configuration
-- and then make your updates, to make sure that you include all of the
-- required fields. To view a summary, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-overview-required-fields.html Required Fields for Create Distribution and Update Distribution>
-- in the /Amazon CloudFront Developer Guide/.
--
-- The update process includes getting the current distribution
-- configuration, updating the XML document that is returned to make your
-- changes, and then submitting an @UpdateDistribution@ request to make the
-- updates.
--
-- For information about updating a distribution using the CloudFront
-- console instead, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-web-creating-console.html Creating a Distribution>
-- in the /Amazon CloudFront Developer Guide/.
--
-- __To update a web distribution using the CloudFront API__
--
-- 1.  Submit a
--     <https://docs.aws.amazon.com/cloudfront/latest/APIReference/API_GetDistributionConfig.html GetDistributionConfig>
--     request to get the current configuration and an @Etag@ header for
--     the distribution.
--
--     If you update the distribution again, you must get a new @Etag@
--     header.
--
-- 2.  Update the XML document that was returned in the response to your
--     @GetDistributionConfig@ request to include your changes.
--
--     When you edit the XML file, be aware of the following:
--
--     -   You must strip out the ETag parameter that is returned.
--
--     -   Additional fields are required when you update a distribution.
--         There may be fields included in the XML file for features that
--         you haven\'t configured for your distribution. This is expected
--         and required to successfully update the distribution.
--
--     -   You can\'t change the value of @CallerReference@. If you try to
--         change this value, CloudFront returns an @IllegalUpdate@ error.
--
--     -   The new configuration replaces the existing configuration; the
--         values that you specify in an @UpdateDistribution@ request are
--         not merged into your existing configuration. When you add,
--         delete, or replace values in an element that allows multiple
--         values (for example, @CNAME@), you must specify all of the
--         values that you want to appear in the updated distribution. In
--         addition, you must update the corresponding @Quantity@ element.
--
-- 3.  Submit an @UpdateDistribution@ request to update the configuration
--     for your distribution:
--
--     -   In the request body, include the XML document that you updated
--         in Step 2. The request body must include an XML document with a
--         @DistributionConfig@ element.
--
--     -   Set the value of the HTTP @If-Match@ header to the value of the
--         @ETag@ header that CloudFront returned when you submitted the
--         @GetDistributionConfig@ request in Step 1.
--
-- 4.  Review the response to the @UpdateDistribution@ request to confirm
--     that the configuration was successfully updated.
--
-- 5.  Optional: Submit a
--     <https://docs.aws.amazon.com/cloudfront/latest/APIReference/API_GetDistribution.html GetDistribution>
--     request to confirm that your changes have propagated. When
--     propagation is complete, the value of @Status@ is @Deployed@.
module Network.AWS.CloudFront.UpdateDistribution
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
    updateDistributionResponse_eTag,
    updateDistributionResponse_distribution,
    updateDistributionResponse_httpStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  request = Request.putXML defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          UpdateDistributionResponse'
            Prelude.<$> (h Core..#? "ETag")
            Prelude.<*> (Core.parseXML x)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateDistribution

instance Prelude.NFData UpdateDistribution

instance Core.ToElement UpdateDistribution where
  toElement UpdateDistribution' {..} =
    Core.mkElement
      "{http://cloudfront.amazonaws.com/doc/2020-05-31/}DistributionConfig"
      distributionConfig

instance Core.ToHeaders UpdateDistribution where
  toHeaders UpdateDistribution' {..} =
    Prelude.mconcat ["If-Match" Core.=# ifMatch]

instance Core.ToPath UpdateDistribution where
  toPath UpdateDistribution' {..} =
    Prelude.mconcat
      [ "/2020-05-31/distribution/",
        Core.toBS id,
        "/config"
      ]

instance Core.ToQuery UpdateDistribution where
  toQuery = Prelude.const Prelude.mempty

-- | The returned result of the corresponding request.
--
-- /See:/ 'newUpdateDistributionResponse' smart constructor.
data UpdateDistributionResponse = UpdateDistributionResponse'
  { -- | The current version of the configuration. For example: @E2QWRUHAPOMQZL@.
    eTag :: Prelude.Maybe Prelude.Text,
    -- | The distribution\'s information.
    distribution :: Prelude.Maybe Distribution,
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
-- 'eTag', 'updateDistributionResponse_eTag' - The current version of the configuration. For example: @E2QWRUHAPOMQZL@.
--
-- 'distribution', 'updateDistributionResponse_distribution' - The distribution\'s information.
--
-- 'httpStatus', 'updateDistributionResponse_httpStatus' - The response's http status code.
newUpdateDistributionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateDistributionResponse
newUpdateDistributionResponse pHttpStatus_ =
  UpdateDistributionResponse'
    { eTag = Prelude.Nothing,
      distribution = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current version of the configuration. For example: @E2QWRUHAPOMQZL@.
updateDistributionResponse_eTag :: Lens.Lens' UpdateDistributionResponse (Prelude.Maybe Prelude.Text)
updateDistributionResponse_eTag = Lens.lens (\UpdateDistributionResponse' {eTag} -> eTag) (\s@UpdateDistributionResponse' {} a -> s {eTag = a} :: UpdateDistributionResponse)

-- | The distribution\'s information.
updateDistributionResponse_distribution :: Lens.Lens' UpdateDistributionResponse (Prelude.Maybe Distribution)
updateDistributionResponse_distribution = Lens.lens (\UpdateDistributionResponse' {distribution} -> distribution) (\s@UpdateDistributionResponse' {} a -> s {distribution = a} :: UpdateDistributionResponse)

-- | The response's http status code.
updateDistributionResponse_httpStatus :: Lens.Lens' UpdateDistributionResponse Prelude.Int
updateDistributionResponse_httpStatus = Lens.lens (\UpdateDistributionResponse' {httpStatus} -> httpStatus) (\s@UpdateDistributionResponse' {} a -> s {httpStatus = a} :: UpdateDistributionResponse)

instance Prelude.NFData UpdateDistributionResponse
