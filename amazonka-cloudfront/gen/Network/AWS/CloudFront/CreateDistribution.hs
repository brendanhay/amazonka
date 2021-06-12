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
-- Module      : Network.AWS.CloudFront.CreateDistribution
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new web distribution. You create a CloudFront distribution to
-- tell CloudFront where you want content to be delivered from, and the
-- details about how to track and manage content delivery. Send a @POST@
-- request to the
-- @\/CloudFront API version\/distribution@\/@distribution ID@ resource.
--
-- When you update a distribution, there are more required fields than when
-- you create a distribution. When you update your distribution by using
-- <https://docs.aws.amazon.com/cloudfront/latest/APIReference/API_UpdateDistribution.html UpdateDistribution>,
-- follow the steps included in the documentation to get the current
-- configuration and then make your updates. This helps to make sure that
-- you include all of the required fields. To view a summary, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/distribution-overview-required-fields.html Required Fields for Create Distribution and Update Distribution>
-- in the /Amazon CloudFront Developer Guide/.
module Network.AWS.CloudFront.CreateDistribution
  ( -- * Creating a Request
    CreateDistribution (..),
    newCreateDistribution,

    -- * Request Lenses
    createDistribution_distributionConfig,

    -- * Destructuring the Response
    CreateDistributionResponse (..),
    newCreateDistributionResponse,

    -- * Response Lenses
    createDistributionResponse_eTag,
    createDistributionResponse_distribution,
    createDistributionResponse_location,
    createDistributionResponse_httpStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The request to create a new distribution.
--
-- /See:/ 'newCreateDistribution' smart constructor.
data CreateDistribution = CreateDistribution'
  { -- | The distribution\'s configuration information.
    distributionConfig :: DistributionConfig
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateDistribution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'distributionConfig', 'createDistribution_distributionConfig' - The distribution\'s configuration information.
newCreateDistribution ::
  -- | 'distributionConfig'
  DistributionConfig ->
  CreateDistribution
newCreateDistribution pDistributionConfig_ =
  CreateDistribution'
    { distributionConfig =
        pDistributionConfig_
    }

-- | The distribution\'s configuration information.
createDistribution_distributionConfig :: Lens.Lens' CreateDistribution DistributionConfig
createDistribution_distributionConfig = Lens.lens (\CreateDistribution' {distributionConfig} -> distributionConfig) (\s@CreateDistribution' {} a -> s {distributionConfig = a} :: CreateDistribution)

instance Core.AWSRequest CreateDistribution where
  type
    AWSResponse CreateDistribution =
      CreateDistributionResponse
  request = Request.postXML defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          CreateDistributionResponse'
            Core.<$> (h Core..#? "ETag")
            Core.<*> (Core.parseXML x)
            Core.<*> (h Core..#? "Location")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateDistribution

instance Core.NFData CreateDistribution

instance Core.ToElement CreateDistribution where
  toElement CreateDistribution' {..} =
    Core.mkElement
      "{http://cloudfront.amazonaws.com/doc/2020-05-31/}DistributionConfig"
      distributionConfig

instance Core.ToHeaders CreateDistribution where
  toHeaders = Core.const Core.mempty

instance Core.ToPath CreateDistribution where
  toPath = Core.const "/2020-05-31/distribution"

instance Core.ToQuery CreateDistribution where
  toQuery = Core.const Core.mempty

-- | The returned result of the corresponding request.
--
-- /See:/ 'newCreateDistributionResponse' smart constructor.
data CreateDistributionResponse = CreateDistributionResponse'
  { -- | The current version of the distribution created.
    eTag :: Core.Maybe Core.Text,
    -- | The distribution\'s information.
    distribution :: Core.Maybe Distribution,
    -- | The fully qualified URI of the new distribution resource just created.
    location :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateDistributionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eTag', 'createDistributionResponse_eTag' - The current version of the distribution created.
--
-- 'distribution', 'createDistributionResponse_distribution' - The distribution\'s information.
--
-- 'location', 'createDistributionResponse_location' - The fully qualified URI of the new distribution resource just created.
--
-- 'httpStatus', 'createDistributionResponse_httpStatus' - The response's http status code.
newCreateDistributionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateDistributionResponse
newCreateDistributionResponse pHttpStatus_ =
  CreateDistributionResponse'
    { eTag = Core.Nothing,
      distribution = Core.Nothing,
      location = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current version of the distribution created.
createDistributionResponse_eTag :: Lens.Lens' CreateDistributionResponse (Core.Maybe Core.Text)
createDistributionResponse_eTag = Lens.lens (\CreateDistributionResponse' {eTag} -> eTag) (\s@CreateDistributionResponse' {} a -> s {eTag = a} :: CreateDistributionResponse)

-- | The distribution\'s information.
createDistributionResponse_distribution :: Lens.Lens' CreateDistributionResponse (Core.Maybe Distribution)
createDistributionResponse_distribution = Lens.lens (\CreateDistributionResponse' {distribution} -> distribution) (\s@CreateDistributionResponse' {} a -> s {distribution = a} :: CreateDistributionResponse)

-- | The fully qualified URI of the new distribution resource just created.
createDistributionResponse_location :: Lens.Lens' CreateDistributionResponse (Core.Maybe Core.Text)
createDistributionResponse_location = Lens.lens (\CreateDistributionResponse' {location} -> location) (\s@CreateDistributionResponse' {} a -> s {location = a} :: CreateDistributionResponse)

-- | The response's http status code.
createDistributionResponse_httpStatus :: Lens.Lens' CreateDistributionResponse Core.Int
createDistributionResponse_httpStatus = Lens.lens (\CreateDistributionResponse' {httpStatus} -> httpStatus) (\s@CreateDistributionResponse' {} a -> s {httpStatus = a} :: CreateDistributionResponse)

instance Core.NFData CreateDistributionResponse
