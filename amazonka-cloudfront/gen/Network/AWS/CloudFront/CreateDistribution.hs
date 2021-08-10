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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The request to create a new distribution.
--
-- /See:/ 'newCreateDistribution' smart constructor.
data CreateDistribution = CreateDistribution'
  { -- | The distribution\'s configuration information.
    distributionConfig :: DistributionConfig
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

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
            Prelude.<$> (h Core..#? "ETag")
            Prelude.<*> (Core.parseXML x)
            Prelude.<*> (h Core..#? "Location")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateDistribution

instance Prelude.NFData CreateDistribution

instance Core.ToElement CreateDistribution where
  toElement CreateDistribution' {..} =
    Core.mkElement
      "{http://cloudfront.amazonaws.com/doc/2020-05-31/}DistributionConfig"
      distributionConfig

instance Core.ToHeaders CreateDistribution where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath CreateDistribution where
  toPath = Prelude.const "/2020-05-31/distribution"

instance Core.ToQuery CreateDistribution where
  toQuery = Prelude.const Prelude.mempty

-- | The returned result of the corresponding request.
--
-- /See:/ 'newCreateDistributionResponse' smart constructor.
data CreateDistributionResponse = CreateDistributionResponse'
  { -- | The current version of the distribution created.
    eTag :: Prelude.Maybe Prelude.Text,
    -- | The distribution\'s information.
    distribution :: Prelude.Maybe Distribution,
    -- | The fully qualified URI of the new distribution resource just created.
    location :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  CreateDistributionResponse
newCreateDistributionResponse pHttpStatus_ =
  CreateDistributionResponse'
    { eTag = Prelude.Nothing,
      distribution = Prelude.Nothing,
      location = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current version of the distribution created.
createDistributionResponse_eTag :: Lens.Lens' CreateDistributionResponse (Prelude.Maybe Prelude.Text)
createDistributionResponse_eTag = Lens.lens (\CreateDistributionResponse' {eTag} -> eTag) (\s@CreateDistributionResponse' {} a -> s {eTag = a} :: CreateDistributionResponse)

-- | The distribution\'s information.
createDistributionResponse_distribution :: Lens.Lens' CreateDistributionResponse (Prelude.Maybe Distribution)
createDistributionResponse_distribution = Lens.lens (\CreateDistributionResponse' {distribution} -> distribution) (\s@CreateDistributionResponse' {} a -> s {distribution = a} :: CreateDistributionResponse)

-- | The fully qualified URI of the new distribution resource just created.
createDistributionResponse_location :: Lens.Lens' CreateDistributionResponse (Prelude.Maybe Prelude.Text)
createDistributionResponse_location = Lens.lens (\CreateDistributionResponse' {location} -> location) (\s@CreateDistributionResponse' {} a -> s {location = a} :: CreateDistributionResponse)

-- | The response's http status code.
createDistributionResponse_httpStatus :: Lens.Lens' CreateDistributionResponse Prelude.Int
createDistributionResponse_httpStatus = Lens.lens (\CreateDistributionResponse' {httpStatus} -> httpStatus) (\s@CreateDistributionResponse' {} a -> s {httpStatus = a} :: CreateDistributionResponse)

instance Prelude.NFData CreateDistributionResponse
