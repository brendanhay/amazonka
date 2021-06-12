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
-- Module      : Network.AWS.CloudFront.CreateCloudFrontOriginAccessIdentity
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new origin access identity. If you\'re using Amazon S3 for
-- your origin, you can use an origin access identity to require users to
-- access your content using a CloudFront URL instead of the Amazon S3 URL.
-- For more information about how to use origin access identities, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/PrivateContent.html Serving Private Content through CloudFront>
-- in the /Amazon CloudFront Developer Guide/.
module Network.AWS.CloudFront.CreateCloudFrontOriginAccessIdentity
  ( -- * Creating a Request
    CreateCloudFrontOriginAccessIdentity (..),
    newCreateCloudFrontOriginAccessIdentity,

    -- * Request Lenses
    createCloudFrontOriginAccessIdentity_cloudFrontOriginAccessIdentityConfig,

    -- * Destructuring the Response
    CreateCloudFrontOriginAccessIdentityResponse (..),
    newCreateCloudFrontOriginAccessIdentityResponse,

    -- * Response Lenses
    createCloudFrontOriginAccessIdentityResponse_eTag,
    createCloudFrontOriginAccessIdentityResponse_cloudFrontOriginAccessIdentity,
    createCloudFrontOriginAccessIdentityResponse_location,
    createCloudFrontOriginAccessIdentityResponse_httpStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The request to create a new origin access identity (OAI). An origin
-- access identity is a special CloudFront user that you can associate with
-- Amazon S3 origins, so that you can secure all or just some of your
-- Amazon S3 content. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/private-content-restricting-access-to-s3.html Restricting Access to Amazon S3 Content by Using an Origin Access Identity>
-- in the /Amazon CloudFront Developer Guide/.
--
-- /See:/ 'newCreateCloudFrontOriginAccessIdentity' smart constructor.
data CreateCloudFrontOriginAccessIdentity = CreateCloudFrontOriginAccessIdentity'
  { -- | The current configuration information for the identity.
    cloudFrontOriginAccessIdentityConfig :: CloudFrontOriginAccessIdentityConfig
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateCloudFrontOriginAccessIdentity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cloudFrontOriginAccessIdentityConfig', 'createCloudFrontOriginAccessIdentity_cloudFrontOriginAccessIdentityConfig' - The current configuration information for the identity.
newCreateCloudFrontOriginAccessIdentity ::
  -- | 'cloudFrontOriginAccessIdentityConfig'
  CloudFrontOriginAccessIdentityConfig ->
  CreateCloudFrontOriginAccessIdentity
newCreateCloudFrontOriginAccessIdentity
  pCloudFrontOriginAccessIdentityConfig_ =
    CreateCloudFrontOriginAccessIdentity'
      { cloudFrontOriginAccessIdentityConfig =
          pCloudFrontOriginAccessIdentityConfig_
      }

-- | The current configuration information for the identity.
createCloudFrontOriginAccessIdentity_cloudFrontOriginAccessIdentityConfig :: Lens.Lens' CreateCloudFrontOriginAccessIdentity CloudFrontOriginAccessIdentityConfig
createCloudFrontOriginAccessIdentity_cloudFrontOriginAccessIdentityConfig = Lens.lens (\CreateCloudFrontOriginAccessIdentity' {cloudFrontOriginAccessIdentityConfig} -> cloudFrontOriginAccessIdentityConfig) (\s@CreateCloudFrontOriginAccessIdentity' {} a -> s {cloudFrontOriginAccessIdentityConfig = a} :: CreateCloudFrontOriginAccessIdentity)

instance
  Core.AWSRequest
    CreateCloudFrontOriginAccessIdentity
  where
  type
    AWSResponse CreateCloudFrontOriginAccessIdentity =
      CreateCloudFrontOriginAccessIdentityResponse
  request = Request.postXML defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          CreateCloudFrontOriginAccessIdentityResponse'
            Core.<$> (h Core..#? "ETag") Core.<*> (Core.parseXML x)
              Core.<*> (h Core..#? "Location")
              Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    CreateCloudFrontOriginAccessIdentity

instance
  Core.NFData
    CreateCloudFrontOriginAccessIdentity

instance
  Core.ToElement
    CreateCloudFrontOriginAccessIdentity
  where
  toElement CreateCloudFrontOriginAccessIdentity' {..} =
    Core.mkElement
      "{http://cloudfront.amazonaws.com/doc/2020-05-31/}CloudFrontOriginAccessIdentityConfig"
      cloudFrontOriginAccessIdentityConfig

instance
  Core.ToHeaders
    CreateCloudFrontOriginAccessIdentity
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToPath
    CreateCloudFrontOriginAccessIdentity
  where
  toPath =
    Core.const
      "/2020-05-31/origin-access-identity/cloudfront"

instance
  Core.ToQuery
    CreateCloudFrontOriginAccessIdentity
  where
  toQuery = Core.const Core.mempty

-- | The returned result of the corresponding request.
--
-- /See:/ 'newCreateCloudFrontOriginAccessIdentityResponse' smart constructor.
data CreateCloudFrontOriginAccessIdentityResponse = CreateCloudFrontOriginAccessIdentityResponse'
  { -- | The current version of the origin access identity created.
    eTag :: Core.Maybe Core.Text,
    -- | The origin access identity\'s information.
    cloudFrontOriginAccessIdentity :: Core.Maybe CloudFrontOriginAccessIdentity,
    -- | The fully qualified URI of the new origin access identity just created.
    location :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateCloudFrontOriginAccessIdentityResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eTag', 'createCloudFrontOriginAccessIdentityResponse_eTag' - The current version of the origin access identity created.
--
-- 'cloudFrontOriginAccessIdentity', 'createCloudFrontOriginAccessIdentityResponse_cloudFrontOriginAccessIdentity' - The origin access identity\'s information.
--
-- 'location', 'createCloudFrontOriginAccessIdentityResponse_location' - The fully qualified URI of the new origin access identity just created.
--
-- 'httpStatus', 'createCloudFrontOriginAccessIdentityResponse_httpStatus' - The response's http status code.
newCreateCloudFrontOriginAccessIdentityResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateCloudFrontOriginAccessIdentityResponse
newCreateCloudFrontOriginAccessIdentityResponse
  pHttpStatus_ =
    CreateCloudFrontOriginAccessIdentityResponse'
      { eTag =
          Core.Nothing,
        cloudFrontOriginAccessIdentity =
          Core.Nothing,
        location = Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The current version of the origin access identity created.
createCloudFrontOriginAccessIdentityResponse_eTag :: Lens.Lens' CreateCloudFrontOriginAccessIdentityResponse (Core.Maybe Core.Text)
createCloudFrontOriginAccessIdentityResponse_eTag = Lens.lens (\CreateCloudFrontOriginAccessIdentityResponse' {eTag} -> eTag) (\s@CreateCloudFrontOriginAccessIdentityResponse' {} a -> s {eTag = a} :: CreateCloudFrontOriginAccessIdentityResponse)

-- | The origin access identity\'s information.
createCloudFrontOriginAccessIdentityResponse_cloudFrontOriginAccessIdentity :: Lens.Lens' CreateCloudFrontOriginAccessIdentityResponse (Core.Maybe CloudFrontOriginAccessIdentity)
createCloudFrontOriginAccessIdentityResponse_cloudFrontOriginAccessIdentity = Lens.lens (\CreateCloudFrontOriginAccessIdentityResponse' {cloudFrontOriginAccessIdentity} -> cloudFrontOriginAccessIdentity) (\s@CreateCloudFrontOriginAccessIdentityResponse' {} a -> s {cloudFrontOriginAccessIdentity = a} :: CreateCloudFrontOriginAccessIdentityResponse)

-- | The fully qualified URI of the new origin access identity just created.
createCloudFrontOriginAccessIdentityResponse_location :: Lens.Lens' CreateCloudFrontOriginAccessIdentityResponse (Core.Maybe Core.Text)
createCloudFrontOriginAccessIdentityResponse_location = Lens.lens (\CreateCloudFrontOriginAccessIdentityResponse' {location} -> location) (\s@CreateCloudFrontOriginAccessIdentityResponse' {} a -> s {location = a} :: CreateCloudFrontOriginAccessIdentityResponse)

-- | The response's http status code.
createCloudFrontOriginAccessIdentityResponse_httpStatus :: Lens.Lens' CreateCloudFrontOriginAccessIdentityResponse Core.Int
createCloudFrontOriginAccessIdentityResponse_httpStatus = Lens.lens (\CreateCloudFrontOriginAccessIdentityResponse' {httpStatus} -> httpStatus) (\s@CreateCloudFrontOriginAccessIdentityResponse' {} a -> s {httpStatus = a} :: CreateCloudFrontOriginAccessIdentityResponse)

instance
  Core.NFData
    CreateCloudFrontOriginAccessIdentityResponse
