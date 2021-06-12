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
-- Module      : Network.AWS.CloudFront.CreateOriginRequestPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an origin request policy.
--
-- After you create an origin request policy, you can attach it to one or
-- more cache behaviors. When it’s attached to a cache behavior, the origin
-- request policy determines the values that CloudFront includes in
-- requests that it sends to the origin. Each request that CloudFront sends
-- to the origin includes the following:
--
-- -   The request body and the URL path (without the domain name) from the
--     viewer request.
--
-- -   The headers that CloudFront automatically includes in every origin
--     request, including @Host@, @User-Agent@, and @X-Amz-Cf-Id@.
--
-- -   All HTTP headers, cookies, and URL query strings that are specified
--     in the cache policy or the origin request policy. These can include
--     items from the viewer request and, in the case of headers,
--     additional ones that are added by CloudFront.
--
-- CloudFront sends a request when it can’t find a valid object in its
-- cache that matches the request. If you want to send values to the origin
-- and also include them in the cache key, use @CachePolicy@.
--
-- For more information about origin request policies, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-origin-requests.html Controlling origin requests>
-- in the /Amazon CloudFront Developer Guide/.
module Network.AWS.CloudFront.CreateOriginRequestPolicy
  ( -- * Creating a Request
    CreateOriginRequestPolicy (..),
    newCreateOriginRequestPolicy,

    -- * Request Lenses
    createOriginRequestPolicy_originRequestPolicyConfig,

    -- * Destructuring the Response
    CreateOriginRequestPolicyResponse (..),
    newCreateOriginRequestPolicyResponse,

    -- * Response Lenses
    createOriginRequestPolicyResponse_eTag,
    createOriginRequestPolicyResponse_originRequestPolicy,
    createOriginRequestPolicyResponse_location,
    createOriginRequestPolicyResponse_httpStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateOriginRequestPolicy' smart constructor.
data CreateOriginRequestPolicy = CreateOriginRequestPolicy'
  { -- | An origin request policy configuration.
    originRequestPolicyConfig :: OriginRequestPolicyConfig
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateOriginRequestPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'originRequestPolicyConfig', 'createOriginRequestPolicy_originRequestPolicyConfig' - An origin request policy configuration.
newCreateOriginRequestPolicy ::
  -- | 'originRequestPolicyConfig'
  OriginRequestPolicyConfig ->
  CreateOriginRequestPolicy
newCreateOriginRequestPolicy
  pOriginRequestPolicyConfig_ =
    CreateOriginRequestPolicy'
      { originRequestPolicyConfig =
          pOriginRequestPolicyConfig_
      }

-- | An origin request policy configuration.
createOriginRequestPolicy_originRequestPolicyConfig :: Lens.Lens' CreateOriginRequestPolicy OriginRequestPolicyConfig
createOriginRequestPolicy_originRequestPolicyConfig = Lens.lens (\CreateOriginRequestPolicy' {originRequestPolicyConfig} -> originRequestPolicyConfig) (\s@CreateOriginRequestPolicy' {} a -> s {originRequestPolicyConfig = a} :: CreateOriginRequestPolicy)

instance Core.AWSRequest CreateOriginRequestPolicy where
  type
    AWSResponse CreateOriginRequestPolicy =
      CreateOriginRequestPolicyResponse
  request = Request.postXML defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          CreateOriginRequestPolicyResponse'
            Core.<$> (h Core..#? "ETag")
            Core.<*> (Core.parseXML x)
            Core.<*> (h Core..#? "Location")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateOriginRequestPolicy

instance Core.NFData CreateOriginRequestPolicy

instance Core.ToElement CreateOriginRequestPolicy where
  toElement CreateOriginRequestPolicy' {..} =
    Core.mkElement
      "{http://cloudfront.amazonaws.com/doc/2020-05-31/}OriginRequestPolicyConfig"
      originRequestPolicyConfig

instance Core.ToHeaders CreateOriginRequestPolicy where
  toHeaders = Core.const Core.mempty

instance Core.ToPath CreateOriginRequestPolicy where
  toPath =
    Core.const "/2020-05-31/origin-request-policy"

instance Core.ToQuery CreateOriginRequestPolicy where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateOriginRequestPolicyResponse' smart constructor.
data CreateOriginRequestPolicyResponse = CreateOriginRequestPolicyResponse'
  { -- | The current version of the origin request policy.
    eTag :: Core.Maybe Core.Text,
    -- | An origin request policy.
    originRequestPolicy :: Core.Maybe OriginRequestPolicy,
    -- | The fully qualified URI of the origin request policy just created.
    location :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateOriginRequestPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eTag', 'createOriginRequestPolicyResponse_eTag' - The current version of the origin request policy.
--
-- 'originRequestPolicy', 'createOriginRequestPolicyResponse_originRequestPolicy' - An origin request policy.
--
-- 'location', 'createOriginRequestPolicyResponse_location' - The fully qualified URI of the origin request policy just created.
--
-- 'httpStatus', 'createOriginRequestPolicyResponse_httpStatus' - The response's http status code.
newCreateOriginRequestPolicyResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateOriginRequestPolicyResponse
newCreateOriginRequestPolicyResponse pHttpStatus_ =
  CreateOriginRequestPolicyResponse'
    { eTag =
        Core.Nothing,
      originRequestPolicy = Core.Nothing,
      location = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current version of the origin request policy.
createOriginRequestPolicyResponse_eTag :: Lens.Lens' CreateOriginRequestPolicyResponse (Core.Maybe Core.Text)
createOriginRequestPolicyResponse_eTag = Lens.lens (\CreateOriginRequestPolicyResponse' {eTag} -> eTag) (\s@CreateOriginRequestPolicyResponse' {} a -> s {eTag = a} :: CreateOriginRequestPolicyResponse)

-- | An origin request policy.
createOriginRequestPolicyResponse_originRequestPolicy :: Lens.Lens' CreateOriginRequestPolicyResponse (Core.Maybe OriginRequestPolicy)
createOriginRequestPolicyResponse_originRequestPolicy = Lens.lens (\CreateOriginRequestPolicyResponse' {originRequestPolicy} -> originRequestPolicy) (\s@CreateOriginRequestPolicyResponse' {} a -> s {originRequestPolicy = a} :: CreateOriginRequestPolicyResponse)

-- | The fully qualified URI of the origin request policy just created.
createOriginRequestPolicyResponse_location :: Lens.Lens' CreateOriginRequestPolicyResponse (Core.Maybe Core.Text)
createOriginRequestPolicyResponse_location = Lens.lens (\CreateOriginRequestPolicyResponse' {location} -> location) (\s@CreateOriginRequestPolicyResponse' {} a -> s {location = a} :: CreateOriginRequestPolicyResponse)

-- | The response's http status code.
createOriginRequestPolicyResponse_httpStatus :: Lens.Lens' CreateOriginRequestPolicyResponse Core.Int
createOriginRequestPolicyResponse_httpStatus = Lens.lens (\CreateOriginRequestPolicyResponse' {httpStatus} -> httpStatus) (\s@CreateOriginRequestPolicyResponse' {} a -> s {httpStatus = a} :: CreateOriginRequestPolicyResponse)

instance
  Core.NFData
    CreateOriginRequestPolicyResponse
