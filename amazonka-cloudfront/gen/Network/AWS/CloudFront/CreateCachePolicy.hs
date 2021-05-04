{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CloudFront.CreateCachePolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a cache policy.
--
-- After you create a cache policy, you can attach it to one or more cache
-- behaviors. When it’s attached to a cache behavior, the cache policy
-- determines the following:
--
-- -   The values that CloudFront includes in the /cache key/. These values
--     can include HTTP headers, cookies, and URL query strings. CloudFront
--     uses the cache key to find an object in its cache that it can return
--     to the viewer.
--
-- -   The default, minimum, and maximum time to live (TTL) values that you
--     want objects to stay in the CloudFront cache.
--
-- The headers, cookies, and query strings that are included in the cache
-- key are automatically included in requests that CloudFront sends to the
-- origin. CloudFront sends a request when it can’t find an object in its
-- cache that matches the request’s cache key. If you want to send values
-- to the origin but /not/ include them in the cache key, use
-- @OriginRequestPolicy@.
--
-- For more information about cache policies, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html Controlling the cache key>
-- in the /Amazon CloudFront Developer Guide/.
module Network.AWS.CloudFront.CreateCachePolicy
  ( -- * Creating a Request
    CreateCachePolicy (..),
    newCreateCachePolicy,

    -- * Request Lenses
    createCachePolicy_cachePolicyConfig,

    -- * Destructuring the Response
    CreateCachePolicyResponse (..),
    newCreateCachePolicyResponse,

    -- * Response Lenses
    createCachePolicyResponse_eTag,
    createCachePolicyResponse_cachePolicy,
    createCachePolicyResponse_location,
    createCachePolicyResponse_httpStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateCachePolicy' smart constructor.
data CreateCachePolicy = CreateCachePolicy'
  { -- | A cache policy configuration.
    cachePolicyConfig :: CachePolicyConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateCachePolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cachePolicyConfig', 'createCachePolicy_cachePolicyConfig' - A cache policy configuration.
newCreateCachePolicy ::
  -- | 'cachePolicyConfig'
  CachePolicyConfig ->
  CreateCachePolicy
newCreateCachePolicy pCachePolicyConfig_ =
  CreateCachePolicy'
    { cachePolicyConfig =
        pCachePolicyConfig_
    }

-- | A cache policy configuration.
createCachePolicy_cachePolicyConfig :: Lens.Lens' CreateCachePolicy CachePolicyConfig
createCachePolicy_cachePolicyConfig = Lens.lens (\CreateCachePolicy' {cachePolicyConfig} -> cachePolicyConfig) (\s@CreateCachePolicy' {} a -> s {cachePolicyConfig = a} :: CreateCachePolicy)

instance Prelude.AWSRequest CreateCachePolicy where
  type Rs CreateCachePolicy = CreateCachePolicyResponse
  request = Request.postXML defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          CreateCachePolicyResponse'
            Prelude.<$> (h Prelude..#? "ETag")
            Prelude.<*> (Prelude.parseXML x)
            Prelude.<*> (h Prelude..#? "Location")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateCachePolicy

instance Prelude.NFData CreateCachePolicy

instance Prelude.ToElement CreateCachePolicy where
  toElement CreateCachePolicy' {..} =
    Prelude.mkElement
      "{http://cloudfront.amazonaws.com/doc/2020-05-31/}CachePolicyConfig"
      cachePolicyConfig

instance Prelude.ToHeaders CreateCachePolicy where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath CreateCachePolicy where
  toPath = Prelude.const "/2020-05-31/cache-policy"

instance Prelude.ToQuery CreateCachePolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateCachePolicyResponse' smart constructor.
data CreateCachePolicyResponse = CreateCachePolicyResponse'
  { -- | The current version of the cache policy.
    eTag :: Prelude.Maybe Prelude.Text,
    -- | A cache policy.
    cachePolicy :: Prelude.Maybe CachePolicy,
    -- | The fully qualified URI of the cache policy just created.
    location :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateCachePolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eTag', 'createCachePolicyResponse_eTag' - The current version of the cache policy.
--
-- 'cachePolicy', 'createCachePolicyResponse_cachePolicy' - A cache policy.
--
-- 'location', 'createCachePolicyResponse_location' - The fully qualified URI of the cache policy just created.
--
-- 'httpStatus', 'createCachePolicyResponse_httpStatus' - The response's http status code.
newCreateCachePolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateCachePolicyResponse
newCreateCachePolicyResponse pHttpStatus_ =
  CreateCachePolicyResponse'
    { eTag = Prelude.Nothing,
      cachePolicy = Prelude.Nothing,
      location = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current version of the cache policy.
createCachePolicyResponse_eTag :: Lens.Lens' CreateCachePolicyResponse (Prelude.Maybe Prelude.Text)
createCachePolicyResponse_eTag = Lens.lens (\CreateCachePolicyResponse' {eTag} -> eTag) (\s@CreateCachePolicyResponse' {} a -> s {eTag = a} :: CreateCachePolicyResponse)

-- | A cache policy.
createCachePolicyResponse_cachePolicy :: Lens.Lens' CreateCachePolicyResponse (Prelude.Maybe CachePolicy)
createCachePolicyResponse_cachePolicy = Lens.lens (\CreateCachePolicyResponse' {cachePolicy} -> cachePolicy) (\s@CreateCachePolicyResponse' {} a -> s {cachePolicy = a} :: CreateCachePolicyResponse)

-- | The fully qualified URI of the cache policy just created.
createCachePolicyResponse_location :: Lens.Lens' CreateCachePolicyResponse (Prelude.Maybe Prelude.Text)
createCachePolicyResponse_location = Lens.lens (\CreateCachePolicyResponse' {location} -> location) (\s@CreateCachePolicyResponse' {} a -> s {location = a} :: CreateCachePolicyResponse)

-- | The response's http status code.
createCachePolicyResponse_httpStatus :: Lens.Lens' CreateCachePolicyResponse Prelude.Int
createCachePolicyResponse_httpStatus = Lens.lens (\CreateCachePolicyResponse' {httpStatus} -> httpStatus) (\s@CreateCachePolicyResponse' {} a -> s {httpStatus = a} :: CreateCachePolicyResponse)

instance Prelude.NFData CreateCachePolicyResponse
