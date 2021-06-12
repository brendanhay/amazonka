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
-- Module      : Network.AWS.CloudFront.UpdateCachePolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a cache policy configuration.
--
-- When you update a cache policy configuration, all the fields are updated
-- with the values provided in the request. You cannot update some fields
-- independent of others. To update a cache policy configuration:
--
-- 1.  Use @GetCachePolicyConfig@ to get the current configuration.
--
-- 2.  Locally modify the fields in the cache policy configuration that you
--     want to update.
--
-- 3.  Call @UpdateCachePolicy@ by providing the entire cache policy
--     configuration, including the fields that you modified and those that
--     you didn’t.
module Network.AWS.CloudFront.UpdateCachePolicy
  ( -- * Creating a Request
    UpdateCachePolicy (..),
    newUpdateCachePolicy,

    -- * Request Lenses
    updateCachePolicy_ifMatch,
    updateCachePolicy_cachePolicyConfig,
    updateCachePolicy_id,

    -- * Destructuring the Response
    UpdateCachePolicyResponse (..),
    newUpdateCachePolicyResponse,

    -- * Response Lenses
    updateCachePolicyResponse_eTag,
    updateCachePolicyResponse_cachePolicy,
    updateCachePolicyResponse_httpStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateCachePolicy' smart constructor.
data UpdateCachePolicy = UpdateCachePolicy'
  { -- | The version of the cache policy that you are updating. The version is
    -- returned in the cache policy’s @ETag@ field in the response to
    -- @GetCachePolicyConfig@.
    ifMatch :: Core.Maybe Core.Text,
    -- | A cache policy configuration.
    cachePolicyConfig :: CachePolicyConfig,
    -- | The unique identifier for the cache policy that you are updating. The
    -- identifier is returned in a cache behavior’s @CachePolicyId@ field in
    -- the response to @GetDistributionConfig@.
    id :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateCachePolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ifMatch', 'updateCachePolicy_ifMatch' - The version of the cache policy that you are updating. The version is
-- returned in the cache policy’s @ETag@ field in the response to
-- @GetCachePolicyConfig@.
--
-- 'cachePolicyConfig', 'updateCachePolicy_cachePolicyConfig' - A cache policy configuration.
--
-- 'id', 'updateCachePolicy_id' - The unique identifier for the cache policy that you are updating. The
-- identifier is returned in a cache behavior’s @CachePolicyId@ field in
-- the response to @GetDistributionConfig@.
newUpdateCachePolicy ::
  -- | 'cachePolicyConfig'
  CachePolicyConfig ->
  -- | 'id'
  Core.Text ->
  UpdateCachePolicy
newUpdateCachePolicy pCachePolicyConfig_ pId_ =
  UpdateCachePolicy'
    { ifMatch = Core.Nothing,
      cachePolicyConfig = pCachePolicyConfig_,
      id = pId_
    }

-- | The version of the cache policy that you are updating. The version is
-- returned in the cache policy’s @ETag@ field in the response to
-- @GetCachePolicyConfig@.
updateCachePolicy_ifMatch :: Lens.Lens' UpdateCachePolicy (Core.Maybe Core.Text)
updateCachePolicy_ifMatch = Lens.lens (\UpdateCachePolicy' {ifMatch} -> ifMatch) (\s@UpdateCachePolicy' {} a -> s {ifMatch = a} :: UpdateCachePolicy)

-- | A cache policy configuration.
updateCachePolicy_cachePolicyConfig :: Lens.Lens' UpdateCachePolicy CachePolicyConfig
updateCachePolicy_cachePolicyConfig = Lens.lens (\UpdateCachePolicy' {cachePolicyConfig} -> cachePolicyConfig) (\s@UpdateCachePolicy' {} a -> s {cachePolicyConfig = a} :: UpdateCachePolicy)

-- | The unique identifier for the cache policy that you are updating. The
-- identifier is returned in a cache behavior’s @CachePolicyId@ field in
-- the response to @GetDistributionConfig@.
updateCachePolicy_id :: Lens.Lens' UpdateCachePolicy Core.Text
updateCachePolicy_id = Lens.lens (\UpdateCachePolicy' {id} -> id) (\s@UpdateCachePolicy' {} a -> s {id = a} :: UpdateCachePolicy)

instance Core.AWSRequest UpdateCachePolicy where
  type
    AWSResponse UpdateCachePolicy =
      UpdateCachePolicyResponse
  request = Request.putXML defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          UpdateCachePolicyResponse'
            Core.<$> (h Core..#? "ETag")
            Core.<*> (Core.parseXML x)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateCachePolicy

instance Core.NFData UpdateCachePolicy

instance Core.ToElement UpdateCachePolicy where
  toElement UpdateCachePolicy' {..} =
    Core.mkElement
      "{http://cloudfront.amazonaws.com/doc/2020-05-31/}CachePolicyConfig"
      cachePolicyConfig

instance Core.ToHeaders UpdateCachePolicy where
  toHeaders UpdateCachePolicy' {..} =
    Core.mconcat ["If-Match" Core.=# ifMatch]

instance Core.ToPath UpdateCachePolicy where
  toPath UpdateCachePolicy' {..} =
    Core.mconcat
      ["/2020-05-31/cache-policy/", Core.toBS id]

instance Core.ToQuery UpdateCachePolicy where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateCachePolicyResponse' smart constructor.
data UpdateCachePolicyResponse = UpdateCachePolicyResponse'
  { -- | The current version of the cache policy.
    eTag :: Core.Maybe Core.Text,
    -- | A cache policy.
    cachePolicy :: Core.Maybe CachePolicy,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateCachePolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eTag', 'updateCachePolicyResponse_eTag' - The current version of the cache policy.
--
-- 'cachePolicy', 'updateCachePolicyResponse_cachePolicy' - A cache policy.
--
-- 'httpStatus', 'updateCachePolicyResponse_httpStatus' - The response's http status code.
newUpdateCachePolicyResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateCachePolicyResponse
newUpdateCachePolicyResponse pHttpStatus_ =
  UpdateCachePolicyResponse'
    { eTag = Core.Nothing,
      cachePolicy = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current version of the cache policy.
updateCachePolicyResponse_eTag :: Lens.Lens' UpdateCachePolicyResponse (Core.Maybe Core.Text)
updateCachePolicyResponse_eTag = Lens.lens (\UpdateCachePolicyResponse' {eTag} -> eTag) (\s@UpdateCachePolicyResponse' {} a -> s {eTag = a} :: UpdateCachePolicyResponse)

-- | A cache policy.
updateCachePolicyResponse_cachePolicy :: Lens.Lens' UpdateCachePolicyResponse (Core.Maybe CachePolicy)
updateCachePolicyResponse_cachePolicy = Lens.lens (\UpdateCachePolicyResponse' {cachePolicy} -> cachePolicy) (\s@UpdateCachePolicyResponse' {} a -> s {cachePolicy = a} :: UpdateCachePolicyResponse)

-- | The response's http status code.
updateCachePolicyResponse_httpStatus :: Lens.Lens' UpdateCachePolicyResponse Core.Int
updateCachePolicyResponse_httpStatus = Lens.lens (\UpdateCachePolicyResponse' {httpStatus} -> httpStatus) (\s@UpdateCachePolicyResponse' {} a -> s {httpStatus = a} :: UpdateCachePolicyResponse)

instance Core.NFData UpdateCachePolicyResponse
