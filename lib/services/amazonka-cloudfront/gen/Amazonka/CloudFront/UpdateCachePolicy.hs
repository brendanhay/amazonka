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
-- Module      : Amazonka.CloudFront.UpdateCachePolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
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
--     you didn\'t.
module Amazonka.CloudFront.UpdateCachePolicy
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
    updateCachePolicyResponse_cachePolicy,
    updateCachePolicyResponse_eTag,
    updateCachePolicyResponse_httpStatus,
  )
where

import Amazonka.CloudFront.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateCachePolicy' smart constructor.
data UpdateCachePolicy = UpdateCachePolicy'
  { -- | The version of the cache policy that you are updating. The version is
    -- returned in the cache policy\'s @ETag@ field in the response to
    -- @GetCachePolicyConfig@.
    ifMatch :: Prelude.Maybe Prelude.Text,
    -- | A cache policy configuration.
    cachePolicyConfig :: CachePolicyConfig,
    -- | The unique identifier for the cache policy that you are updating. The
    -- identifier is returned in a cache behavior\'s @CachePolicyId@ field in
    -- the response to @GetDistributionConfig@.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateCachePolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ifMatch', 'updateCachePolicy_ifMatch' - The version of the cache policy that you are updating. The version is
-- returned in the cache policy\'s @ETag@ field in the response to
-- @GetCachePolicyConfig@.
--
-- 'cachePolicyConfig', 'updateCachePolicy_cachePolicyConfig' - A cache policy configuration.
--
-- 'id', 'updateCachePolicy_id' - The unique identifier for the cache policy that you are updating. The
-- identifier is returned in a cache behavior\'s @CachePolicyId@ field in
-- the response to @GetDistributionConfig@.
newUpdateCachePolicy ::
  -- | 'cachePolicyConfig'
  CachePolicyConfig ->
  -- | 'id'
  Prelude.Text ->
  UpdateCachePolicy
newUpdateCachePolicy pCachePolicyConfig_ pId_ =
  UpdateCachePolicy'
    { ifMatch = Prelude.Nothing,
      cachePolicyConfig = pCachePolicyConfig_,
      id = pId_
    }

-- | The version of the cache policy that you are updating. The version is
-- returned in the cache policy\'s @ETag@ field in the response to
-- @GetCachePolicyConfig@.
updateCachePolicy_ifMatch :: Lens.Lens' UpdateCachePolicy (Prelude.Maybe Prelude.Text)
updateCachePolicy_ifMatch = Lens.lens (\UpdateCachePolicy' {ifMatch} -> ifMatch) (\s@UpdateCachePolicy' {} a -> s {ifMatch = a} :: UpdateCachePolicy)

-- | A cache policy configuration.
updateCachePolicy_cachePolicyConfig :: Lens.Lens' UpdateCachePolicy CachePolicyConfig
updateCachePolicy_cachePolicyConfig = Lens.lens (\UpdateCachePolicy' {cachePolicyConfig} -> cachePolicyConfig) (\s@UpdateCachePolicy' {} a -> s {cachePolicyConfig = a} :: UpdateCachePolicy)

-- | The unique identifier for the cache policy that you are updating. The
-- identifier is returned in a cache behavior\'s @CachePolicyId@ field in
-- the response to @GetDistributionConfig@.
updateCachePolicy_id :: Lens.Lens' UpdateCachePolicy Prelude.Text
updateCachePolicy_id = Lens.lens (\UpdateCachePolicy' {id} -> id) (\s@UpdateCachePolicy' {} a -> s {id = a} :: UpdateCachePolicy)

instance Core.AWSRequest UpdateCachePolicy where
  type
    AWSResponse UpdateCachePolicy =
      UpdateCachePolicyResponse
  request overrides =
    Request.putXML (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          UpdateCachePolicyResponse'
            Prelude.<$> (Data.parseXML x)
            Prelude.<*> (h Data..#? "ETag")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateCachePolicy where
  hashWithSalt _salt UpdateCachePolicy' {..} =
    _salt
      `Prelude.hashWithSalt` ifMatch
      `Prelude.hashWithSalt` cachePolicyConfig
      `Prelude.hashWithSalt` id

instance Prelude.NFData UpdateCachePolicy where
  rnf UpdateCachePolicy' {..} =
    Prelude.rnf ifMatch
      `Prelude.seq` Prelude.rnf cachePolicyConfig
      `Prelude.seq` Prelude.rnf id

instance Data.ToElement UpdateCachePolicy where
  toElement UpdateCachePolicy' {..} =
    Data.mkElement
      "{http://cloudfront.amazonaws.com/doc/2020-05-31/}CachePolicyConfig"
      cachePolicyConfig

instance Data.ToHeaders UpdateCachePolicy where
  toHeaders UpdateCachePolicy' {..} =
    Prelude.mconcat ["If-Match" Data.=# ifMatch]

instance Data.ToPath UpdateCachePolicy where
  toPath UpdateCachePolicy' {..} =
    Prelude.mconcat
      ["/2020-05-31/cache-policy/", Data.toBS id]

instance Data.ToQuery UpdateCachePolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateCachePolicyResponse' smart constructor.
data UpdateCachePolicyResponse = UpdateCachePolicyResponse'
  { -- | A cache policy.
    cachePolicy :: Prelude.Maybe CachePolicy,
    -- | The current version of the cache policy.
    eTag :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateCachePolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cachePolicy', 'updateCachePolicyResponse_cachePolicy' - A cache policy.
--
-- 'eTag', 'updateCachePolicyResponse_eTag' - The current version of the cache policy.
--
-- 'httpStatus', 'updateCachePolicyResponse_httpStatus' - The response's http status code.
newUpdateCachePolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateCachePolicyResponse
newUpdateCachePolicyResponse pHttpStatus_ =
  UpdateCachePolicyResponse'
    { cachePolicy =
        Prelude.Nothing,
      eTag = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A cache policy.
updateCachePolicyResponse_cachePolicy :: Lens.Lens' UpdateCachePolicyResponse (Prelude.Maybe CachePolicy)
updateCachePolicyResponse_cachePolicy = Lens.lens (\UpdateCachePolicyResponse' {cachePolicy} -> cachePolicy) (\s@UpdateCachePolicyResponse' {} a -> s {cachePolicy = a} :: UpdateCachePolicyResponse)

-- | The current version of the cache policy.
updateCachePolicyResponse_eTag :: Lens.Lens' UpdateCachePolicyResponse (Prelude.Maybe Prelude.Text)
updateCachePolicyResponse_eTag = Lens.lens (\UpdateCachePolicyResponse' {eTag} -> eTag) (\s@UpdateCachePolicyResponse' {} a -> s {eTag = a} :: UpdateCachePolicyResponse)

-- | The response's http status code.
updateCachePolicyResponse_httpStatus :: Lens.Lens' UpdateCachePolicyResponse Prelude.Int
updateCachePolicyResponse_httpStatus = Lens.lens (\UpdateCachePolicyResponse' {httpStatus} -> httpStatus) (\s@UpdateCachePolicyResponse' {} a -> s {httpStatus = a} :: UpdateCachePolicyResponse)

instance Prelude.NFData UpdateCachePolicyResponse where
  rnf UpdateCachePolicyResponse' {..} =
    Prelude.rnf cachePolicy
      `Prelude.seq` Prelude.rnf eTag
      `Prelude.seq` Prelude.rnf httpStatus
