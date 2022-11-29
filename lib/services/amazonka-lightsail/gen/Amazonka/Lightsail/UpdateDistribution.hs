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
-- Module      : Amazonka.Lightsail.UpdateDistribution
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing Amazon Lightsail content delivery network (CDN)
-- distribution.
--
-- Use this action to update the configuration of your existing
-- distribution.
module Amazonka.Lightsail.UpdateDistribution
  ( -- * Creating a Request
    UpdateDistribution (..),
    newUpdateDistribution,

    -- * Request Lenses
    updateDistribution_isEnabled,
    updateDistribution_cacheBehaviorSettings,
    updateDistribution_cacheBehaviors,
    updateDistribution_origin,
    updateDistribution_defaultCacheBehavior,
    updateDistribution_distributionName,

    -- * Destructuring the Response
    UpdateDistributionResponse (..),
    newUpdateDistributionResponse,

    -- * Response Lenses
    updateDistributionResponse_operation,
    updateDistributionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Lightsail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateDistribution' smart constructor.
data UpdateDistribution = UpdateDistribution'
  { -- | Indicates whether to enable the distribution.
    isEnabled :: Prelude.Maybe Prelude.Bool,
    -- | An object that describes the cache behavior settings for the
    -- distribution.
    --
    -- The @cacheBehaviorSettings@ specified in your
    -- @UpdateDistributionRequest@ will replace your distribution\'s existing
    -- settings.
    cacheBehaviorSettings :: Prelude.Maybe CacheSettings,
    -- | An array of objects that describe the per-path cache behavior for the
    -- distribution.
    cacheBehaviors :: Prelude.Maybe [CacheBehaviorPerPath],
    -- | An object that describes the origin resource for the distribution, such
    -- as a Lightsail instance, bucket, or load balancer.
    --
    -- The distribution pulls, caches, and serves content from the origin.
    origin :: Prelude.Maybe InputOrigin,
    -- | An object that describes the default cache behavior for the
    -- distribution.
    defaultCacheBehavior :: Prelude.Maybe CacheBehavior,
    -- | The name of the distribution to update.
    --
    -- Use the @GetDistributions@ action to get a list of distribution names
    -- that you can specify.
    distributionName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDistribution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isEnabled', 'updateDistribution_isEnabled' - Indicates whether to enable the distribution.
--
-- 'cacheBehaviorSettings', 'updateDistribution_cacheBehaviorSettings' - An object that describes the cache behavior settings for the
-- distribution.
--
-- The @cacheBehaviorSettings@ specified in your
-- @UpdateDistributionRequest@ will replace your distribution\'s existing
-- settings.
--
-- 'cacheBehaviors', 'updateDistribution_cacheBehaviors' - An array of objects that describe the per-path cache behavior for the
-- distribution.
--
-- 'origin', 'updateDistribution_origin' - An object that describes the origin resource for the distribution, such
-- as a Lightsail instance, bucket, or load balancer.
--
-- The distribution pulls, caches, and serves content from the origin.
--
-- 'defaultCacheBehavior', 'updateDistribution_defaultCacheBehavior' - An object that describes the default cache behavior for the
-- distribution.
--
-- 'distributionName', 'updateDistribution_distributionName' - The name of the distribution to update.
--
-- Use the @GetDistributions@ action to get a list of distribution names
-- that you can specify.
newUpdateDistribution ::
  -- | 'distributionName'
  Prelude.Text ->
  UpdateDistribution
newUpdateDistribution pDistributionName_ =
  UpdateDistribution'
    { isEnabled = Prelude.Nothing,
      cacheBehaviorSettings = Prelude.Nothing,
      cacheBehaviors = Prelude.Nothing,
      origin = Prelude.Nothing,
      defaultCacheBehavior = Prelude.Nothing,
      distributionName = pDistributionName_
    }

-- | Indicates whether to enable the distribution.
updateDistribution_isEnabled :: Lens.Lens' UpdateDistribution (Prelude.Maybe Prelude.Bool)
updateDistribution_isEnabled = Lens.lens (\UpdateDistribution' {isEnabled} -> isEnabled) (\s@UpdateDistribution' {} a -> s {isEnabled = a} :: UpdateDistribution)

-- | An object that describes the cache behavior settings for the
-- distribution.
--
-- The @cacheBehaviorSettings@ specified in your
-- @UpdateDistributionRequest@ will replace your distribution\'s existing
-- settings.
updateDistribution_cacheBehaviorSettings :: Lens.Lens' UpdateDistribution (Prelude.Maybe CacheSettings)
updateDistribution_cacheBehaviorSettings = Lens.lens (\UpdateDistribution' {cacheBehaviorSettings} -> cacheBehaviorSettings) (\s@UpdateDistribution' {} a -> s {cacheBehaviorSettings = a} :: UpdateDistribution)

-- | An array of objects that describe the per-path cache behavior for the
-- distribution.
updateDistribution_cacheBehaviors :: Lens.Lens' UpdateDistribution (Prelude.Maybe [CacheBehaviorPerPath])
updateDistribution_cacheBehaviors = Lens.lens (\UpdateDistribution' {cacheBehaviors} -> cacheBehaviors) (\s@UpdateDistribution' {} a -> s {cacheBehaviors = a} :: UpdateDistribution) Prelude.. Lens.mapping Lens.coerced

-- | An object that describes the origin resource for the distribution, such
-- as a Lightsail instance, bucket, or load balancer.
--
-- The distribution pulls, caches, and serves content from the origin.
updateDistribution_origin :: Lens.Lens' UpdateDistribution (Prelude.Maybe InputOrigin)
updateDistribution_origin = Lens.lens (\UpdateDistribution' {origin} -> origin) (\s@UpdateDistribution' {} a -> s {origin = a} :: UpdateDistribution)

-- | An object that describes the default cache behavior for the
-- distribution.
updateDistribution_defaultCacheBehavior :: Lens.Lens' UpdateDistribution (Prelude.Maybe CacheBehavior)
updateDistribution_defaultCacheBehavior = Lens.lens (\UpdateDistribution' {defaultCacheBehavior} -> defaultCacheBehavior) (\s@UpdateDistribution' {} a -> s {defaultCacheBehavior = a} :: UpdateDistribution)

-- | The name of the distribution to update.
--
-- Use the @GetDistributions@ action to get a list of distribution names
-- that you can specify.
updateDistribution_distributionName :: Lens.Lens' UpdateDistribution Prelude.Text
updateDistribution_distributionName = Lens.lens (\UpdateDistribution' {distributionName} -> distributionName) (\s@UpdateDistribution' {} a -> s {distributionName = a} :: UpdateDistribution)

instance Core.AWSRequest UpdateDistribution where
  type
    AWSResponse UpdateDistribution =
      UpdateDistributionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateDistributionResponse'
            Prelude.<$> (x Core..?> "operation")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateDistribution where
  hashWithSalt _salt UpdateDistribution' {..} =
    _salt `Prelude.hashWithSalt` isEnabled
      `Prelude.hashWithSalt` cacheBehaviorSettings
      `Prelude.hashWithSalt` cacheBehaviors
      `Prelude.hashWithSalt` origin
      `Prelude.hashWithSalt` defaultCacheBehavior
      `Prelude.hashWithSalt` distributionName

instance Prelude.NFData UpdateDistribution where
  rnf UpdateDistribution' {..} =
    Prelude.rnf isEnabled
      `Prelude.seq` Prelude.rnf cacheBehaviorSettings
      `Prelude.seq` Prelude.rnf cacheBehaviors
      `Prelude.seq` Prelude.rnf origin
      `Prelude.seq` Prelude.rnf defaultCacheBehavior
      `Prelude.seq` Prelude.rnf distributionName

instance Core.ToHeaders UpdateDistribution where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.UpdateDistribution" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateDistribution where
  toJSON UpdateDistribution' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("isEnabled" Core..=) Prelude.<$> isEnabled,
            ("cacheBehaviorSettings" Core..=)
              Prelude.<$> cacheBehaviorSettings,
            ("cacheBehaviors" Core..=)
              Prelude.<$> cacheBehaviors,
            ("origin" Core..=) Prelude.<$> origin,
            ("defaultCacheBehavior" Core..=)
              Prelude.<$> defaultCacheBehavior,
            Prelude.Just
              ("distributionName" Core..= distributionName)
          ]
      )

instance Core.ToPath UpdateDistribution where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateDistribution where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateDistributionResponse' smart constructor.
data UpdateDistributionResponse = UpdateDistributionResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operation :: Prelude.Maybe Operation,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDistributionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operation', 'updateDistributionResponse_operation' - An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
--
-- 'httpStatus', 'updateDistributionResponse_httpStatus' - The response's http status code.
newUpdateDistributionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateDistributionResponse
newUpdateDistributionResponse pHttpStatus_ =
  UpdateDistributionResponse'
    { operation =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
updateDistributionResponse_operation :: Lens.Lens' UpdateDistributionResponse (Prelude.Maybe Operation)
updateDistributionResponse_operation = Lens.lens (\UpdateDistributionResponse' {operation} -> operation) (\s@UpdateDistributionResponse' {} a -> s {operation = a} :: UpdateDistributionResponse)

-- | The response's http status code.
updateDistributionResponse_httpStatus :: Lens.Lens' UpdateDistributionResponse Prelude.Int
updateDistributionResponse_httpStatus = Lens.lens (\UpdateDistributionResponse' {httpStatus} -> httpStatus) (\s@UpdateDistributionResponse' {} a -> s {httpStatus = a} :: UpdateDistributionResponse)

instance Prelude.NFData UpdateDistributionResponse where
  rnf UpdateDistributionResponse' {..} =
    Prelude.rnf operation
      `Prelude.seq` Prelude.rnf httpStatus
