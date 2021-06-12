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
-- Module      : Network.AWS.Lightsail.CreateDistribution
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Amazon Lightsail content delivery network (CDN) distribution.
--
-- A distribution is a globally distributed network of caching servers that
-- improve the performance of your website or web application hosted on a
-- Lightsail instance. For more information, see
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-content-delivery-network-distributions Content delivery networks in Amazon Lightsail>.
module Network.AWS.Lightsail.CreateDistribution
  ( -- * Creating a Request
    CreateDistribution (..),
    newCreateDistribution,

    -- * Request Lenses
    createDistribution_ipAddressType,
    createDistribution_cacheBehaviorSettings,
    createDistribution_tags,
    createDistribution_cacheBehaviors,
    createDistribution_distributionName,
    createDistribution_origin,
    createDistribution_defaultCacheBehavior,
    createDistribution_bundleId,

    -- * Destructuring the Response
    CreateDistributionResponse (..),
    newCreateDistributionResponse,

    -- * Response Lenses
    createDistributionResponse_operation,
    createDistributionResponse_distribution,
    createDistributionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateDistribution' smart constructor.
data CreateDistribution = CreateDistribution'
  { -- | The IP address type for the distribution.
    --
    -- The possible values are @ipv4@ for IPv4 only, and @dualstack@ for IPv4
    -- and IPv6.
    --
    -- The default value is @dualstack@.
    ipAddressType :: Core.Maybe IpAddressType,
    -- | An object that describes the cache behavior settings for the
    -- distribution.
    cacheBehaviorSettings :: Core.Maybe CacheSettings,
    -- | The tag keys and optional values to add to the distribution during
    -- create.
    --
    -- Use the @TagResource@ action to tag a resource after it\'s created.
    tags :: Core.Maybe [Tag],
    -- | An array of objects that describe the per-path cache behavior for the
    -- distribution.
    cacheBehaviors :: Core.Maybe [CacheBehaviorPerPath],
    -- | The name for the distribution.
    distributionName :: Core.Text,
    -- | An object that describes the origin resource for the distribution, such
    -- as a Lightsail instance or load balancer.
    --
    -- The distribution pulls, caches, and serves content from the origin.
    origin :: InputOrigin,
    -- | An object that describes the default cache behavior for the
    -- distribution.
    defaultCacheBehavior :: CacheBehavior,
    -- | The bundle ID to use for the distribution.
    --
    -- A distribution bundle describes the specifications of your distribution,
    -- such as the monthly cost and monthly network transfer quota.
    --
    -- Use the @GetDistributionBundles@ action to get a list of distribution
    -- bundle IDs that you can specify.
    bundleId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateDistribution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipAddressType', 'createDistribution_ipAddressType' - The IP address type for the distribution.
--
-- The possible values are @ipv4@ for IPv4 only, and @dualstack@ for IPv4
-- and IPv6.
--
-- The default value is @dualstack@.
--
-- 'cacheBehaviorSettings', 'createDistribution_cacheBehaviorSettings' - An object that describes the cache behavior settings for the
-- distribution.
--
-- 'tags', 'createDistribution_tags' - The tag keys and optional values to add to the distribution during
-- create.
--
-- Use the @TagResource@ action to tag a resource after it\'s created.
--
-- 'cacheBehaviors', 'createDistribution_cacheBehaviors' - An array of objects that describe the per-path cache behavior for the
-- distribution.
--
-- 'distributionName', 'createDistribution_distributionName' - The name for the distribution.
--
-- 'origin', 'createDistribution_origin' - An object that describes the origin resource for the distribution, such
-- as a Lightsail instance or load balancer.
--
-- The distribution pulls, caches, and serves content from the origin.
--
-- 'defaultCacheBehavior', 'createDistribution_defaultCacheBehavior' - An object that describes the default cache behavior for the
-- distribution.
--
-- 'bundleId', 'createDistribution_bundleId' - The bundle ID to use for the distribution.
--
-- A distribution bundle describes the specifications of your distribution,
-- such as the monthly cost and monthly network transfer quota.
--
-- Use the @GetDistributionBundles@ action to get a list of distribution
-- bundle IDs that you can specify.
newCreateDistribution ::
  -- | 'distributionName'
  Core.Text ->
  -- | 'origin'
  InputOrigin ->
  -- | 'defaultCacheBehavior'
  CacheBehavior ->
  -- | 'bundleId'
  Core.Text ->
  CreateDistribution
newCreateDistribution
  pDistributionName_
  pOrigin_
  pDefaultCacheBehavior_
  pBundleId_ =
    CreateDistribution'
      { ipAddressType = Core.Nothing,
        cacheBehaviorSettings = Core.Nothing,
        tags = Core.Nothing,
        cacheBehaviors = Core.Nothing,
        distributionName = pDistributionName_,
        origin = pOrigin_,
        defaultCacheBehavior = pDefaultCacheBehavior_,
        bundleId = pBundleId_
      }

-- | The IP address type for the distribution.
--
-- The possible values are @ipv4@ for IPv4 only, and @dualstack@ for IPv4
-- and IPv6.
--
-- The default value is @dualstack@.
createDistribution_ipAddressType :: Lens.Lens' CreateDistribution (Core.Maybe IpAddressType)
createDistribution_ipAddressType = Lens.lens (\CreateDistribution' {ipAddressType} -> ipAddressType) (\s@CreateDistribution' {} a -> s {ipAddressType = a} :: CreateDistribution)

-- | An object that describes the cache behavior settings for the
-- distribution.
createDistribution_cacheBehaviorSettings :: Lens.Lens' CreateDistribution (Core.Maybe CacheSettings)
createDistribution_cacheBehaviorSettings = Lens.lens (\CreateDistribution' {cacheBehaviorSettings} -> cacheBehaviorSettings) (\s@CreateDistribution' {} a -> s {cacheBehaviorSettings = a} :: CreateDistribution)

-- | The tag keys and optional values to add to the distribution during
-- create.
--
-- Use the @TagResource@ action to tag a resource after it\'s created.
createDistribution_tags :: Lens.Lens' CreateDistribution (Core.Maybe [Tag])
createDistribution_tags = Lens.lens (\CreateDistribution' {tags} -> tags) (\s@CreateDistribution' {} a -> s {tags = a} :: CreateDistribution) Core.. Lens.mapping Lens._Coerce

-- | An array of objects that describe the per-path cache behavior for the
-- distribution.
createDistribution_cacheBehaviors :: Lens.Lens' CreateDistribution (Core.Maybe [CacheBehaviorPerPath])
createDistribution_cacheBehaviors = Lens.lens (\CreateDistribution' {cacheBehaviors} -> cacheBehaviors) (\s@CreateDistribution' {} a -> s {cacheBehaviors = a} :: CreateDistribution) Core.. Lens.mapping Lens._Coerce

-- | The name for the distribution.
createDistribution_distributionName :: Lens.Lens' CreateDistribution Core.Text
createDistribution_distributionName = Lens.lens (\CreateDistribution' {distributionName} -> distributionName) (\s@CreateDistribution' {} a -> s {distributionName = a} :: CreateDistribution)

-- | An object that describes the origin resource for the distribution, such
-- as a Lightsail instance or load balancer.
--
-- The distribution pulls, caches, and serves content from the origin.
createDistribution_origin :: Lens.Lens' CreateDistribution InputOrigin
createDistribution_origin = Lens.lens (\CreateDistribution' {origin} -> origin) (\s@CreateDistribution' {} a -> s {origin = a} :: CreateDistribution)

-- | An object that describes the default cache behavior for the
-- distribution.
createDistribution_defaultCacheBehavior :: Lens.Lens' CreateDistribution CacheBehavior
createDistribution_defaultCacheBehavior = Lens.lens (\CreateDistribution' {defaultCacheBehavior} -> defaultCacheBehavior) (\s@CreateDistribution' {} a -> s {defaultCacheBehavior = a} :: CreateDistribution)

-- | The bundle ID to use for the distribution.
--
-- A distribution bundle describes the specifications of your distribution,
-- such as the monthly cost and monthly network transfer quota.
--
-- Use the @GetDistributionBundles@ action to get a list of distribution
-- bundle IDs that you can specify.
createDistribution_bundleId :: Lens.Lens' CreateDistribution Core.Text
createDistribution_bundleId = Lens.lens (\CreateDistribution' {bundleId} -> bundleId) (\s@CreateDistribution' {} a -> s {bundleId = a} :: CreateDistribution)

instance Core.AWSRequest CreateDistribution where
  type
    AWSResponse CreateDistribution =
      CreateDistributionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDistributionResponse'
            Core.<$> (x Core..?> "operation")
            Core.<*> (x Core..?> "distribution")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateDistribution

instance Core.NFData CreateDistribution

instance Core.ToHeaders CreateDistribution where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.CreateDistribution" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateDistribution where
  toJSON CreateDistribution' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ipAddressType" Core..=) Core.<$> ipAddressType,
            ("cacheBehaviorSettings" Core..=)
              Core.<$> cacheBehaviorSettings,
            ("tags" Core..=) Core.<$> tags,
            ("cacheBehaviors" Core..=) Core.<$> cacheBehaviors,
            Core.Just
              ("distributionName" Core..= distributionName),
            Core.Just ("origin" Core..= origin),
            Core.Just
              ( "defaultCacheBehavior"
                  Core..= defaultCacheBehavior
              ),
            Core.Just ("bundleId" Core..= bundleId)
          ]
      )

instance Core.ToPath CreateDistribution where
  toPath = Core.const "/"

instance Core.ToQuery CreateDistribution where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateDistributionResponse' smart constructor.
data CreateDistributionResponse = CreateDistributionResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operation :: Core.Maybe Operation,
    -- | An object that describes the distribution created.
    distribution :: Core.Maybe LightsailDistribution,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateDistributionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operation', 'createDistributionResponse_operation' - An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
--
-- 'distribution', 'createDistributionResponse_distribution' - An object that describes the distribution created.
--
-- 'httpStatus', 'createDistributionResponse_httpStatus' - The response's http status code.
newCreateDistributionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateDistributionResponse
newCreateDistributionResponse pHttpStatus_ =
  CreateDistributionResponse'
    { operation =
        Core.Nothing,
      distribution = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
createDistributionResponse_operation :: Lens.Lens' CreateDistributionResponse (Core.Maybe Operation)
createDistributionResponse_operation = Lens.lens (\CreateDistributionResponse' {operation} -> operation) (\s@CreateDistributionResponse' {} a -> s {operation = a} :: CreateDistributionResponse)

-- | An object that describes the distribution created.
createDistributionResponse_distribution :: Lens.Lens' CreateDistributionResponse (Core.Maybe LightsailDistribution)
createDistributionResponse_distribution = Lens.lens (\CreateDistributionResponse' {distribution} -> distribution) (\s@CreateDistributionResponse' {} a -> s {distribution = a} :: CreateDistributionResponse)

-- | The response's http status code.
createDistributionResponse_httpStatus :: Lens.Lens' CreateDistributionResponse Core.Int
createDistributionResponse_httpStatus = Lens.lens (\CreateDistributionResponse' {httpStatus} -> httpStatus) (\s@CreateDistributionResponse' {} a -> s {httpStatus = a} :: CreateDistributionResponse)

instance Core.NFData CreateDistributionResponse
