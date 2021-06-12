{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.CachePolicyConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.CachePolicyConfig where

import Network.AWS.CloudFront.Types.ParametersInCacheKeyAndForwardedToOrigin
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A cache policy configuration.
--
-- This configuration determines the following:
--
-- -   The values that CloudFront includes in the cache key. These values
--     can include HTTP headers, cookies, and URL query strings. CloudFront
--     uses the cache key to find an object in its cache that it can return
--     to the viewer.
--
-- -   The default, minimum, and maximum time to live (TTL) values that you
--     want objects to stay in the CloudFront cache.
--
-- The headers, cookies, and query strings that are included in the cache
-- key are automatically included in requests that CloudFront sends to the
-- origin. CloudFront sends a request when it can’t find a valid object in
-- its cache that matches the request’s cache key. If you want to send
-- values to the origin but /not/ include them in the cache key, use
-- @OriginRequestPolicy@.
--
-- /See:/ 'newCachePolicyConfig' smart constructor.
data CachePolicyConfig = CachePolicyConfig'
  { -- | A comment to describe the cache policy.
    comment :: Core.Maybe Core.Text,
    -- | The maximum amount of time, in seconds, that objects stay in the
    -- CloudFront cache before CloudFront sends another request to the origin
    -- to see if the object has been updated. CloudFront uses this value only
    -- when the origin sends @Cache-Control@ or @Expires@ headers with the
    -- object. For more information, see
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Expiration.html Managing How Long Content Stays in an Edge Cache (Expiration)>
    -- in the /Amazon CloudFront Developer Guide/.
    --
    -- The default value for this field is 31536000 seconds (one year). If the
    -- value of @MinTTL@ or @DefaultTTL@ is more than 31536000 seconds, then
    -- the default value for this field is the same as the value of
    -- @DefaultTTL@.
    maxTTL :: Core.Maybe Core.Integer,
    -- | The HTTP headers, cookies, and URL query strings to include in the cache
    -- key. The values included in the cache key are automatically included in
    -- requests that CloudFront sends to the origin.
    parametersInCacheKeyAndForwardedToOrigin :: Core.Maybe ParametersInCacheKeyAndForwardedToOrigin,
    -- | The default amount of time, in seconds, that you want objects to stay in
    -- the CloudFront cache before CloudFront sends another request to the
    -- origin to see if the object has been updated. CloudFront uses this value
    -- as the object’s time to live (TTL) only when the origin does /not/ send
    -- @Cache-Control@ or @Expires@ headers with the object. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Expiration.html Managing How Long Content Stays in an Edge Cache (Expiration)>
    -- in the /Amazon CloudFront Developer Guide/.
    --
    -- The default value for this field is 86400 seconds (one day). If the
    -- value of @MinTTL@ is more than 86400 seconds, then the default value for
    -- this field is the same as the value of @MinTTL@.
    defaultTTL :: Core.Maybe Core.Integer,
    -- | A unique name to identify the cache policy.
    name :: Core.Text,
    -- | The minimum amount of time, in seconds, that you want objects to stay in
    -- the CloudFront cache before CloudFront sends another request to the
    -- origin to see if the object has been updated. For more information, see
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Expiration.html Managing How Long Content Stays in an Edge Cache (Expiration)>
    -- in the /Amazon CloudFront Developer Guide/.
    minTTL :: Core.Integer
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CachePolicyConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'comment', 'cachePolicyConfig_comment' - A comment to describe the cache policy.
--
-- 'maxTTL', 'cachePolicyConfig_maxTTL' - The maximum amount of time, in seconds, that objects stay in the
-- CloudFront cache before CloudFront sends another request to the origin
-- to see if the object has been updated. CloudFront uses this value only
-- when the origin sends @Cache-Control@ or @Expires@ headers with the
-- object. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Expiration.html Managing How Long Content Stays in an Edge Cache (Expiration)>
-- in the /Amazon CloudFront Developer Guide/.
--
-- The default value for this field is 31536000 seconds (one year). If the
-- value of @MinTTL@ or @DefaultTTL@ is more than 31536000 seconds, then
-- the default value for this field is the same as the value of
-- @DefaultTTL@.
--
-- 'parametersInCacheKeyAndForwardedToOrigin', 'cachePolicyConfig_parametersInCacheKeyAndForwardedToOrigin' - The HTTP headers, cookies, and URL query strings to include in the cache
-- key. The values included in the cache key are automatically included in
-- requests that CloudFront sends to the origin.
--
-- 'defaultTTL', 'cachePolicyConfig_defaultTTL' - The default amount of time, in seconds, that you want objects to stay in
-- the CloudFront cache before CloudFront sends another request to the
-- origin to see if the object has been updated. CloudFront uses this value
-- as the object’s time to live (TTL) only when the origin does /not/ send
-- @Cache-Control@ or @Expires@ headers with the object. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Expiration.html Managing How Long Content Stays in an Edge Cache (Expiration)>
-- in the /Amazon CloudFront Developer Guide/.
--
-- The default value for this field is 86400 seconds (one day). If the
-- value of @MinTTL@ is more than 86400 seconds, then the default value for
-- this field is the same as the value of @MinTTL@.
--
-- 'name', 'cachePolicyConfig_name' - A unique name to identify the cache policy.
--
-- 'minTTL', 'cachePolicyConfig_minTTL' - The minimum amount of time, in seconds, that you want objects to stay in
-- the CloudFront cache before CloudFront sends another request to the
-- origin to see if the object has been updated. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Expiration.html Managing How Long Content Stays in an Edge Cache (Expiration)>
-- in the /Amazon CloudFront Developer Guide/.
newCachePolicyConfig ::
  -- | 'name'
  Core.Text ->
  -- | 'minTTL'
  Core.Integer ->
  CachePolicyConfig
newCachePolicyConfig pName_ pMinTTL_ =
  CachePolicyConfig'
    { comment = Core.Nothing,
      maxTTL = Core.Nothing,
      parametersInCacheKeyAndForwardedToOrigin =
        Core.Nothing,
      defaultTTL = Core.Nothing,
      name = pName_,
      minTTL = pMinTTL_
    }

-- | A comment to describe the cache policy.
cachePolicyConfig_comment :: Lens.Lens' CachePolicyConfig (Core.Maybe Core.Text)
cachePolicyConfig_comment = Lens.lens (\CachePolicyConfig' {comment} -> comment) (\s@CachePolicyConfig' {} a -> s {comment = a} :: CachePolicyConfig)

-- | The maximum amount of time, in seconds, that objects stay in the
-- CloudFront cache before CloudFront sends another request to the origin
-- to see if the object has been updated. CloudFront uses this value only
-- when the origin sends @Cache-Control@ or @Expires@ headers with the
-- object. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Expiration.html Managing How Long Content Stays in an Edge Cache (Expiration)>
-- in the /Amazon CloudFront Developer Guide/.
--
-- The default value for this field is 31536000 seconds (one year). If the
-- value of @MinTTL@ or @DefaultTTL@ is more than 31536000 seconds, then
-- the default value for this field is the same as the value of
-- @DefaultTTL@.
cachePolicyConfig_maxTTL :: Lens.Lens' CachePolicyConfig (Core.Maybe Core.Integer)
cachePolicyConfig_maxTTL = Lens.lens (\CachePolicyConfig' {maxTTL} -> maxTTL) (\s@CachePolicyConfig' {} a -> s {maxTTL = a} :: CachePolicyConfig)

-- | The HTTP headers, cookies, and URL query strings to include in the cache
-- key. The values included in the cache key are automatically included in
-- requests that CloudFront sends to the origin.
cachePolicyConfig_parametersInCacheKeyAndForwardedToOrigin :: Lens.Lens' CachePolicyConfig (Core.Maybe ParametersInCacheKeyAndForwardedToOrigin)
cachePolicyConfig_parametersInCacheKeyAndForwardedToOrigin = Lens.lens (\CachePolicyConfig' {parametersInCacheKeyAndForwardedToOrigin} -> parametersInCacheKeyAndForwardedToOrigin) (\s@CachePolicyConfig' {} a -> s {parametersInCacheKeyAndForwardedToOrigin = a} :: CachePolicyConfig)

-- | The default amount of time, in seconds, that you want objects to stay in
-- the CloudFront cache before CloudFront sends another request to the
-- origin to see if the object has been updated. CloudFront uses this value
-- as the object’s time to live (TTL) only when the origin does /not/ send
-- @Cache-Control@ or @Expires@ headers with the object. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Expiration.html Managing How Long Content Stays in an Edge Cache (Expiration)>
-- in the /Amazon CloudFront Developer Guide/.
--
-- The default value for this field is 86400 seconds (one day). If the
-- value of @MinTTL@ is more than 86400 seconds, then the default value for
-- this field is the same as the value of @MinTTL@.
cachePolicyConfig_defaultTTL :: Lens.Lens' CachePolicyConfig (Core.Maybe Core.Integer)
cachePolicyConfig_defaultTTL = Lens.lens (\CachePolicyConfig' {defaultTTL} -> defaultTTL) (\s@CachePolicyConfig' {} a -> s {defaultTTL = a} :: CachePolicyConfig)

-- | A unique name to identify the cache policy.
cachePolicyConfig_name :: Lens.Lens' CachePolicyConfig Core.Text
cachePolicyConfig_name = Lens.lens (\CachePolicyConfig' {name} -> name) (\s@CachePolicyConfig' {} a -> s {name = a} :: CachePolicyConfig)

-- | The minimum amount of time, in seconds, that you want objects to stay in
-- the CloudFront cache before CloudFront sends another request to the
-- origin to see if the object has been updated. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Expiration.html Managing How Long Content Stays in an Edge Cache (Expiration)>
-- in the /Amazon CloudFront Developer Guide/.
cachePolicyConfig_minTTL :: Lens.Lens' CachePolicyConfig Core.Integer
cachePolicyConfig_minTTL = Lens.lens (\CachePolicyConfig' {minTTL} -> minTTL) (\s@CachePolicyConfig' {} a -> s {minTTL = a} :: CachePolicyConfig)

instance Core.FromXML CachePolicyConfig where
  parseXML x =
    CachePolicyConfig'
      Core.<$> (x Core..@? "Comment")
      Core.<*> (x Core..@? "MaxTTL")
      Core.<*> ( x
                   Core..@? "ParametersInCacheKeyAndForwardedToOrigin"
               )
      Core.<*> (x Core..@? "DefaultTTL")
      Core.<*> (x Core..@ "Name")
      Core.<*> (x Core..@ "MinTTL")

instance Core.Hashable CachePolicyConfig

instance Core.NFData CachePolicyConfig

instance Core.ToXML CachePolicyConfig where
  toXML CachePolicyConfig' {..} =
    Core.mconcat
      [ "Comment" Core.@= comment,
        "MaxTTL" Core.@= maxTTL,
        "ParametersInCacheKeyAndForwardedToOrigin"
          Core.@= parametersInCacheKeyAndForwardedToOrigin,
        "DefaultTTL" Core.@= defaultTTL,
        "Name" Core.@= name,
        "MinTTL" Core.@= minTTL
      ]
