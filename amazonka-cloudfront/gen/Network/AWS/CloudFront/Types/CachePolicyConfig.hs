{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
    comment :: Prelude.Maybe Prelude.Text,
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
    maxTTL :: Prelude.Maybe Prelude.Integer,
    -- | The HTTP headers, cookies, and URL query strings to include in the cache
    -- key. The values included in the cache key are automatically included in
    -- requests that CloudFront sends to the origin.
    parametersInCacheKeyAndForwardedToOrigin :: Prelude.Maybe ParametersInCacheKeyAndForwardedToOrigin,
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
    defaultTTL :: Prelude.Maybe Prelude.Integer,
    -- | A unique name to identify the cache policy.
    name :: Prelude.Text,
    -- | The minimum amount of time, in seconds, that you want objects to stay in
    -- the CloudFront cache before CloudFront sends another request to the
    -- origin to see if the object has been updated. For more information, see
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Expiration.html Managing How Long Content Stays in an Edge Cache (Expiration)>
    -- in the /Amazon CloudFront Developer Guide/.
    minTTL :: Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'minTTL'
  Prelude.Integer ->
  CachePolicyConfig
newCachePolicyConfig pName_ pMinTTL_ =
  CachePolicyConfig'
    { comment = Prelude.Nothing,
      maxTTL = Prelude.Nothing,
      parametersInCacheKeyAndForwardedToOrigin =
        Prelude.Nothing,
      defaultTTL = Prelude.Nothing,
      name = pName_,
      minTTL = pMinTTL_
    }

-- | A comment to describe the cache policy.
cachePolicyConfig_comment :: Lens.Lens' CachePolicyConfig (Prelude.Maybe Prelude.Text)
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
cachePolicyConfig_maxTTL :: Lens.Lens' CachePolicyConfig (Prelude.Maybe Prelude.Integer)
cachePolicyConfig_maxTTL = Lens.lens (\CachePolicyConfig' {maxTTL} -> maxTTL) (\s@CachePolicyConfig' {} a -> s {maxTTL = a} :: CachePolicyConfig)

-- | The HTTP headers, cookies, and URL query strings to include in the cache
-- key. The values included in the cache key are automatically included in
-- requests that CloudFront sends to the origin.
cachePolicyConfig_parametersInCacheKeyAndForwardedToOrigin :: Lens.Lens' CachePolicyConfig (Prelude.Maybe ParametersInCacheKeyAndForwardedToOrigin)
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
cachePolicyConfig_defaultTTL :: Lens.Lens' CachePolicyConfig (Prelude.Maybe Prelude.Integer)
cachePolicyConfig_defaultTTL = Lens.lens (\CachePolicyConfig' {defaultTTL} -> defaultTTL) (\s@CachePolicyConfig' {} a -> s {defaultTTL = a} :: CachePolicyConfig)

-- | A unique name to identify the cache policy.
cachePolicyConfig_name :: Lens.Lens' CachePolicyConfig Prelude.Text
cachePolicyConfig_name = Lens.lens (\CachePolicyConfig' {name} -> name) (\s@CachePolicyConfig' {} a -> s {name = a} :: CachePolicyConfig)

-- | The minimum amount of time, in seconds, that you want objects to stay in
-- the CloudFront cache before CloudFront sends another request to the
-- origin to see if the object has been updated. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Expiration.html Managing How Long Content Stays in an Edge Cache (Expiration)>
-- in the /Amazon CloudFront Developer Guide/.
cachePolicyConfig_minTTL :: Lens.Lens' CachePolicyConfig Prelude.Integer
cachePolicyConfig_minTTL = Lens.lens (\CachePolicyConfig' {minTTL} -> minTTL) (\s@CachePolicyConfig' {} a -> s {minTTL = a} :: CachePolicyConfig)

instance Prelude.FromXML CachePolicyConfig where
  parseXML x =
    CachePolicyConfig'
      Prelude.<$> (x Prelude..@? "Comment")
      Prelude.<*> (x Prelude..@? "MaxTTL")
      Prelude.<*> ( x
                      Prelude..@? "ParametersInCacheKeyAndForwardedToOrigin"
                  )
      Prelude.<*> (x Prelude..@? "DefaultTTL")
      Prelude.<*> (x Prelude..@ "Name")
      Prelude.<*> (x Prelude..@ "MinTTL")

instance Prelude.Hashable CachePolicyConfig

instance Prelude.NFData CachePolicyConfig

instance Prelude.ToXML CachePolicyConfig where
  toXML CachePolicyConfig' {..} =
    Prelude.mconcat
      [ "Comment" Prelude.@= comment,
        "MaxTTL" Prelude.@= maxTTL,
        "ParametersInCacheKeyAndForwardedToOrigin"
          Prelude.@= parametersInCacheKeyAndForwardedToOrigin,
        "DefaultTTL" Prelude.@= defaultTTL,
        "Name" Prelude.@= name,
        "MinTTL" Prelude.@= minTTL
      ]
