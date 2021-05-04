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
-- Module      : Network.AWS.CloudFront.Types.CookiePreference
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.CookiePreference where

import Network.AWS.CloudFront.Types.CookieNames
import Network.AWS.CloudFront.Types.ItemSelection
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | This field is deprecated. We recommend that you use a cache policy or an
-- origin request policy instead of this field.
--
-- If you want to include cookies in the cache key, use @CookiesConfig@ in
-- a cache policy. See @CachePolicy@.
--
-- If you want to send cookies to the origin but not include them in the
-- cache key, use @CookiesConfig@ in an origin request policy. See
-- @OriginRequestPolicy@.
--
-- A complex type that specifies whether you want CloudFront to forward
-- cookies to the origin and, if so, which ones. For more information about
-- forwarding cookies to the origin, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Cookies.html Caching Content Based on Cookies>
-- in the /Amazon CloudFront Developer Guide/.
--
-- /See:/ 'newCookiePreference' smart constructor.
data CookiePreference = CookiePreference'
  { -- | This field is deprecated. We recommend that you use a cache policy or an
    -- origin request policy instead of this field.
    --
    -- If you want to include cookies in the cache key, use a cache policy. For
    -- more information, see
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies>
    -- in the /Amazon CloudFront Developer Guide/.
    --
    -- If you want to send cookies to the origin but not include them in the
    -- cache key, use an origin request policy. For more information, see
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-origin-requests.html#origin-request-create-origin-request-policy Creating origin request policies>
    -- in the /Amazon CloudFront Developer Guide/.
    --
    -- Required if you specify @whitelist@ for the value of @Forward@. A
    -- complex type that specifies how many different cookies you want
    -- CloudFront to forward to the origin for this cache behavior and, if you
    -- want to forward selected cookies, the names of those cookies.
    --
    -- If you specify @all@ or @none@ for the value of @Forward@, omit
    -- @WhitelistedNames@. If you change the value of @Forward@ from
    -- @whitelist@ to @all@ or @none@ and you don\'t delete the
    -- @WhitelistedNames@ element and its child elements, CloudFront deletes
    -- them automatically.
    --
    -- For the current limit on the number of cookie names that you can
    -- whitelist for each cache behavior, see
    -- <https://docs.aws.amazon.com/general/latest/gr/xrefaws_service_limits.html#limits_cloudfront CloudFront Limits>
    -- in the /AWS General Reference/.
    whitelistedNames :: Prelude.Maybe CookieNames,
    -- | This field is deprecated. We recommend that you use a cache policy or an
    -- origin request policy instead of this field.
    --
    -- If you want to include cookies in the cache key, use a cache policy. For
    -- more information, see
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies>
    -- in the /Amazon CloudFront Developer Guide/.
    --
    -- If you want to send cookies to the origin but not include them in the
    -- cache key, use origin request policy. For more information, see
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-origin-requests.html#origin-request-create-origin-request-policy Creating origin request policies>
    -- in the /Amazon CloudFront Developer Guide/.
    --
    -- Specifies which cookies to forward to the origin for this cache
    -- behavior: all, none, or the list of cookies specified in the
    -- @WhitelistedNames@ complex type.
    --
    -- Amazon S3 doesn\'t process cookies. When the cache behavior is
    -- forwarding requests to an Amazon S3 origin, specify none for the
    -- @Forward@ element.
    forward :: ItemSelection
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CookiePreference' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'whitelistedNames', 'cookiePreference_whitelistedNames' - This field is deprecated. We recommend that you use a cache policy or an
-- origin request policy instead of this field.
--
-- If you want to include cookies in the cache key, use a cache policy. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies>
-- in the /Amazon CloudFront Developer Guide/.
--
-- If you want to send cookies to the origin but not include them in the
-- cache key, use an origin request policy. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-origin-requests.html#origin-request-create-origin-request-policy Creating origin request policies>
-- in the /Amazon CloudFront Developer Guide/.
--
-- Required if you specify @whitelist@ for the value of @Forward@. A
-- complex type that specifies how many different cookies you want
-- CloudFront to forward to the origin for this cache behavior and, if you
-- want to forward selected cookies, the names of those cookies.
--
-- If you specify @all@ or @none@ for the value of @Forward@, omit
-- @WhitelistedNames@. If you change the value of @Forward@ from
-- @whitelist@ to @all@ or @none@ and you don\'t delete the
-- @WhitelistedNames@ element and its child elements, CloudFront deletes
-- them automatically.
--
-- For the current limit on the number of cookie names that you can
-- whitelist for each cache behavior, see
-- <https://docs.aws.amazon.com/general/latest/gr/xrefaws_service_limits.html#limits_cloudfront CloudFront Limits>
-- in the /AWS General Reference/.
--
-- 'forward', 'cookiePreference_forward' - This field is deprecated. We recommend that you use a cache policy or an
-- origin request policy instead of this field.
--
-- If you want to include cookies in the cache key, use a cache policy. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies>
-- in the /Amazon CloudFront Developer Guide/.
--
-- If you want to send cookies to the origin but not include them in the
-- cache key, use origin request policy. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-origin-requests.html#origin-request-create-origin-request-policy Creating origin request policies>
-- in the /Amazon CloudFront Developer Guide/.
--
-- Specifies which cookies to forward to the origin for this cache
-- behavior: all, none, or the list of cookies specified in the
-- @WhitelistedNames@ complex type.
--
-- Amazon S3 doesn\'t process cookies. When the cache behavior is
-- forwarding requests to an Amazon S3 origin, specify none for the
-- @Forward@ element.
newCookiePreference ::
  -- | 'forward'
  ItemSelection ->
  CookiePreference
newCookiePreference pForward_ =
  CookiePreference'
    { whitelistedNames =
        Prelude.Nothing,
      forward = pForward_
    }

-- | This field is deprecated. We recommend that you use a cache policy or an
-- origin request policy instead of this field.
--
-- If you want to include cookies in the cache key, use a cache policy. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies>
-- in the /Amazon CloudFront Developer Guide/.
--
-- If you want to send cookies to the origin but not include them in the
-- cache key, use an origin request policy. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-origin-requests.html#origin-request-create-origin-request-policy Creating origin request policies>
-- in the /Amazon CloudFront Developer Guide/.
--
-- Required if you specify @whitelist@ for the value of @Forward@. A
-- complex type that specifies how many different cookies you want
-- CloudFront to forward to the origin for this cache behavior and, if you
-- want to forward selected cookies, the names of those cookies.
--
-- If you specify @all@ or @none@ for the value of @Forward@, omit
-- @WhitelistedNames@. If you change the value of @Forward@ from
-- @whitelist@ to @all@ or @none@ and you don\'t delete the
-- @WhitelistedNames@ element and its child elements, CloudFront deletes
-- them automatically.
--
-- For the current limit on the number of cookie names that you can
-- whitelist for each cache behavior, see
-- <https://docs.aws.amazon.com/general/latest/gr/xrefaws_service_limits.html#limits_cloudfront CloudFront Limits>
-- in the /AWS General Reference/.
cookiePreference_whitelistedNames :: Lens.Lens' CookiePreference (Prelude.Maybe CookieNames)
cookiePreference_whitelistedNames = Lens.lens (\CookiePreference' {whitelistedNames} -> whitelistedNames) (\s@CookiePreference' {} a -> s {whitelistedNames = a} :: CookiePreference)

-- | This field is deprecated. We recommend that you use a cache policy or an
-- origin request policy instead of this field.
--
-- If you want to include cookies in the cache key, use a cache policy. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-the-cache-key.html#cache-key-create-cache-policy Creating cache policies>
-- in the /Amazon CloudFront Developer Guide/.
--
-- If you want to send cookies to the origin but not include them in the
-- cache key, use origin request policy. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/controlling-origin-requests.html#origin-request-create-origin-request-policy Creating origin request policies>
-- in the /Amazon CloudFront Developer Guide/.
--
-- Specifies which cookies to forward to the origin for this cache
-- behavior: all, none, or the list of cookies specified in the
-- @WhitelistedNames@ complex type.
--
-- Amazon S3 doesn\'t process cookies. When the cache behavior is
-- forwarding requests to an Amazon S3 origin, specify none for the
-- @Forward@ element.
cookiePreference_forward :: Lens.Lens' CookiePreference ItemSelection
cookiePreference_forward = Lens.lens (\CookiePreference' {forward} -> forward) (\s@CookiePreference' {} a -> s {forward = a} :: CookiePreference)

instance Prelude.FromXML CookiePreference where
  parseXML x =
    CookiePreference'
      Prelude.<$> (x Prelude..@? "WhitelistedNames")
      Prelude.<*> (x Prelude..@ "Forward")

instance Prelude.Hashable CookiePreference

instance Prelude.NFData CookiePreference

instance Prelude.ToXML CookiePreference where
  toXML CookiePreference' {..} =
    Prelude.mconcat
      [ "WhitelistedNames" Prelude.@= whitelistedNames,
        "Forward" Prelude.@= forward
      ]
