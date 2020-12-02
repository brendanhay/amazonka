{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.OriginRequestPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.OriginRequestPolicy where

import Network.AWS.CloudFront.Types.OriginRequestPolicyConfig
import Network.AWS.Lens
import Network.AWS.Prelude

-- | An origin request policy.
--
--
-- When it’s attached to a cache behavior, the origin request policy determines the values that CloudFront includes in requests that it sends to the origin. Each request that CloudFront sends to the origin includes the following:
--
--     * The request body and the URL path (without the domain name) from the viewer request.
--
--     * The headers that CloudFront automatically includes in every origin request, including @Host@ , @User-Agent@ , and @X-Amz-Cf-Id@ .
--
--     * All HTTP headers, cookies, and URL query strings that are specified in the cache policy or the origin request policy. These can include items from the viewer request and, in the case of headers, additional ones that are added by CloudFront.
--
--
--
-- CloudFront sends a request when it can’t find an object in its cache that matches the request. If you want to send values to the origin and also include them in the cache key, use @CachePolicy@ .
--
--
-- /See:/ 'originRequestPolicy' smart constructor.
data OriginRequestPolicy = OriginRequestPolicy'
  { _orpId :: !Text,
    _orpLastModifiedTime :: !ISO8601,
    _orpOriginRequestPolicyConfig ::
      !OriginRequestPolicyConfig
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OriginRequestPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'orpId' - The unique identifier for the origin request policy.
--
-- * 'orpLastModifiedTime' - The date and time when the origin request policy was last modified.
--
-- * 'orpOriginRequestPolicyConfig' - The origin request policy configuration.
originRequestPolicy ::
  -- | 'orpId'
  Text ->
  -- | 'orpLastModifiedTime'
  UTCTime ->
  -- | 'orpOriginRequestPolicyConfig'
  OriginRequestPolicyConfig ->
  OriginRequestPolicy
originRequestPolicy
  pId_
  pLastModifiedTime_
  pOriginRequestPolicyConfig_ =
    OriginRequestPolicy'
      { _orpId = pId_,
        _orpLastModifiedTime = _Time # pLastModifiedTime_,
        _orpOriginRequestPolicyConfig = pOriginRequestPolicyConfig_
      }

-- | The unique identifier for the origin request policy.
orpId :: Lens' OriginRequestPolicy Text
orpId = lens _orpId (\s a -> s {_orpId = a})

-- | The date and time when the origin request policy was last modified.
orpLastModifiedTime :: Lens' OriginRequestPolicy UTCTime
orpLastModifiedTime = lens _orpLastModifiedTime (\s a -> s {_orpLastModifiedTime = a}) . _Time

-- | The origin request policy configuration.
orpOriginRequestPolicyConfig :: Lens' OriginRequestPolicy OriginRequestPolicyConfig
orpOriginRequestPolicyConfig = lens _orpOriginRequestPolicyConfig (\s a -> s {_orpOriginRequestPolicyConfig = a})

instance FromXML OriginRequestPolicy where
  parseXML x =
    OriginRequestPolicy'
      <$> (x .@ "Id")
      <*> (x .@ "LastModifiedTime")
      <*> (x .@ "OriginRequestPolicyConfig")

instance Hashable OriginRequestPolicy

instance NFData OriginRequestPolicy
