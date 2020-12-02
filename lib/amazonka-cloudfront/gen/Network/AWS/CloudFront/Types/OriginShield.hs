{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.OriginShield
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.OriginShield where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | CloudFront Origin Shield.
--
--
-- Using Origin Shield can help reduce the load on your origin. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/origin-shield.html Using Origin Shield> in the /Amazon CloudFront Developer Guide/ .
--
--
-- /See:/ 'originShield' smart constructor.
data OriginShield = OriginShield'
  { _osOriginShieldRegion ::
      !(Maybe Text),
    _osEnabled :: !Bool
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OriginShield' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'osOriginShieldRegion' - The AWS Region for Origin Shield. Specify the AWS Region that has the lowest latency to your origin. To specify a region, use the region code, not the region name. For example, specify the US East (Ohio) region as @us-east-2@ . When you enable CloudFront Origin Shield, you must specify the AWS Region for Origin Shield. For the list of AWS Regions that you can specify, and for help choosing the best Region for your origin, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/origin-shield.html#choose-origin-shield-region Choosing the AWS Region for Origin Shield> in the /Amazon CloudFront Developer Guide/ .
--
-- * 'osEnabled' - A flag that specifies whether Origin Shield is enabled. When it’s enabled, CloudFront routes all requests through Origin Shield, which can help protect your origin. When it’s disabled, CloudFront might send requests directly to your origin from multiple edge locations or regional edge caches.
originShield ::
  -- | 'osEnabled'
  Bool ->
  OriginShield
originShield pEnabled_ =
  OriginShield'
    { _osOriginShieldRegion = Nothing,
      _osEnabled = pEnabled_
    }

-- | The AWS Region for Origin Shield. Specify the AWS Region that has the lowest latency to your origin. To specify a region, use the region code, not the region name. For example, specify the US East (Ohio) region as @us-east-2@ . When you enable CloudFront Origin Shield, you must specify the AWS Region for Origin Shield. For the list of AWS Regions that you can specify, and for help choosing the best Region for your origin, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/origin-shield.html#choose-origin-shield-region Choosing the AWS Region for Origin Shield> in the /Amazon CloudFront Developer Guide/ .
osOriginShieldRegion :: Lens' OriginShield (Maybe Text)
osOriginShieldRegion = lens _osOriginShieldRegion (\s a -> s {_osOriginShieldRegion = a})

-- | A flag that specifies whether Origin Shield is enabled. When it’s enabled, CloudFront routes all requests through Origin Shield, which can help protect your origin. When it’s disabled, CloudFront might send requests directly to your origin from multiple edge locations or regional edge caches.
osEnabled :: Lens' OriginShield Bool
osEnabled = lens _osEnabled (\s a -> s {_osEnabled = a})

instance FromXML OriginShield where
  parseXML x =
    OriginShield'
      <$> (x .@? "OriginShieldRegion") <*> (x .@ "Enabled")

instance Hashable OriginShield

instance NFData OriginShield

instance ToXML OriginShield where
  toXML OriginShield' {..} =
    mconcat
      [ "OriginShieldRegion" @= _osOriginShieldRegion,
        "Enabled" @= _osEnabled
      ]
