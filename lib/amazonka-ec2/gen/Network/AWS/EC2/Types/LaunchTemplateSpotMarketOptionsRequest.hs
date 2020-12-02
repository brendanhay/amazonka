{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplateSpotMarketOptionsRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplateSpotMarketOptionsRequest where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.InstanceInterruptionBehavior
import Network.AWS.EC2.Types.SpotInstanceType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The options for Spot Instances.
--
--
--
-- /See:/ 'launchTemplateSpotMarketOptionsRequest' smart constructor.
data LaunchTemplateSpotMarketOptionsRequest = LaunchTemplateSpotMarketOptionsRequest'
  { _ltsmorBlockDurationMinutes ::
      !(Maybe Int),
    _ltsmorInstanceInterruptionBehavior ::
      !( Maybe
           InstanceInterruptionBehavior
       ),
    _ltsmorValidUntil ::
      !( Maybe
           ISO8601
       ),
    _ltsmorSpotInstanceType ::
      !( Maybe
           SpotInstanceType
       ),
    _ltsmorMaxPrice ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LaunchTemplateSpotMarketOptionsRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltsmorBlockDurationMinutes' - The required duration for the Spot Instances (also known as Spot blocks), in minutes. This value must be a multiple of 60 (60, 120, 180, 240, 300, or 360).
--
-- * 'ltsmorInstanceInterruptionBehavior' - The behavior when a Spot Instance is interrupted. The default is @terminate@ .
--
-- * 'ltsmorValidUntil' - The end date of the request. For a one-time request, the request remains active until all instances launch, the request is canceled, or this date is reached. If the request is persistent, it remains active until it is canceled or this date and time is reached. The default end date is 7 days from the current date.
--
-- * 'ltsmorSpotInstanceType' - The Spot Instance request type.
--
-- * 'ltsmorMaxPrice' - The maximum hourly price you're willing to pay for the Spot Instances.
launchTemplateSpotMarketOptionsRequest ::
  LaunchTemplateSpotMarketOptionsRequest
launchTemplateSpotMarketOptionsRequest =
  LaunchTemplateSpotMarketOptionsRequest'
    { _ltsmorBlockDurationMinutes =
        Nothing,
      _ltsmorInstanceInterruptionBehavior = Nothing,
      _ltsmorValidUntil = Nothing,
      _ltsmorSpotInstanceType = Nothing,
      _ltsmorMaxPrice = Nothing
    }

-- | The required duration for the Spot Instances (also known as Spot blocks), in minutes. This value must be a multiple of 60 (60, 120, 180, 240, 300, or 360).
ltsmorBlockDurationMinutes :: Lens' LaunchTemplateSpotMarketOptionsRequest (Maybe Int)
ltsmorBlockDurationMinutes = lens _ltsmorBlockDurationMinutes (\s a -> s {_ltsmorBlockDurationMinutes = a})

-- | The behavior when a Spot Instance is interrupted. The default is @terminate@ .
ltsmorInstanceInterruptionBehavior :: Lens' LaunchTemplateSpotMarketOptionsRequest (Maybe InstanceInterruptionBehavior)
ltsmorInstanceInterruptionBehavior = lens _ltsmorInstanceInterruptionBehavior (\s a -> s {_ltsmorInstanceInterruptionBehavior = a})

-- | The end date of the request. For a one-time request, the request remains active until all instances launch, the request is canceled, or this date is reached. If the request is persistent, it remains active until it is canceled or this date and time is reached. The default end date is 7 days from the current date.
ltsmorValidUntil :: Lens' LaunchTemplateSpotMarketOptionsRequest (Maybe UTCTime)
ltsmorValidUntil = lens _ltsmorValidUntil (\s a -> s {_ltsmorValidUntil = a}) . mapping _Time

-- | The Spot Instance request type.
ltsmorSpotInstanceType :: Lens' LaunchTemplateSpotMarketOptionsRequest (Maybe SpotInstanceType)
ltsmorSpotInstanceType = lens _ltsmorSpotInstanceType (\s a -> s {_ltsmorSpotInstanceType = a})

-- | The maximum hourly price you're willing to pay for the Spot Instances.
ltsmorMaxPrice :: Lens' LaunchTemplateSpotMarketOptionsRequest (Maybe Text)
ltsmorMaxPrice = lens _ltsmorMaxPrice (\s a -> s {_ltsmorMaxPrice = a})

instance Hashable LaunchTemplateSpotMarketOptionsRequest

instance NFData LaunchTemplateSpotMarketOptionsRequest

instance ToQuery LaunchTemplateSpotMarketOptionsRequest where
  toQuery LaunchTemplateSpotMarketOptionsRequest' {..} =
    mconcat
      [ "BlockDurationMinutes" =: _ltsmorBlockDurationMinutes,
        "InstanceInterruptionBehavior"
          =: _ltsmorInstanceInterruptionBehavior,
        "ValidUntil" =: _ltsmorValidUntil,
        "SpotInstanceType" =: _ltsmorSpotInstanceType,
        "MaxPrice" =: _ltsmorMaxPrice
      ]
