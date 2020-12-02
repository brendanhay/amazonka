{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplateSpotMarketOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplateSpotMarketOptions where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.InstanceInterruptionBehavior
import Network.AWS.EC2.Types.SpotInstanceType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The options for Spot Instances.
--
--
--
-- /See:/ 'launchTemplateSpotMarketOptions' smart constructor.
data LaunchTemplateSpotMarketOptions = LaunchTemplateSpotMarketOptions'
  { _ltsmoBlockDurationMinutes ::
      !(Maybe Int),
    _ltsmoInstanceInterruptionBehavior ::
      !( Maybe
           InstanceInterruptionBehavior
       ),
    _ltsmoValidUntil ::
      !(Maybe ISO8601),
    _ltsmoSpotInstanceType ::
      !(Maybe SpotInstanceType),
    _ltsmoMaxPrice ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LaunchTemplateSpotMarketOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltsmoBlockDurationMinutes' - The required duration for the Spot Instances (also known as Spot blocks), in minutes. This value must be a multiple of 60 (60, 120, 180, 240, 300, or 360).
--
-- * 'ltsmoInstanceInterruptionBehavior' - The behavior when a Spot Instance is interrupted.
--
-- * 'ltsmoValidUntil' - The end date of the request. For a one-time request, the request remains active until all instances launch, the request is canceled, or this date is reached. If the request is persistent, it remains active until it is canceled or this date and time is reached.
--
-- * 'ltsmoSpotInstanceType' - The Spot Instance request type.
--
-- * 'ltsmoMaxPrice' - The maximum hourly price you're willing to pay for the Spot Instances.
launchTemplateSpotMarketOptions ::
  LaunchTemplateSpotMarketOptions
launchTemplateSpotMarketOptions =
  LaunchTemplateSpotMarketOptions'
    { _ltsmoBlockDurationMinutes =
        Nothing,
      _ltsmoInstanceInterruptionBehavior = Nothing,
      _ltsmoValidUntil = Nothing,
      _ltsmoSpotInstanceType = Nothing,
      _ltsmoMaxPrice = Nothing
    }

-- | The required duration for the Spot Instances (also known as Spot blocks), in minutes. This value must be a multiple of 60 (60, 120, 180, 240, 300, or 360).
ltsmoBlockDurationMinutes :: Lens' LaunchTemplateSpotMarketOptions (Maybe Int)
ltsmoBlockDurationMinutes = lens _ltsmoBlockDurationMinutes (\s a -> s {_ltsmoBlockDurationMinutes = a})

-- | The behavior when a Spot Instance is interrupted.
ltsmoInstanceInterruptionBehavior :: Lens' LaunchTemplateSpotMarketOptions (Maybe InstanceInterruptionBehavior)
ltsmoInstanceInterruptionBehavior = lens _ltsmoInstanceInterruptionBehavior (\s a -> s {_ltsmoInstanceInterruptionBehavior = a})

-- | The end date of the request. For a one-time request, the request remains active until all instances launch, the request is canceled, or this date is reached. If the request is persistent, it remains active until it is canceled or this date and time is reached.
ltsmoValidUntil :: Lens' LaunchTemplateSpotMarketOptions (Maybe UTCTime)
ltsmoValidUntil = lens _ltsmoValidUntil (\s a -> s {_ltsmoValidUntil = a}) . mapping _Time

-- | The Spot Instance request type.
ltsmoSpotInstanceType :: Lens' LaunchTemplateSpotMarketOptions (Maybe SpotInstanceType)
ltsmoSpotInstanceType = lens _ltsmoSpotInstanceType (\s a -> s {_ltsmoSpotInstanceType = a})

-- | The maximum hourly price you're willing to pay for the Spot Instances.
ltsmoMaxPrice :: Lens' LaunchTemplateSpotMarketOptions (Maybe Text)
ltsmoMaxPrice = lens _ltsmoMaxPrice (\s a -> s {_ltsmoMaxPrice = a})

instance FromXML LaunchTemplateSpotMarketOptions where
  parseXML x =
    LaunchTemplateSpotMarketOptions'
      <$> (x .@? "blockDurationMinutes")
      <*> (x .@? "instanceInterruptionBehavior")
      <*> (x .@? "validUntil")
      <*> (x .@? "spotInstanceType")
      <*> (x .@? "maxPrice")

instance Hashable LaunchTemplateSpotMarketOptions

instance NFData LaunchTemplateSpotMarketOptions
