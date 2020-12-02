{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ConfigSnapshotDeliveryProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ConfigSnapshotDeliveryProperties where

import Network.AWS.Config.Types.MaximumExecutionFrequency
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides options for how often AWS Config delivers configuration snapshots to the Amazon S3 bucket in your delivery channel.
--
--
-- The frequency for a rule that triggers evaluations for your resources when AWS Config delivers the configuration snapshot is set by one of two values, depending on which is less frequent:
--
--     * The value for the @deliveryFrequency@ parameter within the delivery channel configuration, which sets how often AWS Config delivers configuration snapshots. This value also sets how often AWS Config invokes evaluations for AWS Config rules.
--
--     * The value for the @MaximumExecutionFrequency@ parameter, which sets the maximum frequency with which AWS Config invokes evaluations for the rule. For more information, see 'ConfigRule' .
--
--
--
-- If the @deliveryFrequency@ value is less frequent than the @MaximumExecutionFrequency@ value for a rule, AWS Config invokes the rule only as often as the @deliveryFrequency@ value.
--
--     * For example, you want your rule to run evaluations when AWS Config delivers the configuration snapshot.
--
--     * You specify the @MaximumExecutionFrequency@ value for @Six_Hours@ .
--
--     * You then specify the delivery channel @deliveryFrequency@ value for @TwentyFour_Hours@ .
--
--     * Because the value for @deliveryFrequency@ is less frequent than @MaximumExecutionFrequency@ , AWS Config invokes evaluations for the rule every 24 hours.
--
--
--
-- You should set the @MaximumExecutionFrequency@ value to be at least as frequent as the @deliveryFrequency@ value. You can view the @deliveryFrequency@ value by using the @DescribeDeliveryChannnels@ action.
--
-- To update the @deliveryFrequency@ with which AWS Config delivers your configuration snapshots, use the @PutDeliveryChannel@ action.
--
--
-- /See:/ 'configSnapshotDeliveryProperties' smart constructor.
newtype ConfigSnapshotDeliveryProperties = ConfigSnapshotDeliveryProperties'
  { _csdpDeliveryFrequency ::
      Maybe
        MaximumExecutionFrequency
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ConfigSnapshotDeliveryProperties' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csdpDeliveryFrequency' - The frequency with which AWS Config delivers configuration snapshots.
configSnapshotDeliveryProperties ::
  ConfigSnapshotDeliveryProperties
configSnapshotDeliveryProperties =
  ConfigSnapshotDeliveryProperties'
    { _csdpDeliveryFrequency =
        Nothing
    }

-- | The frequency with which AWS Config delivers configuration snapshots.
csdpDeliveryFrequency :: Lens' ConfigSnapshotDeliveryProperties (Maybe MaximumExecutionFrequency)
csdpDeliveryFrequency = lens _csdpDeliveryFrequency (\s a -> s {_csdpDeliveryFrequency = a})

instance FromJSON ConfigSnapshotDeliveryProperties where
  parseJSON =
    withObject
      "ConfigSnapshotDeliveryProperties"
      ( \x ->
          ConfigSnapshotDeliveryProperties' <$> (x .:? "deliveryFrequency")
      )

instance Hashable ConfigSnapshotDeliveryProperties

instance NFData ConfigSnapshotDeliveryProperties

instance ToJSON ConfigSnapshotDeliveryProperties where
  toJSON ConfigSnapshotDeliveryProperties' {..} =
    object
      (catMaybes [("deliveryFrequency" .=) <$> _csdpDeliveryFrequency])
