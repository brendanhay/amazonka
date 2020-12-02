{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.DeliveryChannelStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.DeliveryChannelStatus where

import Network.AWS.Config.Types.ConfigExportDeliveryInfo
import Network.AWS.Config.Types.ConfigStreamDeliveryInfo
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The status of a specified delivery channel.
--
--
-- Valid values: @Success@ | @Failure@
--
--
-- /See:/ 'deliveryChannelStatus' smart constructor.
data DeliveryChannelStatus = DeliveryChannelStatus'
  { _dcsConfigSnapshotDeliveryInfo ::
      !(Maybe ConfigExportDeliveryInfo),
    _dcsConfigStreamDeliveryInfo ::
      !(Maybe ConfigStreamDeliveryInfo),
    _dcsConfigHistoryDeliveryInfo ::
      !(Maybe ConfigExportDeliveryInfo),
    _dcsName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeliveryChannelStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcsConfigSnapshotDeliveryInfo' - A list containing the status of the delivery of the snapshot to the specified Amazon S3 bucket.
--
-- * 'dcsConfigStreamDeliveryInfo' - A list containing the status of the delivery of the configuration stream notification to the specified Amazon SNS topic.
--
-- * 'dcsConfigHistoryDeliveryInfo' - A list that contains the status of the delivery of the configuration history to the specified Amazon S3 bucket.
--
-- * 'dcsName' - The name of the delivery channel.
deliveryChannelStatus ::
  DeliveryChannelStatus
deliveryChannelStatus =
  DeliveryChannelStatus'
    { _dcsConfigSnapshotDeliveryInfo = Nothing,
      _dcsConfigStreamDeliveryInfo = Nothing,
      _dcsConfigHistoryDeliveryInfo = Nothing,
      _dcsName = Nothing
    }

-- | A list containing the status of the delivery of the snapshot to the specified Amazon S3 bucket.
dcsConfigSnapshotDeliveryInfo :: Lens' DeliveryChannelStatus (Maybe ConfigExportDeliveryInfo)
dcsConfigSnapshotDeliveryInfo = lens _dcsConfigSnapshotDeliveryInfo (\s a -> s {_dcsConfigSnapshotDeliveryInfo = a})

-- | A list containing the status of the delivery of the configuration stream notification to the specified Amazon SNS topic.
dcsConfigStreamDeliveryInfo :: Lens' DeliveryChannelStatus (Maybe ConfigStreamDeliveryInfo)
dcsConfigStreamDeliveryInfo = lens _dcsConfigStreamDeliveryInfo (\s a -> s {_dcsConfigStreamDeliveryInfo = a})

-- | A list that contains the status of the delivery of the configuration history to the specified Amazon S3 bucket.
dcsConfigHistoryDeliveryInfo :: Lens' DeliveryChannelStatus (Maybe ConfigExportDeliveryInfo)
dcsConfigHistoryDeliveryInfo = lens _dcsConfigHistoryDeliveryInfo (\s a -> s {_dcsConfigHistoryDeliveryInfo = a})

-- | The name of the delivery channel.
dcsName :: Lens' DeliveryChannelStatus (Maybe Text)
dcsName = lens _dcsName (\s a -> s {_dcsName = a})

instance FromJSON DeliveryChannelStatus where
  parseJSON =
    withObject
      "DeliveryChannelStatus"
      ( \x ->
          DeliveryChannelStatus'
            <$> (x .:? "configSnapshotDeliveryInfo")
            <*> (x .:? "configStreamDeliveryInfo")
            <*> (x .:? "configHistoryDeliveryInfo")
            <*> (x .:? "name")
      )

instance Hashable DeliveryChannelStatus

instance NFData DeliveryChannelStatus
