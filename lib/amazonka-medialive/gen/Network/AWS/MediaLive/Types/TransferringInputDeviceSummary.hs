{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.TransferringInputDeviceSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.TransferringInputDeviceSummary where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.InputDeviceTransferType
import Network.AWS.Prelude

-- | Details about the input device that is being transferred.
--
-- /See:/ 'transferringInputDeviceSummary' smart constructor.
data TransferringInputDeviceSummary = TransferringInputDeviceSummary'
  { _tidsTransferType ::
      !( Maybe
           InputDeviceTransferType
       ),
    _tidsId :: !(Maybe Text),
    _tidsTargetCustomerId ::
      !(Maybe Text),
    _tidsMessage :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TransferringInputDeviceSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tidsTransferType' - The type (direction) of the input device transfer.
--
-- * 'tidsId' - The unique ID of the input device.
--
-- * 'tidsTargetCustomerId' - The AWS account ID for the recipient of the input device transfer.
--
-- * 'tidsMessage' - The optional message that the sender has attached to the transfer.
transferringInputDeviceSummary ::
  TransferringInputDeviceSummary
transferringInputDeviceSummary =
  TransferringInputDeviceSummary'
    { _tidsTransferType = Nothing,
      _tidsId = Nothing,
      _tidsTargetCustomerId = Nothing,
      _tidsMessage = Nothing
    }

-- | The type (direction) of the input device transfer.
tidsTransferType :: Lens' TransferringInputDeviceSummary (Maybe InputDeviceTransferType)
tidsTransferType = lens _tidsTransferType (\s a -> s {_tidsTransferType = a})

-- | The unique ID of the input device.
tidsId :: Lens' TransferringInputDeviceSummary (Maybe Text)
tidsId = lens _tidsId (\s a -> s {_tidsId = a})

-- | The AWS account ID for the recipient of the input device transfer.
tidsTargetCustomerId :: Lens' TransferringInputDeviceSummary (Maybe Text)
tidsTargetCustomerId = lens _tidsTargetCustomerId (\s a -> s {_tidsTargetCustomerId = a})

-- | The optional message that the sender has attached to the transfer.
tidsMessage :: Lens' TransferringInputDeviceSummary (Maybe Text)
tidsMessage = lens _tidsMessage (\s a -> s {_tidsMessage = a})

instance FromJSON TransferringInputDeviceSummary where
  parseJSON =
    withObject
      "TransferringInputDeviceSummary"
      ( \x ->
          TransferringInputDeviceSummary'
            <$> (x .:? "transferType")
            <*> (x .:? "id")
            <*> (x .:? "targetCustomerId")
            <*> (x .:? "message")
      )

instance Hashable TransferringInputDeviceSummary

instance NFData TransferringInputDeviceSummary
