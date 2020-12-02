{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.TransferData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.TransferData where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Data used to transfer a certificate to an AWS account.
--
--
--
-- /See:/ 'transferData' smart constructor.
data TransferData = TransferData'
  { _tdTransferDate ::
      !(Maybe POSIX),
    _tdAcceptDate :: !(Maybe POSIX),
    _tdTransferMessage :: !(Maybe Text),
    _tdRejectDate :: !(Maybe POSIX),
    _tdRejectReason :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TransferData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tdTransferDate' - The date the transfer took place.
--
-- * 'tdAcceptDate' - The date the transfer was accepted.
--
-- * 'tdTransferMessage' - The transfer message.
--
-- * 'tdRejectDate' - The date the transfer was rejected.
--
-- * 'tdRejectReason' - The reason why the transfer was rejected.
transferData ::
  TransferData
transferData =
  TransferData'
    { _tdTransferDate = Nothing,
      _tdAcceptDate = Nothing,
      _tdTransferMessage = Nothing,
      _tdRejectDate = Nothing,
      _tdRejectReason = Nothing
    }

-- | The date the transfer took place.
tdTransferDate :: Lens' TransferData (Maybe UTCTime)
tdTransferDate = lens _tdTransferDate (\s a -> s {_tdTransferDate = a}) . mapping _Time

-- | The date the transfer was accepted.
tdAcceptDate :: Lens' TransferData (Maybe UTCTime)
tdAcceptDate = lens _tdAcceptDate (\s a -> s {_tdAcceptDate = a}) . mapping _Time

-- | The transfer message.
tdTransferMessage :: Lens' TransferData (Maybe Text)
tdTransferMessage = lens _tdTransferMessage (\s a -> s {_tdTransferMessage = a})

-- | The date the transfer was rejected.
tdRejectDate :: Lens' TransferData (Maybe UTCTime)
tdRejectDate = lens _tdRejectDate (\s a -> s {_tdRejectDate = a}) . mapping _Time

-- | The reason why the transfer was rejected.
tdRejectReason :: Lens' TransferData (Maybe Text)
tdRejectReason = lens _tdRejectReason (\s a -> s {_tdRejectReason = a})

instance FromJSON TransferData where
  parseJSON =
    withObject
      "TransferData"
      ( \x ->
          TransferData'
            <$> (x .:? "transferDate")
            <*> (x .:? "acceptDate")
            <*> (x .:? "transferMessage")
            <*> (x .:? "rejectDate")
            <*> (x .:? "rejectReason")
      )

instance Hashable TransferData

instance NFData TransferData
