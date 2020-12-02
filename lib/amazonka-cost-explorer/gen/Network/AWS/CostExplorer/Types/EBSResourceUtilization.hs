{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.EBSResourceUtilization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.EBSResourceUtilization where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The EBS field that contains a list of EBS metrics associated with the current instance.
--
--
--
-- /See:/ 'ebsResourceUtilization' smart constructor.
data EBSResourceUtilization = EBSResourceUtilization'
  { _eruEBSWriteBytesPerSecond ::
      !(Maybe Text),
    _eruEBSWriteOpsPerSecond :: !(Maybe Text),
    _eruEBSReadOpsPerSecond :: !(Maybe Text),
    _eruEBSReadBytesPerSecond :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EBSResourceUtilization' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eruEBSWriteBytesPerSecond' - The maximum size of write operations per second.
--
-- * 'eruEBSWriteOpsPerSecond' - The maximum number of write operations per second.
--
-- * 'eruEBSReadOpsPerSecond' - The maximum number of read operations per second.
--
-- * 'eruEBSReadBytesPerSecond' - The maximum size of read operations per second
ebsResourceUtilization ::
  EBSResourceUtilization
ebsResourceUtilization =
  EBSResourceUtilization'
    { _eruEBSWriteBytesPerSecond = Nothing,
      _eruEBSWriteOpsPerSecond = Nothing,
      _eruEBSReadOpsPerSecond = Nothing,
      _eruEBSReadBytesPerSecond = Nothing
    }

-- | The maximum size of write operations per second.
eruEBSWriteBytesPerSecond :: Lens' EBSResourceUtilization (Maybe Text)
eruEBSWriteBytesPerSecond = lens _eruEBSWriteBytesPerSecond (\s a -> s {_eruEBSWriteBytesPerSecond = a})

-- | The maximum number of write operations per second.
eruEBSWriteOpsPerSecond :: Lens' EBSResourceUtilization (Maybe Text)
eruEBSWriteOpsPerSecond = lens _eruEBSWriteOpsPerSecond (\s a -> s {_eruEBSWriteOpsPerSecond = a})

-- | The maximum number of read operations per second.
eruEBSReadOpsPerSecond :: Lens' EBSResourceUtilization (Maybe Text)
eruEBSReadOpsPerSecond = lens _eruEBSReadOpsPerSecond (\s a -> s {_eruEBSReadOpsPerSecond = a})

-- | The maximum size of read operations per second
eruEBSReadBytesPerSecond :: Lens' EBSResourceUtilization (Maybe Text)
eruEBSReadBytesPerSecond = lens _eruEBSReadBytesPerSecond (\s a -> s {_eruEBSReadBytesPerSecond = a})

instance FromJSON EBSResourceUtilization where
  parseJSON =
    withObject
      "EBSResourceUtilization"
      ( \x ->
          EBSResourceUtilization'
            <$> (x .:? "EbsWriteBytesPerSecond")
            <*> (x .:? "EbsWriteOpsPerSecond")
            <*> (x .:? "EbsReadOpsPerSecond")
            <*> (x .:? "EbsReadBytesPerSecond")
      )

instance Hashable EBSResourceUtilization

instance NFData EBSResourceUtilization
