{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSMv2.Types.DestinationBackup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudHSMv2.Types.DestinationBackup where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about the backup that will be copied and created by the 'CopyBackupToRegion' operation.
--
--
--
-- /See:/ 'destinationBackup' smart constructor.
data DestinationBackup = DestinationBackup'
  { _dbSourceCluster ::
      !(Maybe Text),
    _dbSourceRegion :: !(Maybe Text),
    _dbSourceBackup :: !(Maybe Text),
    _dbCreateTimestamp :: !(Maybe POSIX)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DestinationBackup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dbSourceCluster' - The identifier (ID) of the cluster containing the source backup from which the new backup was copied.
--
-- * 'dbSourceRegion' - The AWS region that contains the source backup from which the new backup was copied.
--
-- * 'dbSourceBackup' - The identifier (ID) of the source backup from which the new backup was copied.
--
-- * 'dbCreateTimestamp' - The date and time when both the source backup was created.
destinationBackup ::
  DestinationBackup
destinationBackup =
  DestinationBackup'
    { _dbSourceCluster = Nothing,
      _dbSourceRegion = Nothing,
      _dbSourceBackup = Nothing,
      _dbCreateTimestamp = Nothing
    }

-- | The identifier (ID) of the cluster containing the source backup from which the new backup was copied.
dbSourceCluster :: Lens' DestinationBackup (Maybe Text)
dbSourceCluster = lens _dbSourceCluster (\s a -> s {_dbSourceCluster = a})

-- | The AWS region that contains the source backup from which the new backup was copied.
dbSourceRegion :: Lens' DestinationBackup (Maybe Text)
dbSourceRegion = lens _dbSourceRegion (\s a -> s {_dbSourceRegion = a})

-- | The identifier (ID) of the source backup from which the new backup was copied.
dbSourceBackup :: Lens' DestinationBackup (Maybe Text)
dbSourceBackup = lens _dbSourceBackup (\s a -> s {_dbSourceBackup = a})

-- | The date and time when both the source backup was created.
dbCreateTimestamp :: Lens' DestinationBackup (Maybe UTCTime)
dbCreateTimestamp = lens _dbCreateTimestamp (\s a -> s {_dbCreateTimestamp = a}) . mapping _Time

instance FromJSON DestinationBackup where
  parseJSON =
    withObject
      "DestinationBackup"
      ( \x ->
          DestinationBackup'
            <$> (x .:? "SourceCluster")
            <*> (x .:? "SourceRegion")
            <*> (x .:? "SourceBackup")
            <*> (x .:? "CreateTimestamp")
      )

instance Hashable DestinationBackup

instance NFData DestinationBackup
