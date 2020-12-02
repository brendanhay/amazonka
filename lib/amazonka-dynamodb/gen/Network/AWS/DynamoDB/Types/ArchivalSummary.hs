{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ArchivalSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ArchivalSummary where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains details of a table archival operation.
--
--
--
-- /See:/ 'archivalSummary' smart constructor.
data ArchivalSummary = ArchivalSummary'
  { _asArchivalReason ::
      !(Maybe Text),
    _asArchivalDateTime :: !(Maybe POSIX),
    _asArchivalBackupARN :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ArchivalSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asArchivalReason' - The reason DynamoDB archived the table. Currently, the only possible value is:     * @INACCESSIBLE_ENCRYPTION_CREDENTIALS@ - The table was archived due to the table's AWS KMS key being inaccessible for more than seven days. An On-Demand backup was created at the archival time.
--
-- * 'asArchivalDateTime' - The date and time when table archival was initiated by DynamoDB, in UNIX epoch time format.
--
-- * 'asArchivalBackupARN' - The Amazon Resource Name (ARN) of the backup the table was archived to, when applicable in the archival reason. If you wish to restore this backup to the same table name, you will need to delete the original table.
archivalSummary ::
  ArchivalSummary
archivalSummary =
  ArchivalSummary'
    { _asArchivalReason = Nothing,
      _asArchivalDateTime = Nothing,
      _asArchivalBackupARN = Nothing
    }

-- | The reason DynamoDB archived the table. Currently, the only possible value is:     * @INACCESSIBLE_ENCRYPTION_CREDENTIALS@ - The table was archived due to the table's AWS KMS key being inaccessible for more than seven days. An On-Demand backup was created at the archival time.
asArchivalReason :: Lens' ArchivalSummary (Maybe Text)
asArchivalReason = lens _asArchivalReason (\s a -> s {_asArchivalReason = a})

-- | The date and time when table archival was initiated by DynamoDB, in UNIX epoch time format.
asArchivalDateTime :: Lens' ArchivalSummary (Maybe UTCTime)
asArchivalDateTime = lens _asArchivalDateTime (\s a -> s {_asArchivalDateTime = a}) . mapping _Time

-- | The Amazon Resource Name (ARN) of the backup the table was archived to, when applicable in the archival reason. If you wish to restore this backup to the same table name, you will need to delete the original table.
asArchivalBackupARN :: Lens' ArchivalSummary (Maybe Text)
asArchivalBackupARN = lens _asArchivalBackupARN (\s a -> s {_asArchivalBackupARN = a})

instance FromJSON ArchivalSummary where
  parseJSON =
    withObject
      "ArchivalSummary"
      ( \x ->
          ArchivalSummary'
            <$> (x .:? "ArchivalReason")
            <*> (x .:? "ArchivalDateTime")
            <*> (x .:? "ArchivalBackupArn")
      )

instance Hashable ArchivalSummary

instance NFData ArchivalSummary
