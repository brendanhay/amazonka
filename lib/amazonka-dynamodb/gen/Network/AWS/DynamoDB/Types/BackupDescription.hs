{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.BackupDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.BackupDescription where

import Network.AWS.DynamoDB.Types.BackupDetails
import Network.AWS.DynamoDB.Types.SourceTableDetails
import Network.AWS.DynamoDB.Types.SourceTableFeatureDetails
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains the description of the backup created for the table.
--
--
--
-- /See:/ 'backupDescription' smart constructor.
data BackupDescription = BackupDescription'
  { _bdBackupDetails ::
      !(Maybe BackupDetails),
    _bdSourceTableDetails :: !(Maybe SourceTableDetails),
    _bdSourceTableFeatureDetails ::
      !(Maybe SourceTableFeatureDetails)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BackupDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bdBackupDetails' - Contains the details of the backup created for the table.
--
-- * 'bdSourceTableDetails' - Contains the details of the table when the backup was created.
--
-- * 'bdSourceTableFeatureDetails' - Contains the details of the features enabled on the table when the backup was created. For example, LSIs, GSIs, streams, TTL.
backupDescription ::
  BackupDescription
backupDescription =
  BackupDescription'
    { _bdBackupDetails = Nothing,
      _bdSourceTableDetails = Nothing,
      _bdSourceTableFeatureDetails = Nothing
    }

-- | Contains the details of the backup created for the table.
bdBackupDetails :: Lens' BackupDescription (Maybe BackupDetails)
bdBackupDetails = lens _bdBackupDetails (\s a -> s {_bdBackupDetails = a})

-- | Contains the details of the table when the backup was created.
bdSourceTableDetails :: Lens' BackupDescription (Maybe SourceTableDetails)
bdSourceTableDetails = lens _bdSourceTableDetails (\s a -> s {_bdSourceTableDetails = a})

-- | Contains the details of the features enabled on the table when the backup was created. For example, LSIs, GSIs, streams, TTL.
bdSourceTableFeatureDetails :: Lens' BackupDescription (Maybe SourceTableFeatureDetails)
bdSourceTableFeatureDetails = lens _bdSourceTableFeatureDetails (\s a -> s {_bdSourceTableFeatureDetails = a})

instance FromJSON BackupDescription where
  parseJSON =
    withObject
      "BackupDescription"
      ( \x ->
          BackupDescription'
            <$> (x .:? "BackupDetails")
            <*> (x .:? "SourceTableDetails")
            <*> (x .:? "SourceTableFeatureDetails")
      )

instance Hashable BackupDescription

instance NFData BackupDescription
