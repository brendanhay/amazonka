{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.RestoreSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.RestoreSummary where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains details for the restore.
--
--
--
-- /See:/ 'restoreSummary' smart constructor.
data RestoreSummary = RestoreSummary'
  { _rsSourceTableARN ::
      !(Maybe Text),
    _rsSourceBackupARN :: !(Maybe Text),
    _rsRestoreDateTime :: !POSIX,
    _rsRestoreInProgress :: !Bool
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RestoreSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rsSourceTableARN' - The ARN of the source table of the backup that is being restored.
--
-- * 'rsSourceBackupARN' - The Amazon Resource Name (ARN) of the backup from which the table was restored.
--
-- * 'rsRestoreDateTime' - Point in time or source backup time.
--
-- * 'rsRestoreInProgress' - Indicates if a restore is in progress or not.
restoreSummary ::
  -- | 'rsRestoreDateTime'
  UTCTime ->
  -- | 'rsRestoreInProgress'
  Bool ->
  RestoreSummary
restoreSummary pRestoreDateTime_ pRestoreInProgress_ =
  RestoreSummary'
    { _rsSourceTableARN = Nothing,
      _rsSourceBackupARN = Nothing,
      _rsRestoreDateTime = _Time # pRestoreDateTime_,
      _rsRestoreInProgress = pRestoreInProgress_
    }

-- | The ARN of the source table of the backup that is being restored.
rsSourceTableARN :: Lens' RestoreSummary (Maybe Text)
rsSourceTableARN = lens _rsSourceTableARN (\s a -> s {_rsSourceTableARN = a})

-- | The Amazon Resource Name (ARN) of the backup from which the table was restored.
rsSourceBackupARN :: Lens' RestoreSummary (Maybe Text)
rsSourceBackupARN = lens _rsSourceBackupARN (\s a -> s {_rsSourceBackupARN = a})

-- | Point in time or source backup time.
rsRestoreDateTime :: Lens' RestoreSummary UTCTime
rsRestoreDateTime = lens _rsRestoreDateTime (\s a -> s {_rsRestoreDateTime = a}) . _Time

-- | Indicates if a restore is in progress or not.
rsRestoreInProgress :: Lens' RestoreSummary Bool
rsRestoreInProgress = lens _rsRestoreInProgress (\s a -> s {_rsRestoreInProgress = a})

instance FromJSON RestoreSummary where
  parseJSON =
    withObject
      "RestoreSummary"
      ( \x ->
          RestoreSummary'
            <$> (x .:? "SourceTableArn")
            <*> (x .:? "SourceBackupArn")
            <*> (x .: "RestoreDateTime")
            <*> (x .: "RestoreInProgress")
      )

instance Hashable RestoreSummary

instance NFData RestoreSummary
