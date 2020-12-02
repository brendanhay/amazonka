{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSMv2.Types.BackupRetentionPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudHSMv2.Types.BackupRetentionPolicy where

import Network.AWS.CloudHSMv2.Types.BackupRetentionType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A policy that defines the number of days to retain backups.
--
--
--
-- /See:/ 'backupRetentionPolicy' smart constructor.
data BackupRetentionPolicy = BackupRetentionPolicy'
  { _brpValue ::
      !(Maybe Text),
    _brpType :: !(Maybe BackupRetentionType)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BackupRetentionPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'brpValue' - Use a value between 7 - 379.
--
-- * 'brpType' - The type of backup retention policy. For the @DAYS@ type, the value is the number of days to retain backups.
backupRetentionPolicy ::
  BackupRetentionPolicy
backupRetentionPolicy =
  BackupRetentionPolicy' {_brpValue = Nothing, _brpType = Nothing}

-- | Use a value between 7 - 379.
brpValue :: Lens' BackupRetentionPolicy (Maybe Text)
brpValue = lens _brpValue (\s a -> s {_brpValue = a})

-- | The type of backup retention policy. For the @DAYS@ type, the value is the number of days to retain backups.
brpType :: Lens' BackupRetentionPolicy (Maybe BackupRetentionType)
brpType = lens _brpType (\s a -> s {_brpType = a})

instance FromJSON BackupRetentionPolicy where
  parseJSON =
    withObject
      "BackupRetentionPolicy"
      ( \x ->
          BackupRetentionPolicy' <$> (x .:? "Value") <*> (x .:? "Type")
      )

instance Hashable BackupRetentionPolicy

instance NFData BackupRetentionPolicy

instance ToJSON BackupRetentionPolicy where
  toJSON BackupRetentionPolicy' {..} =
    object
      (catMaybes [("Value" .=) <$> _brpValue, ("Type" .=) <$> _brpType])
