{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.DeltaSyncConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.DeltaSyncConfig where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a Delta Sync configuration.
--
--
--
-- /See:/ 'deltaSyncConfig' smart constructor.
data DeltaSyncConfig = DeltaSyncConfig'
  { _dscBaseTableTTL ::
      !(Maybe Integer),
    _dscDeltaSyncTableName :: !(Maybe Text),
    _dscDeltaSyncTableTTL :: !(Maybe Integer)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeltaSyncConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dscBaseTableTTL' - The number of minutes an Item is stored in the datasource.
--
-- * 'dscDeltaSyncTableName' - The Delta Sync table name.
--
-- * 'dscDeltaSyncTableTTL' - The number of minutes a Delta Sync log entry is stored in the Delta Sync table.
deltaSyncConfig ::
  DeltaSyncConfig
deltaSyncConfig =
  DeltaSyncConfig'
    { _dscBaseTableTTL = Nothing,
      _dscDeltaSyncTableName = Nothing,
      _dscDeltaSyncTableTTL = Nothing
    }

-- | The number of minutes an Item is stored in the datasource.
dscBaseTableTTL :: Lens' DeltaSyncConfig (Maybe Integer)
dscBaseTableTTL = lens _dscBaseTableTTL (\s a -> s {_dscBaseTableTTL = a})

-- | The Delta Sync table name.
dscDeltaSyncTableName :: Lens' DeltaSyncConfig (Maybe Text)
dscDeltaSyncTableName = lens _dscDeltaSyncTableName (\s a -> s {_dscDeltaSyncTableName = a})

-- | The number of minutes a Delta Sync log entry is stored in the Delta Sync table.
dscDeltaSyncTableTTL :: Lens' DeltaSyncConfig (Maybe Integer)
dscDeltaSyncTableTTL = lens _dscDeltaSyncTableTTL (\s a -> s {_dscDeltaSyncTableTTL = a})

instance FromJSON DeltaSyncConfig where
  parseJSON =
    withObject
      "DeltaSyncConfig"
      ( \x ->
          DeltaSyncConfig'
            <$> (x .:? "baseTableTTL")
            <*> (x .:? "deltaSyncTableName")
            <*> (x .:? "deltaSyncTableTTL")
      )

instance Hashable DeltaSyncConfig

instance NFData DeltaSyncConfig

instance ToJSON DeltaSyncConfig where
  toJSON DeltaSyncConfig' {..} =
    object
      ( catMaybes
          [ ("baseTableTTL" .=) <$> _dscBaseTableTTL,
            ("deltaSyncTableName" .=) <$> _dscDeltaSyncTableName,
            ("deltaSyncTableTTL" .=) <$> _dscDeltaSyncTableTTL
          ]
      )
