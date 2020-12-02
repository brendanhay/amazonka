{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.MappingEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.MappingEntry where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Defines a mapping.
--
--
--
-- /See:/ 'mappingEntry' smart constructor.
data MappingEntry = MappingEntry'
  { _meTargetTable :: !(Maybe Text),
    _meSourceType :: !(Maybe Text),
    _meSourceTable :: !(Maybe Text),
    _meTargetType :: !(Maybe Text),
    _meTargetPath :: !(Maybe Text),
    _meSourcePath :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MappingEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'meTargetTable' - The target table.
--
-- * 'meSourceType' - The source type.
--
-- * 'meSourceTable' - The name of the source table.
--
-- * 'meTargetType' - The target type.
--
-- * 'meTargetPath' - The target path.
--
-- * 'meSourcePath' - The source path.
mappingEntry ::
  MappingEntry
mappingEntry =
  MappingEntry'
    { _meTargetTable = Nothing,
      _meSourceType = Nothing,
      _meSourceTable = Nothing,
      _meTargetType = Nothing,
      _meTargetPath = Nothing,
      _meSourcePath = Nothing
    }

-- | The target table.
meTargetTable :: Lens' MappingEntry (Maybe Text)
meTargetTable = lens _meTargetTable (\s a -> s {_meTargetTable = a})

-- | The source type.
meSourceType :: Lens' MappingEntry (Maybe Text)
meSourceType = lens _meSourceType (\s a -> s {_meSourceType = a})

-- | The name of the source table.
meSourceTable :: Lens' MappingEntry (Maybe Text)
meSourceTable = lens _meSourceTable (\s a -> s {_meSourceTable = a})

-- | The target type.
meTargetType :: Lens' MappingEntry (Maybe Text)
meTargetType = lens _meTargetType (\s a -> s {_meTargetType = a})

-- | The target path.
meTargetPath :: Lens' MappingEntry (Maybe Text)
meTargetPath = lens _meTargetPath (\s a -> s {_meTargetPath = a})

-- | The source path.
meSourcePath :: Lens' MappingEntry (Maybe Text)
meSourcePath = lens _meSourcePath (\s a -> s {_meSourcePath = a})

instance FromJSON MappingEntry where
  parseJSON =
    withObject
      "MappingEntry"
      ( \x ->
          MappingEntry'
            <$> (x .:? "TargetTable")
            <*> (x .:? "SourceType")
            <*> (x .:? "SourceTable")
            <*> (x .:? "TargetType")
            <*> (x .:? "TargetPath")
            <*> (x .:? "SourcePath")
      )

instance Hashable MappingEntry

instance NFData MappingEntry

instance ToJSON MappingEntry where
  toJSON MappingEntry' {..} =
    object
      ( catMaybes
          [ ("TargetTable" .=) <$> _meTargetTable,
            ("SourceType" .=) <$> _meSourceType,
            ("SourceTable" .=) <$> _meSourceTable,
            ("TargetType" .=) <$> _meTargetType,
            ("TargetPath" .=) <$> _meTargetPath,
            ("SourcePath" .=) <$> _meSourcePath
          ]
      )
