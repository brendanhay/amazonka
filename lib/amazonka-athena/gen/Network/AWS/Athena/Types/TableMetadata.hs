{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.Types.TableMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Athena.Types.TableMetadata where

import Network.AWS.Athena.Types.Column
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains metadata for a table.
--
--
--
-- /See:/ 'tableMetadata' smart constructor.
data TableMetadata = TableMetadata'
  { _tmTableType :: !(Maybe Text),
    _tmParameters :: !(Maybe (Map Text (Text))),
    _tmColumns :: !(Maybe [Column]),
    _tmLastAccessTime :: !(Maybe POSIX),
    _tmPartitionKeys :: !(Maybe [Column]),
    _tmCreateTime :: !(Maybe POSIX),
    _tmName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TableMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tmTableType' - The type of table. In Athena, only @EXTERNAL_TABLE@ is supported.
--
-- * 'tmParameters' - A set of custom key/value pairs for table properties.
--
-- * 'tmColumns' - A list of the columns in the table.
--
-- * 'tmLastAccessTime' - The last time the table was accessed.
--
-- * 'tmPartitionKeys' - A list of the partition keys in the table.
--
-- * 'tmCreateTime' - The time that the table was created.
--
-- * 'tmName' - The name of the table.
tableMetadata ::
  -- | 'tmName'
  Text ->
  TableMetadata
tableMetadata pName_ =
  TableMetadata'
    { _tmTableType = Nothing,
      _tmParameters = Nothing,
      _tmColumns = Nothing,
      _tmLastAccessTime = Nothing,
      _tmPartitionKeys = Nothing,
      _tmCreateTime = Nothing,
      _tmName = pName_
    }

-- | The type of table. In Athena, only @EXTERNAL_TABLE@ is supported.
tmTableType :: Lens' TableMetadata (Maybe Text)
tmTableType = lens _tmTableType (\s a -> s {_tmTableType = a})

-- | A set of custom key/value pairs for table properties.
tmParameters :: Lens' TableMetadata (HashMap Text (Text))
tmParameters = lens _tmParameters (\s a -> s {_tmParameters = a}) . _Default . _Map

-- | A list of the columns in the table.
tmColumns :: Lens' TableMetadata [Column]
tmColumns = lens _tmColumns (\s a -> s {_tmColumns = a}) . _Default . _Coerce

-- | The last time the table was accessed.
tmLastAccessTime :: Lens' TableMetadata (Maybe UTCTime)
tmLastAccessTime = lens _tmLastAccessTime (\s a -> s {_tmLastAccessTime = a}) . mapping _Time

-- | A list of the partition keys in the table.
tmPartitionKeys :: Lens' TableMetadata [Column]
tmPartitionKeys = lens _tmPartitionKeys (\s a -> s {_tmPartitionKeys = a}) . _Default . _Coerce

-- | The time that the table was created.
tmCreateTime :: Lens' TableMetadata (Maybe UTCTime)
tmCreateTime = lens _tmCreateTime (\s a -> s {_tmCreateTime = a}) . mapping _Time

-- | The name of the table.
tmName :: Lens' TableMetadata Text
tmName = lens _tmName (\s a -> s {_tmName = a})

instance FromJSON TableMetadata where
  parseJSON =
    withObject
      "TableMetadata"
      ( \x ->
          TableMetadata'
            <$> (x .:? "TableType")
            <*> (x .:? "Parameters" .!= mempty)
            <*> (x .:? "Columns" .!= mempty)
            <*> (x .:? "LastAccessTime")
            <*> (x .:? "PartitionKeys" .!= mempty)
            <*> (x .:? "CreateTime")
            <*> (x .: "Name")
      )

instance Hashable TableMetadata

instance NFData TableMetadata
