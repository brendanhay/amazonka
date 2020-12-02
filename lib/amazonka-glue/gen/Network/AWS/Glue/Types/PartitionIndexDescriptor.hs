{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.PartitionIndexDescriptor
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.PartitionIndexDescriptor where

import Network.AWS.Glue.Types.BackfillError
import Network.AWS.Glue.Types.KeySchemaElement
import Network.AWS.Glue.Types.PartitionIndexStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A descriptor for a partition index in a table.
--
--
--
-- /See:/ 'partitionIndexDescriptor' smart constructor.
data PartitionIndexDescriptor = PartitionIndexDescriptor'
  { _pidBackfillErrors ::
      !(Maybe [BackfillError]),
    _pidIndexName :: !Text,
    _pidKeys :: !(List1 KeySchemaElement),
    _pidIndexStatus :: !PartitionIndexStatus
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PartitionIndexDescriptor' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pidBackfillErrors' - A list of errors that can occur when registering partition indexes for an existing table.
--
-- * 'pidIndexName' - The name of the partition index.
--
-- * 'pidKeys' - A list of one or more keys, as @KeySchemaElement@ structures, for the partition index.
--
-- * 'pidIndexStatus' - The status of the partition index.  The possible statuses are:     * CREATING: The index is being created. When an index is in a CREATING state, the index or its table cannot be deleted.     * ACTIVE: The index creation succeeds.     * FAILED: The index creation fails.      * DELETING: The index is deleted from the list of indexes.
partitionIndexDescriptor ::
  -- | 'pidIndexName'
  Text ->
  -- | 'pidKeys'
  NonEmpty KeySchemaElement ->
  -- | 'pidIndexStatus'
  PartitionIndexStatus ->
  PartitionIndexDescriptor
partitionIndexDescriptor pIndexName_ pKeys_ pIndexStatus_ =
  PartitionIndexDescriptor'
    { _pidBackfillErrors = Nothing,
      _pidIndexName = pIndexName_,
      _pidKeys = _List1 # pKeys_,
      _pidIndexStatus = pIndexStatus_
    }

-- | A list of errors that can occur when registering partition indexes for an existing table.
pidBackfillErrors :: Lens' PartitionIndexDescriptor [BackfillError]
pidBackfillErrors = lens _pidBackfillErrors (\s a -> s {_pidBackfillErrors = a}) . _Default . _Coerce

-- | The name of the partition index.
pidIndexName :: Lens' PartitionIndexDescriptor Text
pidIndexName = lens _pidIndexName (\s a -> s {_pidIndexName = a})

-- | A list of one or more keys, as @KeySchemaElement@ structures, for the partition index.
pidKeys :: Lens' PartitionIndexDescriptor (NonEmpty KeySchemaElement)
pidKeys = lens _pidKeys (\s a -> s {_pidKeys = a}) . _List1

-- | The status of the partition index.  The possible statuses are:     * CREATING: The index is being created. When an index is in a CREATING state, the index or its table cannot be deleted.     * ACTIVE: The index creation succeeds.     * FAILED: The index creation fails.      * DELETING: The index is deleted from the list of indexes.
pidIndexStatus :: Lens' PartitionIndexDescriptor PartitionIndexStatus
pidIndexStatus = lens _pidIndexStatus (\s a -> s {_pidIndexStatus = a})

instance FromJSON PartitionIndexDescriptor where
  parseJSON =
    withObject
      "PartitionIndexDescriptor"
      ( \x ->
          PartitionIndexDescriptor'
            <$> (x .:? "BackfillErrors" .!= mempty)
            <*> (x .: "IndexName")
            <*> (x .: "Keys")
            <*> (x .: "IndexStatus")
      )

instance Hashable PartitionIndexDescriptor

instance NFData PartitionIndexDescriptor
