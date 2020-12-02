{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.PartitionIndex
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.PartitionIndex where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A structure for a partition index.
--
--
--
-- /See:/ 'partitionIndex' smart constructor.
data PartitionIndex = PartitionIndex'
  { _piKeys :: !(List1 Text),
    _piIndexName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PartitionIndex' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'piKeys' - The keys for the partition index.
--
-- * 'piIndexName' - The name of the partition index.
partitionIndex ::
  -- | 'piKeys'
  NonEmpty Text ->
  -- | 'piIndexName'
  Text ->
  PartitionIndex
partitionIndex pKeys_ pIndexName_ =
  PartitionIndex'
    { _piKeys = _List1 # pKeys_,
      _piIndexName = pIndexName_
    }

-- | The keys for the partition index.
piKeys :: Lens' PartitionIndex (NonEmpty Text)
piKeys = lens _piKeys (\s a -> s {_piKeys = a}) . _List1

-- | The name of the partition index.
piIndexName :: Lens' PartitionIndex Text
piIndexName = lens _piIndexName (\s a -> s {_piIndexName = a})

instance Hashable PartitionIndex

instance NFData PartitionIndex

instance ToJSON PartitionIndex where
  toJSON PartitionIndex' {..} =
    object
      ( catMaybes
          [Just ("Keys" .= _piKeys), Just ("IndexName" .= _piIndexName)]
      )
