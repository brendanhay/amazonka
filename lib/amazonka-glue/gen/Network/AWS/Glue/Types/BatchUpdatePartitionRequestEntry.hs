{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.BatchUpdatePartitionRequestEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.BatchUpdatePartitionRequestEntry where

import Network.AWS.Glue.Types.PartitionInput
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A structure that contains the values and structure used to update a partition.
--
--
--
-- /See:/ 'batchUpdatePartitionRequestEntry' smart constructor.
data BatchUpdatePartitionRequestEntry = BatchUpdatePartitionRequestEntry'
  { _buprePartitionValueList ::
      ![Text],
    _buprePartitionInput ::
      !PartitionInput
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchUpdatePartitionRequestEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'buprePartitionValueList' - A list of values defining the partitions.
--
-- * 'buprePartitionInput' - The structure used to update a partition.
batchUpdatePartitionRequestEntry ::
  -- | 'buprePartitionInput'
  PartitionInput ->
  BatchUpdatePartitionRequestEntry
batchUpdatePartitionRequestEntry pPartitionInput_ =
  BatchUpdatePartitionRequestEntry'
    { _buprePartitionValueList =
        mempty,
      _buprePartitionInput = pPartitionInput_
    }

-- | A list of values defining the partitions.
buprePartitionValueList :: Lens' BatchUpdatePartitionRequestEntry [Text]
buprePartitionValueList = lens _buprePartitionValueList (\s a -> s {_buprePartitionValueList = a}) . _Coerce

-- | The structure used to update a partition.
buprePartitionInput :: Lens' BatchUpdatePartitionRequestEntry PartitionInput
buprePartitionInput = lens _buprePartitionInput (\s a -> s {_buprePartitionInput = a})

instance Hashable BatchUpdatePartitionRequestEntry

instance NFData BatchUpdatePartitionRequestEntry

instance ToJSON BatchUpdatePartitionRequestEntry where
  toJSON BatchUpdatePartitionRequestEntry' {..} =
    object
      ( catMaybes
          [ Just ("PartitionValueList" .= _buprePartitionValueList),
            Just ("PartitionInput" .= _buprePartitionInput)
          ]
      )
