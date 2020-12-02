{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.BatchUpdatePartitionFailureEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.BatchUpdatePartitionFailureEntry where

import Network.AWS.Glue.Types.ErrorDetail
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about a batch update partition error.
--
--
--
-- /See:/ 'batchUpdatePartitionFailureEntry' smart constructor.
data BatchUpdatePartitionFailureEntry = BatchUpdatePartitionFailureEntry'
  { _bupfePartitionValueList ::
      !(Maybe [Text]),
    _bupfeErrorDetail ::
      !(Maybe ErrorDetail)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchUpdatePartitionFailureEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bupfePartitionValueList' - A list of values defining the partitions.
--
-- * 'bupfeErrorDetail' - The details about the batch update partition error.
batchUpdatePartitionFailureEntry ::
  BatchUpdatePartitionFailureEntry
batchUpdatePartitionFailureEntry =
  BatchUpdatePartitionFailureEntry'
    { _bupfePartitionValueList =
        Nothing,
      _bupfeErrorDetail = Nothing
    }

-- | A list of values defining the partitions.
bupfePartitionValueList :: Lens' BatchUpdatePartitionFailureEntry [Text]
bupfePartitionValueList = lens _bupfePartitionValueList (\s a -> s {_bupfePartitionValueList = a}) . _Default . _Coerce

-- | The details about the batch update partition error.
bupfeErrorDetail :: Lens' BatchUpdatePartitionFailureEntry (Maybe ErrorDetail)
bupfeErrorDetail = lens _bupfeErrorDetail (\s a -> s {_bupfeErrorDetail = a})

instance FromJSON BatchUpdatePartitionFailureEntry where
  parseJSON =
    withObject
      "BatchUpdatePartitionFailureEntry"
      ( \x ->
          BatchUpdatePartitionFailureEntry'
            <$> (x .:? "PartitionValueList" .!= mempty) <*> (x .:? "ErrorDetail")
      )

instance Hashable BatchUpdatePartitionFailureEntry

instance NFData BatchUpdatePartitionFailureEntry
