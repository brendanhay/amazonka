{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.BackfillError
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.BackfillError where

import Network.AWS.Glue.Types.BackfillErrorCode
import Network.AWS.Glue.Types.PartitionValueList
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A list of errors that can occur when registering partition indexes for an existing table.
--
--
-- These errors give the details about why an index registration failed and provide a limited number of partitions in the response, so that you can fix the partitions at fault and try registering the index again. The most common set of errors that can occur are categorized as follows:
--
--     * EncryptedPartitionError: The partitions are encrypted.
--
--     * InvalidPartitionTypeDataError: The partition value doesn't match the data type for that partition column.
--
--     * MissingPartitionValueError: The partitions are encrypted.
--
--     * UnsupportedPartitionCharacterError: Characters inside the partition value are not supported. For example: U+0000 , U+0001, U+0002.
--
--     * InternalError: Any error which does not belong to other error codes.
--
--
--
--
-- /See:/ 'backfillError' smart constructor.
data BackfillError = BackfillError'
  { _bePartitions ::
      !(Maybe [PartitionValueList]),
    _beCode :: !(Maybe BackfillErrorCode)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BackfillError' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bePartitions' - A list of a limited number of partitions in the response.
--
-- * 'beCode' - The error code for an error that occurred when registering partition indexes for an existing table.
backfillError ::
  BackfillError
backfillError =
  BackfillError' {_bePartitions = Nothing, _beCode = Nothing}

-- | A list of a limited number of partitions in the response.
bePartitions :: Lens' BackfillError [PartitionValueList]
bePartitions = lens _bePartitions (\s a -> s {_bePartitions = a}) . _Default . _Coerce

-- | The error code for an error that occurred when registering partition indexes for an existing table.
beCode :: Lens' BackfillError (Maybe BackfillErrorCode)
beCode = lens _beCode (\s a -> s {_beCode = a})

instance FromJSON BackfillError where
  parseJSON =
    withObject
      "BackfillError"
      ( \x ->
          BackfillError'
            <$> (x .:? "Partitions" .!= mempty) <*> (x .:? "Code")
      )

instance Hashable BackfillError

instance NFData BackfillError
