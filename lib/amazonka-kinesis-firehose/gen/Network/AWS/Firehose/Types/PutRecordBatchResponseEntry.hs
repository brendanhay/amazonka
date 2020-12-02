{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.PutRecordBatchResponseEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.PutRecordBatchResponseEntry where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains the result for an individual record from a 'PutRecordBatch' request. If the record is successfully added to your delivery stream, it receives a record ID. If the record fails to be added to your delivery stream, the result includes an error code and an error message.
--
--
--
-- /See:/ 'putRecordBatchResponseEntry' smart constructor.
data PutRecordBatchResponseEntry = PutRecordBatchResponseEntry'
  { _prbreRecordId ::
      !(Maybe Text),
    _prbreErrorCode :: !(Maybe Text),
    _prbreErrorMessage :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutRecordBatchResponseEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'prbreRecordId' - The ID of the record.
--
-- * 'prbreErrorCode' - The error code for an individual record result.
--
-- * 'prbreErrorMessage' - The error message for an individual record result.
putRecordBatchResponseEntry ::
  PutRecordBatchResponseEntry
putRecordBatchResponseEntry =
  PutRecordBatchResponseEntry'
    { _prbreRecordId = Nothing,
      _prbreErrorCode = Nothing,
      _prbreErrorMessage = Nothing
    }

-- | The ID of the record.
prbreRecordId :: Lens' PutRecordBatchResponseEntry (Maybe Text)
prbreRecordId = lens _prbreRecordId (\s a -> s {_prbreRecordId = a})

-- | The error code for an individual record result.
prbreErrorCode :: Lens' PutRecordBatchResponseEntry (Maybe Text)
prbreErrorCode = lens _prbreErrorCode (\s a -> s {_prbreErrorCode = a})

-- | The error message for an individual record result.
prbreErrorMessage :: Lens' PutRecordBatchResponseEntry (Maybe Text)
prbreErrorMessage = lens _prbreErrorMessage (\s a -> s {_prbreErrorMessage = a})

instance FromJSON PutRecordBatchResponseEntry where
  parseJSON =
    withObject
      "PutRecordBatchResponseEntry"
      ( \x ->
          PutRecordBatchResponseEntry'
            <$> (x .:? "RecordId")
            <*> (x .:? "ErrorCode")
            <*> (x .:? "ErrorMessage")
      )

instance Hashable PutRecordBatchResponseEntry

instance NFData PutRecordBatchResponseEntry
