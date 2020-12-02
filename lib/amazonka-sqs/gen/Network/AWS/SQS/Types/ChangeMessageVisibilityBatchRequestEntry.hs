{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SQS.Types.ChangeMessageVisibilityBatchRequestEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SQS.Types.ChangeMessageVisibilityBatchRequestEntry where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Encloses a receipt handle and an entry id for each message in @'ChangeMessageVisibilityBatch' .@
--
--
-- /Important:/ All of the following list parameters must be prefixed with @ChangeMessageVisibilityBatchRequestEntry.n@ , where @n@ is an integer value starting with @1@ . For example, a parameter list for this action might look like this:
--
-- @&ChangeMessageVisibilityBatchRequestEntry.1.Id=change_visibility_msg_2@
--
-- @&ChangeMessageVisibilityBatchRequestEntry.1.ReceiptHandle=your_receipt_handle@
--
-- @&ChangeMessageVisibilityBatchRequestEntry.1.VisibilityTimeout=45@
--
--
-- /See:/ 'changeMessageVisibilityBatchRequestEntry' smart constructor.
data ChangeMessageVisibilityBatchRequestEntry = ChangeMessageVisibilityBatchRequestEntry'
  { _cVisibilityTimeout ::
      !( Maybe
           Int
       ),
    _cId ::
      !Text,
    _cReceiptHandle ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ChangeMessageVisibilityBatchRequestEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cVisibilityTimeout' - The new value (in seconds) for the message's visibility timeout.
--
-- * 'cId' - An identifier for this particular receipt handle used to communicate the result.
--
-- * 'cReceiptHandle' - A receipt handle.
changeMessageVisibilityBatchRequestEntry ::
  -- | 'cId'
  Text ->
  -- | 'cReceiptHandle'
  Text ->
  ChangeMessageVisibilityBatchRequestEntry
changeMessageVisibilityBatchRequestEntry pId_ pReceiptHandle_ =
  ChangeMessageVisibilityBatchRequestEntry'
    { _cVisibilityTimeout =
        Nothing,
      _cId = pId_,
      _cReceiptHandle = pReceiptHandle_
    }

-- | The new value (in seconds) for the message's visibility timeout.
cVisibilityTimeout :: Lens' ChangeMessageVisibilityBatchRequestEntry (Maybe Int)
cVisibilityTimeout = lens _cVisibilityTimeout (\s a -> s {_cVisibilityTimeout = a})

-- | An identifier for this particular receipt handle used to communicate the result.
cId :: Lens' ChangeMessageVisibilityBatchRequestEntry Text
cId = lens _cId (\s a -> s {_cId = a})

-- | A receipt handle.
cReceiptHandle :: Lens' ChangeMessageVisibilityBatchRequestEntry Text
cReceiptHandle = lens _cReceiptHandle (\s a -> s {_cReceiptHandle = a})

instance Hashable ChangeMessageVisibilityBatchRequestEntry

instance NFData ChangeMessageVisibilityBatchRequestEntry

instance ToQuery ChangeMessageVisibilityBatchRequestEntry where
  toQuery ChangeMessageVisibilityBatchRequestEntry' {..} =
    mconcat
      [ "VisibilityTimeout" =: _cVisibilityTimeout,
        "Id" =: _cId,
        "ReceiptHandle" =: _cReceiptHandle
      ]
