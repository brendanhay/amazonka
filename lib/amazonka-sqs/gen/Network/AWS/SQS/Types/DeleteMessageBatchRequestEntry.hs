{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SQS.Types.DeleteMessageBatchRequestEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SQS.Types.DeleteMessageBatchRequestEntry where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Encloses a receipt handle and an identifier for it.
--
--
--
-- /See:/ 'deleteMessageBatchRequestEntry' smart constructor.
data DeleteMessageBatchRequestEntry = DeleteMessageBatchRequestEntry'
  { _dmbreId ::
      !Text,
    _dmbreReceiptHandle :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteMessageBatchRequestEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmbreId' - An identifier for this particular receipt handle. This is used to communicate the result.
--
-- * 'dmbreReceiptHandle' - A receipt handle.
deleteMessageBatchRequestEntry ::
  -- | 'dmbreId'
  Text ->
  -- | 'dmbreReceiptHandle'
  Text ->
  DeleteMessageBatchRequestEntry
deleteMessageBatchRequestEntry pId_ pReceiptHandle_ =
  DeleteMessageBatchRequestEntry'
    { _dmbreId = pId_,
      _dmbreReceiptHandle = pReceiptHandle_
    }

-- | An identifier for this particular receipt handle. This is used to communicate the result.
dmbreId :: Lens' DeleteMessageBatchRequestEntry Text
dmbreId = lens _dmbreId (\s a -> s {_dmbreId = a})

-- | A receipt handle.
dmbreReceiptHandle :: Lens' DeleteMessageBatchRequestEntry Text
dmbreReceiptHandle = lens _dmbreReceiptHandle (\s a -> s {_dmbreReceiptHandle = a})

instance Hashable DeleteMessageBatchRequestEntry

instance NFData DeleteMessageBatchRequestEntry

instance ToQuery DeleteMessageBatchRequestEntry where
  toQuery DeleteMessageBatchRequestEntry' {..} =
    mconcat
      ["Id" =: _dmbreId, "ReceiptHandle" =: _dmbreReceiptHandle]
