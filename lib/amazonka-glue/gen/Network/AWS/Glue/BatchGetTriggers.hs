{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.BatchGetTriggers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of resource metadata for a given list of trigger names. After calling the @ListTriggers@ operation, you can call this operation to access the data to which you have been granted permissions. This operation supports all IAM permissions, including permission conditions that uses tags.
module Network.AWS.Glue.BatchGetTriggers
  ( -- * Creating a Request
    batchGetTriggers,
    BatchGetTriggers,

    -- * Request Lenses
    bgtTriggerNames,

    -- * Destructuring the Response
    batchGetTriggersResponse,
    BatchGetTriggersResponse,

    -- * Response Lenses
    bgtrsTriggersNotFound,
    bgtrsTriggers,
    bgtrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'batchGetTriggers' smart constructor.
newtype BatchGetTriggers = BatchGetTriggers'
  { _bgtTriggerNames ::
      [Text]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchGetTriggers' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bgtTriggerNames' - A list of trigger names, which may be the names returned from the @ListTriggers@ operation.
batchGetTriggers ::
  BatchGetTriggers
batchGetTriggers = BatchGetTriggers' {_bgtTriggerNames = mempty}

-- | A list of trigger names, which may be the names returned from the @ListTriggers@ operation.
bgtTriggerNames :: Lens' BatchGetTriggers [Text]
bgtTriggerNames = lens _bgtTriggerNames (\s a -> s {_bgtTriggerNames = a}) . _Coerce

instance AWSRequest BatchGetTriggers where
  type Rs BatchGetTriggers = BatchGetTriggersResponse
  request = postJSON glue
  response =
    receiveJSON
      ( \s h x ->
          BatchGetTriggersResponse'
            <$> (x .?> "TriggersNotFound" .!@ mempty)
            <*> (x .?> "Triggers" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable BatchGetTriggers

instance NFData BatchGetTriggers

instance ToHeaders BatchGetTriggers where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("AWSGlue.BatchGetTriggers" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON BatchGetTriggers where
  toJSON BatchGetTriggers' {..} =
    object (catMaybes [Just ("TriggerNames" .= _bgtTriggerNames)])

instance ToPath BatchGetTriggers where
  toPath = const "/"

instance ToQuery BatchGetTriggers where
  toQuery = const mempty

-- | /See:/ 'batchGetTriggersResponse' smart constructor.
data BatchGetTriggersResponse = BatchGetTriggersResponse'
  { _bgtrsTriggersNotFound ::
      !(Maybe [Text]),
    _bgtrsTriggers :: !(Maybe [Trigger]),
    _bgtrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchGetTriggersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bgtrsTriggersNotFound' - A list of names of triggers not found.
--
-- * 'bgtrsTriggers' - A list of trigger definitions.
--
-- * 'bgtrsResponseStatus' - -- | The response status code.
batchGetTriggersResponse ::
  -- | 'bgtrsResponseStatus'
  Int ->
  BatchGetTriggersResponse
batchGetTriggersResponse pResponseStatus_ =
  BatchGetTriggersResponse'
    { _bgtrsTriggersNotFound = Nothing,
      _bgtrsTriggers = Nothing,
      _bgtrsResponseStatus = pResponseStatus_
    }

-- | A list of names of triggers not found.
bgtrsTriggersNotFound :: Lens' BatchGetTriggersResponse [Text]
bgtrsTriggersNotFound = lens _bgtrsTriggersNotFound (\s a -> s {_bgtrsTriggersNotFound = a}) . _Default . _Coerce

-- | A list of trigger definitions.
bgtrsTriggers :: Lens' BatchGetTriggersResponse [Trigger]
bgtrsTriggers = lens _bgtrsTriggers (\s a -> s {_bgtrsTriggers = a}) . _Default . _Coerce

-- | -- | The response status code.
bgtrsResponseStatus :: Lens' BatchGetTriggersResponse Int
bgtrsResponseStatus = lens _bgtrsResponseStatus (\s a -> s {_bgtrsResponseStatus = a})

instance NFData BatchGetTriggersResponse
