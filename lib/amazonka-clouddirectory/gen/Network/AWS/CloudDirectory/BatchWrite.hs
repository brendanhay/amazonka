{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.BatchWrite
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Performs all the write operations in a batch. Either all the operations succeed or none.
--
--
module Network.AWS.CloudDirectory.BatchWrite
    (
    -- * Creating a Request
      batchWrite
    , BatchWrite
    -- * Request Lenses
    , bwDirectoryARN
    , bwOperations

    -- * Destructuring the Response
    , batchWriteResponse
    , BatchWriteResponse
    -- * Response Lenses
    , bwrsResponses
    , bwrsResponseStatus
    ) where

import Network.AWS.CloudDirectory.Types
import Network.AWS.CloudDirectory.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'batchWrite' smart constructor.
data BatchWrite = BatchWrite'
  { _bwDirectoryARN :: !Text
  , _bwOperations   :: ![BatchWriteOperation]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchWrite' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bwDirectoryARN' - The Amazon Resource Name (ARN) that is associated with the 'Directory' . For more information, see 'arns' .
--
-- * 'bwOperations' - A list of operations that are part of the batch.
batchWrite
    :: Text -- ^ 'bwDirectoryARN'
    -> BatchWrite
batchWrite pDirectoryARN_ =
  BatchWrite' {_bwDirectoryARN = pDirectoryARN_, _bwOperations = mempty}


-- | The Amazon Resource Name (ARN) that is associated with the 'Directory' . For more information, see 'arns' .
bwDirectoryARN :: Lens' BatchWrite Text
bwDirectoryARN = lens _bwDirectoryARN (\ s a -> s{_bwDirectoryARN = a})

-- | A list of operations that are part of the batch.
bwOperations :: Lens' BatchWrite [BatchWriteOperation]
bwOperations = lens _bwOperations (\ s a -> s{_bwOperations = a}) . _Coerce

instance AWSRequest BatchWrite where
        type Rs BatchWrite = BatchWriteResponse
        request = putJSON cloudDirectory
        response
          = receiveJSON
              (\ s h x ->
                 BatchWriteResponse' <$>
                   (x .?> "Responses" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable BatchWrite where

instance NFData BatchWrite where

instance ToHeaders BatchWrite where
        toHeaders BatchWrite'{..}
          = mconcat ["x-amz-data-partition" =# _bwDirectoryARN]

instance ToJSON BatchWrite where
        toJSON BatchWrite'{..}
          = object
              (catMaybes [Just ("Operations" .= _bwOperations)])

instance ToPath BatchWrite where
        toPath
          = const "/amazonclouddirectory/2017-01-11/batchwrite"

instance ToQuery BatchWrite where
        toQuery = const mempty

-- | /See:/ 'batchWriteResponse' smart constructor.
data BatchWriteResponse = BatchWriteResponse'
  { _bwrsResponses      :: !(Maybe [BatchWriteOperationResponse])
  , _bwrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchWriteResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bwrsResponses' - A list of all the responses for each batch write.
--
-- * 'bwrsResponseStatus' - -- | The response status code.
batchWriteResponse
    :: Int -- ^ 'bwrsResponseStatus'
    -> BatchWriteResponse
batchWriteResponse pResponseStatus_ =
  BatchWriteResponse'
    {_bwrsResponses = Nothing, _bwrsResponseStatus = pResponseStatus_}


-- | A list of all the responses for each batch write.
bwrsResponses :: Lens' BatchWriteResponse [BatchWriteOperationResponse]
bwrsResponses = lens _bwrsResponses (\ s a -> s{_bwrsResponses = a}) . _Default . _Coerce

-- | -- | The response status code.
bwrsResponseStatus :: Lens' BatchWriteResponse Int
bwrsResponseStatus = lens _bwrsResponseStatus (\ s a -> s{_bwrsResponseStatus = a})

instance NFData BatchWriteResponse where
