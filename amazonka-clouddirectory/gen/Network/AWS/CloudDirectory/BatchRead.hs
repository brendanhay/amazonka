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
-- Module      : Network.AWS.CloudDirectory.BatchRead
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Performs all the read operations in a batch.
--
--
module Network.AWS.CloudDirectory.BatchRead
    (
    -- * Creating a Request
      batchRead
    , BatchRead
    -- * Request Lenses
    , brConsistencyLevel
    , brDirectoryARN
    , brOperations

    -- * Destructuring the Response
    , batchReadResponse
    , BatchReadResponse
    -- * Response Lenses
    , brrsResponses
    , brrsResponseStatus
    ) where

import Network.AWS.CloudDirectory.Types
import Network.AWS.CloudDirectory.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'batchRead' smart constructor.
data BatchRead = BatchRead'
  { _brConsistencyLevel :: !(Maybe ConsistencyLevel)
  , _brDirectoryARN     :: !Text
  , _brOperations       :: ![BatchReadOperation]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchRead' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'brConsistencyLevel' - Represents the manner and timing in which the successful write or update of an object is reflected in a subsequent read operation of that same object.
--
-- * 'brDirectoryARN' - The Amazon Resource Name (ARN) that is associated with the 'Directory' . For more information, see 'arns' .
--
-- * 'brOperations' - A list of operations that are part of the batch.
batchRead
    :: Text -- ^ 'brDirectoryARN'
    -> BatchRead
batchRead pDirectoryARN_ =
  BatchRead'
    { _brConsistencyLevel = Nothing
    , _brDirectoryARN = pDirectoryARN_
    , _brOperations = mempty
    }


-- | Represents the manner and timing in which the successful write or update of an object is reflected in a subsequent read operation of that same object.
brConsistencyLevel :: Lens' BatchRead (Maybe ConsistencyLevel)
brConsistencyLevel = lens _brConsistencyLevel (\ s a -> s{_brConsistencyLevel = a})

-- | The Amazon Resource Name (ARN) that is associated with the 'Directory' . For more information, see 'arns' .
brDirectoryARN :: Lens' BatchRead Text
brDirectoryARN = lens _brDirectoryARN (\ s a -> s{_brDirectoryARN = a})

-- | A list of operations that are part of the batch.
brOperations :: Lens' BatchRead [BatchReadOperation]
brOperations = lens _brOperations (\ s a -> s{_brOperations = a}) . _Coerce

instance AWSRequest BatchRead where
        type Rs BatchRead = BatchReadResponse
        request = postJSON cloudDirectory
        response
          = receiveJSON
              (\ s h x ->
                 BatchReadResponse' <$>
                   (x .?> "Responses" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable BatchRead where

instance NFData BatchRead where

instance ToHeaders BatchRead where
        toHeaders BatchRead'{..}
          = mconcat
              ["x-amz-consistency-level" =# _brConsistencyLevel,
               "x-amz-data-partition" =# _brDirectoryARN]

instance ToJSON BatchRead where
        toJSON BatchRead'{..}
          = object
              (catMaybes [Just ("Operations" .= _brOperations)])

instance ToPath BatchRead where
        toPath
          = const "/amazonclouddirectory/2017-01-11/batchread"

instance ToQuery BatchRead where
        toQuery = const mempty

-- | /See:/ 'batchReadResponse' smart constructor.
data BatchReadResponse = BatchReadResponse'
  { _brrsResponses      :: !(Maybe [BatchReadOperationResponse])
  , _brrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchReadResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'brrsResponses' - A list of all the responses for each batch read.
--
-- * 'brrsResponseStatus' - -- | The response status code.
batchReadResponse
    :: Int -- ^ 'brrsResponseStatus'
    -> BatchReadResponse
batchReadResponse pResponseStatus_ =
  BatchReadResponse'
    {_brrsResponses = Nothing, _brrsResponseStatus = pResponseStatus_}


-- | A list of all the responses for each batch read.
brrsResponses :: Lens' BatchReadResponse [BatchReadOperationResponse]
brrsResponses = lens _brrsResponses (\ s a -> s{_brrsResponses = a}) . _Default . _Coerce

-- | -- | The response status code.
brrsResponseStatus :: Lens' BatchReadResponse Int
brrsResponseStatus = lens _brrsResponseStatus (\ s a -> s{_brrsResponseStatus = a})

instance NFData BatchReadResponse where
