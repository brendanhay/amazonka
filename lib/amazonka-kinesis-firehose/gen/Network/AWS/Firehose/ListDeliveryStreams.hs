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
-- Module      : Network.AWS.Firehose.ListDeliveryStreams
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists your delivery streams.
--
--
-- The number of delivery streams might be too large to return using a single call to @ListDeliveryStreams@ . You can limit the number of delivery streams returned, using the __Limit__ parameter. To determine whether there are more delivery streams to list, check the value of @HasMoreDeliveryStreams@ in the output. If there are more delivery streams to list, you can request them by specifying the name of the last delivery stream returned in the call in the @ExclusiveStartDeliveryStreamName@ parameter of a subsequent call.
--
module Network.AWS.Firehose.ListDeliveryStreams
    (
    -- * Creating a Request
      listDeliveryStreams
    , ListDeliveryStreams
    -- * Request Lenses
    , ldsLimit
    , ldsDeliveryStreamType
    , ldsExclusiveStartDeliveryStreamName

    -- * Destructuring the Response
    , listDeliveryStreamsResponse
    , ListDeliveryStreamsResponse
    -- * Response Lenses
    , ldsrsResponseStatus
    , ldsrsDeliveryStreamNames
    , ldsrsHasMoreDeliveryStreams
    ) where

import Network.AWS.Firehose.Types
import Network.AWS.Firehose.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listDeliveryStreams' smart constructor.
data ListDeliveryStreams = ListDeliveryStreams'
  { _ldsLimit                            :: !(Maybe Nat)
  , _ldsDeliveryStreamType               :: !(Maybe DeliveryStreamType)
  , _ldsExclusiveStartDeliveryStreamName :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListDeliveryStreams' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldsLimit' - The maximum number of delivery streams to list. The default value is 10.
--
-- * 'ldsDeliveryStreamType' - The delivery stream type. This can be one of the following values:     * @DirectPut@ : Provider applications access the delivery stream directly.     * @KinesisStreamAsSource@ : The delivery stream uses a Kinesis data stream as a source. This parameter is optional. If this parameter is omitted, delivery streams of all types are returned.
--
-- * 'ldsExclusiveStartDeliveryStreamName' - The name of the delivery stream to start the list with.
listDeliveryStreams
    :: ListDeliveryStreams
listDeliveryStreams =
  ListDeliveryStreams'
    { _ldsLimit = Nothing
    , _ldsDeliveryStreamType = Nothing
    , _ldsExclusiveStartDeliveryStreamName = Nothing
    }


-- | The maximum number of delivery streams to list. The default value is 10.
ldsLimit :: Lens' ListDeliveryStreams (Maybe Natural)
ldsLimit = lens _ldsLimit (\ s a -> s{_ldsLimit = a}) . mapping _Nat

-- | The delivery stream type. This can be one of the following values:     * @DirectPut@ : Provider applications access the delivery stream directly.     * @KinesisStreamAsSource@ : The delivery stream uses a Kinesis data stream as a source. This parameter is optional. If this parameter is omitted, delivery streams of all types are returned.
ldsDeliveryStreamType :: Lens' ListDeliveryStreams (Maybe DeliveryStreamType)
ldsDeliveryStreamType = lens _ldsDeliveryStreamType (\ s a -> s{_ldsDeliveryStreamType = a})

-- | The name of the delivery stream to start the list with.
ldsExclusiveStartDeliveryStreamName :: Lens' ListDeliveryStreams (Maybe Text)
ldsExclusiveStartDeliveryStreamName = lens _ldsExclusiveStartDeliveryStreamName (\ s a -> s{_ldsExclusiveStartDeliveryStreamName = a})

instance AWSRequest ListDeliveryStreams where
        type Rs ListDeliveryStreams =
             ListDeliveryStreamsResponse
        request = postJSON firehose
        response
          = receiveJSON
              (\ s h x ->
                 ListDeliveryStreamsResponse' <$>
                   (pure (fromEnum s)) <*>
                     (x .?> "DeliveryStreamNames" .!@ mempty)
                     <*> (x .:> "HasMoreDeliveryStreams"))

instance Hashable ListDeliveryStreams where

instance NFData ListDeliveryStreams where

instance ToHeaders ListDeliveryStreams where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Firehose_20150804.ListDeliveryStreams" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListDeliveryStreams where
        toJSON ListDeliveryStreams'{..}
          = object
              (catMaybes
                 [("Limit" .=) <$> _ldsLimit,
                  ("DeliveryStreamType" .=) <$> _ldsDeliveryStreamType,
                  ("ExclusiveStartDeliveryStreamName" .=) <$>
                    _ldsExclusiveStartDeliveryStreamName])

instance ToPath ListDeliveryStreams where
        toPath = const "/"

instance ToQuery ListDeliveryStreams where
        toQuery = const mempty

-- | /See:/ 'listDeliveryStreamsResponse' smart constructor.
data ListDeliveryStreamsResponse = ListDeliveryStreamsResponse'
  { _ldsrsResponseStatus         :: !Int
  , _ldsrsDeliveryStreamNames    :: ![Text]
  , _ldsrsHasMoreDeliveryStreams :: !Bool
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListDeliveryStreamsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldsrsResponseStatus' - -- | The response status code.
--
-- * 'ldsrsDeliveryStreamNames' - The names of the delivery streams.
--
-- * 'ldsrsHasMoreDeliveryStreams' - Indicates whether there are more delivery streams available to list.
listDeliveryStreamsResponse
    :: Int -- ^ 'ldsrsResponseStatus'
    -> Bool -- ^ 'ldsrsHasMoreDeliveryStreams'
    -> ListDeliveryStreamsResponse
listDeliveryStreamsResponse pResponseStatus_ pHasMoreDeliveryStreams_ =
  ListDeliveryStreamsResponse'
    { _ldsrsResponseStatus = pResponseStatus_
    , _ldsrsDeliveryStreamNames = mempty
    , _ldsrsHasMoreDeliveryStreams = pHasMoreDeliveryStreams_
    }


-- | -- | The response status code.
ldsrsResponseStatus :: Lens' ListDeliveryStreamsResponse Int
ldsrsResponseStatus = lens _ldsrsResponseStatus (\ s a -> s{_ldsrsResponseStatus = a})

-- | The names of the delivery streams.
ldsrsDeliveryStreamNames :: Lens' ListDeliveryStreamsResponse [Text]
ldsrsDeliveryStreamNames = lens _ldsrsDeliveryStreamNames (\ s a -> s{_ldsrsDeliveryStreamNames = a}) . _Coerce

-- | Indicates whether there are more delivery streams available to list.
ldsrsHasMoreDeliveryStreams :: Lens' ListDeliveryStreamsResponse Bool
ldsrsHasMoreDeliveryStreams = lens _ldsrsHasMoreDeliveryStreams (\ s a -> s{_ldsrsHasMoreDeliveryStreams = a})

instance NFData ListDeliveryStreamsResponse where
