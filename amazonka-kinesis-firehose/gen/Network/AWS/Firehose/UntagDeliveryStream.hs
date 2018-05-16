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
-- Module      : Network.AWS.Firehose.UntagDeliveryStream
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes tags from the specified delivery stream. Removed tags are deleted, and you can't recover them after this operation successfully completes.
--
--
-- If you specify a tag that doesn't exist, the operation ignores it.
--
-- This operation has a limit of five transactions per second per account.
--
module Network.AWS.Firehose.UntagDeliveryStream
    (
    -- * Creating a Request
      untagDeliveryStream
    , UntagDeliveryStream
    -- * Request Lenses
    , udsDeliveryStreamName
    , udsTagKeys

    -- * Destructuring the Response
    , untagDeliveryStreamResponse
    , UntagDeliveryStreamResponse
    -- * Response Lenses
    , udsrsResponseStatus
    ) where

import Network.AWS.Firehose.Types
import Network.AWS.Firehose.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'untagDeliveryStream' smart constructor.
data UntagDeliveryStream = UntagDeliveryStream'
  { _udsDeliveryStreamName :: !Text
  , _udsTagKeys            :: !(List1 Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UntagDeliveryStream' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'udsDeliveryStreamName' - The name of the delivery stream.
--
-- * 'udsTagKeys' - A list of tag keys. Each corresponding tag is removed from the delivery stream.
untagDeliveryStream
    :: Text -- ^ 'udsDeliveryStreamName'
    -> NonEmpty Text -- ^ 'udsTagKeys'
    -> UntagDeliveryStream
untagDeliveryStream pDeliveryStreamName_ pTagKeys_ =
  UntagDeliveryStream'
    { _udsDeliveryStreamName = pDeliveryStreamName_
    , _udsTagKeys = _List1 # pTagKeys_
    }


-- | The name of the delivery stream.
udsDeliveryStreamName :: Lens' UntagDeliveryStream Text
udsDeliveryStreamName = lens _udsDeliveryStreamName (\ s a -> s{_udsDeliveryStreamName = a})

-- | A list of tag keys. Each corresponding tag is removed from the delivery stream.
udsTagKeys :: Lens' UntagDeliveryStream (NonEmpty Text)
udsTagKeys = lens _udsTagKeys (\ s a -> s{_udsTagKeys = a}) . _List1

instance AWSRequest UntagDeliveryStream where
        type Rs UntagDeliveryStream =
             UntagDeliveryStreamResponse
        request = postJSON firehose
        response
          = receiveEmpty
              (\ s h x ->
                 UntagDeliveryStreamResponse' <$> (pure (fromEnum s)))

instance Hashable UntagDeliveryStream where

instance NFData UntagDeliveryStream where

instance ToHeaders UntagDeliveryStream where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Firehose_20150804.UntagDeliveryStream" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UntagDeliveryStream where
        toJSON UntagDeliveryStream'{..}
          = object
              (catMaybes
                 [Just
                    ("DeliveryStreamName" .= _udsDeliveryStreamName),
                  Just ("TagKeys" .= _udsTagKeys)])

instance ToPath UntagDeliveryStream where
        toPath = const "/"

instance ToQuery UntagDeliveryStream where
        toQuery = const mempty

-- | /See:/ 'untagDeliveryStreamResponse' smart constructor.
newtype UntagDeliveryStreamResponse = UntagDeliveryStreamResponse'
  { _udsrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UntagDeliveryStreamResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'udsrsResponseStatus' - -- | The response status code.
untagDeliveryStreamResponse
    :: Int -- ^ 'udsrsResponseStatus'
    -> UntagDeliveryStreamResponse
untagDeliveryStreamResponse pResponseStatus_ =
  UntagDeliveryStreamResponse' {_udsrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
udsrsResponseStatus :: Lens' UntagDeliveryStreamResponse Int
udsrsResponseStatus = lens _udsrsResponseStatus (\ s a -> s{_udsrsResponseStatus = a})

instance NFData UntagDeliveryStreamResponse where
