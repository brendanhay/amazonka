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
-- Module      : Network.AWS.Firehose.TagDeliveryStream
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or updates tags for the specified delivery stream. A tag is a key-value pair (the value is optional) that you can define and assign to AWS resources. If you specify a tag that already exists, the tag value is replaced with the value that you specify in the request. Tags are metadata. For example, you can add friendly names and descriptions or other types of information that can help you distinguish the delivery stream. For more information about tags, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ .
--
--
-- Each delivery stream can have up to 50 tags.
--
-- This operation has a limit of five transactions per second per account.
--
module Network.AWS.Firehose.TagDeliveryStream
    (
    -- * Creating a Request
      tagDeliveryStream
    , TagDeliveryStream
    -- * Request Lenses
    , tdsDeliveryStreamName
    , tdsTags

    -- * Destructuring the Response
    , tagDeliveryStreamResponse
    , TagDeliveryStreamResponse
    -- * Response Lenses
    , tdsrsResponseStatus
    ) where

import Network.AWS.Firehose.Types
import Network.AWS.Firehose.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'tagDeliveryStream' smart constructor.
data TagDeliveryStream = TagDeliveryStream'
  { _tdsDeliveryStreamName :: !Text
  , _tdsTags               :: !(List1 Tag)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TagDeliveryStream' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tdsDeliveryStreamName' - The name of the delivery stream to which you want to add the tags.
--
-- * 'tdsTags' - A set of key-value pairs to use to create the tags.
tagDeliveryStream
    :: Text -- ^ 'tdsDeliveryStreamName'
    -> NonEmpty Tag -- ^ 'tdsTags'
    -> TagDeliveryStream
tagDeliveryStream pDeliveryStreamName_ pTags_ =
  TagDeliveryStream'
    {_tdsDeliveryStreamName = pDeliveryStreamName_, _tdsTags = _List1 # pTags_}


-- | The name of the delivery stream to which you want to add the tags.
tdsDeliveryStreamName :: Lens' TagDeliveryStream Text
tdsDeliveryStreamName = lens _tdsDeliveryStreamName (\ s a -> s{_tdsDeliveryStreamName = a})

-- | A set of key-value pairs to use to create the tags.
tdsTags :: Lens' TagDeliveryStream (NonEmpty Tag)
tdsTags = lens _tdsTags (\ s a -> s{_tdsTags = a}) . _List1

instance AWSRequest TagDeliveryStream where
        type Rs TagDeliveryStream = TagDeliveryStreamResponse
        request = postJSON firehose
        response
          = receiveEmpty
              (\ s h x ->
                 TagDeliveryStreamResponse' <$> (pure (fromEnum s)))

instance Hashable TagDeliveryStream where

instance NFData TagDeliveryStream where

instance ToHeaders TagDeliveryStream where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Firehose_20150804.TagDeliveryStream" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON TagDeliveryStream where
        toJSON TagDeliveryStream'{..}
          = object
              (catMaybes
                 [Just
                    ("DeliveryStreamName" .= _tdsDeliveryStreamName),
                  Just ("Tags" .= _tdsTags)])

instance ToPath TagDeliveryStream where
        toPath = const "/"

instance ToQuery TagDeliveryStream where
        toQuery = const mempty

-- | /See:/ 'tagDeliveryStreamResponse' smart constructor.
newtype TagDeliveryStreamResponse = TagDeliveryStreamResponse'
  { _tdsrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TagDeliveryStreamResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tdsrsResponseStatus' - -- | The response status code.
tagDeliveryStreamResponse
    :: Int -- ^ 'tdsrsResponseStatus'
    -> TagDeliveryStreamResponse
tagDeliveryStreamResponse pResponseStatus_ =
  TagDeliveryStreamResponse' {_tdsrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
tdsrsResponseStatus :: Lens' TagDeliveryStreamResponse Int
tdsrsResponseStatus = lens _tdsrsResponseStatus (\ s a -> s{_tdsrsResponseStatus = a})

instance NFData TagDeliveryStreamResponse where
