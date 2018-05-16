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
-- Module      : Network.AWS.KinesisVideo.UntagStream
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes one or more tags from a stream. In the request, specify only a tag key or keys; don't specify the value. If you specify a tag key that does not exist, it's ignored.
--
--
-- In the request, you must provide the @StreamName@ or @StreamARN@ .
--
module Network.AWS.KinesisVideo.UntagStream
    (
    -- * Creating a Request
      untagStream
    , UntagStream
    -- * Request Lenses
    , usStreamARN
    , usStreamName
    , usTagKeyList

    -- * Destructuring the Response
    , untagStreamResponse
    , UntagStreamResponse
    -- * Response Lenses
    , ursResponseStatus
    ) where

import Network.AWS.KinesisVideo.Types
import Network.AWS.KinesisVideo.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'untagStream' smart constructor.
data UntagStream = UntagStream'
  { _usStreamARN  :: !(Maybe Text)
  , _usStreamName :: !(Maybe Text)
  , _usTagKeyList :: !(List1 Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UntagStream' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'usStreamARN' - The Amazon Resource Name (ARN) of the stream that you want to remove tags from.
--
-- * 'usStreamName' - The name of the stream that you want to remove tags from.
--
-- * 'usTagKeyList' - A list of the keys of the tags that you want to remove.
untagStream
    :: NonEmpty Text -- ^ 'usTagKeyList'
    -> UntagStream
untagStream pTagKeyList_ =
  UntagStream'
    { _usStreamARN = Nothing
    , _usStreamName = Nothing
    , _usTagKeyList = _List1 # pTagKeyList_
    }


-- | The Amazon Resource Name (ARN) of the stream that you want to remove tags from.
usStreamARN :: Lens' UntagStream (Maybe Text)
usStreamARN = lens _usStreamARN (\ s a -> s{_usStreamARN = a})

-- | The name of the stream that you want to remove tags from.
usStreamName :: Lens' UntagStream (Maybe Text)
usStreamName = lens _usStreamName (\ s a -> s{_usStreamName = a})

-- | A list of the keys of the tags that you want to remove.
usTagKeyList :: Lens' UntagStream (NonEmpty Text)
usTagKeyList = lens _usTagKeyList (\ s a -> s{_usTagKeyList = a}) . _List1

instance AWSRequest UntagStream where
        type Rs UntagStream = UntagStreamResponse
        request = postJSON kinesisVideo
        response
          = receiveEmpty
              (\ s h x ->
                 UntagStreamResponse' <$> (pure (fromEnum s)))

instance Hashable UntagStream where

instance NFData UntagStream where

instance ToHeaders UntagStream where
        toHeaders = const mempty

instance ToJSON UntagStream where
        toJSON UntagStream'{..}
          = object
              (catMaybes
                 [("StreamARN" .=) <$> _usStreamARN,
                  ("StreamName" .=) <$> _usStreamName,
                  Just ("TagKeyList" .= _usTagKeyList)])

instance ToPath UntagStream where
        toPath = const "/untagStream"

instance ToQuery UntagStream where
        toQuery = const mempty

-- | /See:/ 'untagStreamResponse' smart constructor.
newtype UntagStreamResponse = UntagStreamResponse'
  { _ursResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UntagStreamResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ursResponseStatus' - -- | The response status code.
untagStreamResponse
    :: Int -- ^ 'ursResponseStatus'
    -> UntagStreamResponse
untagStreamResponse pResponseStatus_ =
  UntagStreamResponse' {_ursResponseStatus = pResponseStatus_}


-- | -- | The response status code.
ursResponseStatus :: Lens' UntagStreamResponse Int
ursResponseStatus = lens _ursResponseStatus (\ s a -> s{_ursResponseStatus = a})

instance NFData UntagStreamResponse where
