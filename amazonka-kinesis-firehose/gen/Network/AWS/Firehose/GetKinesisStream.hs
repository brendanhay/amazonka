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
-- Module      : Network.AWS.Firehose.GetKinesisStream
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Undocumented operation.
module Network.AWS.Firehose.GetKinesisStream
    (
    -- * Creating a Request
      getKinesisStream
    , GetKinesisStream
    -- * Request Lenses
    , gksDeliveryStreamARN

    -- * Destructuring the Response
    , getKinesisStreamResponse
    , GetKinesisStreamResponse
    -- * Response Lenses
    , gksrsCredentialsForReadingKinesisStream
    , gksrsKinesisStreamARN
    , gksrsResponseStatus
    ) where

import Network.AWS.Firehose.Types
import Network.AWS.Firehose.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getKinesisStream' smart constructor.
newtype GetKinesisStream = GetKinesisStream'
  { _gksDeliveryStreamARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetKinesisStream' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gksDeliveryStreamARN' - Undocumented member.
getKinesisStream
    :: Text -- ^ 'gksDeliveryStreamARN'
    -> GetKinesisStream
getKinesisStream pDeliveryStreamARN_ =
  GetKinesisStream' {_gksDeliveryStreamARN = pDeliveryStreamARN_}


-- | Undocumented member.
gksDeliveryStreamARN :: Lens' GetKinesisStream Text
gksDeliveryStreamARN = lens _gksDeliveryStreamARN (\ s a -> s{_gksDeliveryStreamARN = a});

instance AWSRequest GetKinesisStream where
        type Rs GetKinesisStream = GetKinesisStreamResponse
        request = postJSON firehose
        response
          = receiveJSON
              (\ s h x ->
                 GetKinesisStreamResponse' <$>
                   (x .?> "CredentialsForReadingKinesisStream") <*>
                     (x .?> "KinesisStreamARN")
                     <*> (pure (fromEnum s)))

instance Hashable GetKinesisStream where

instance NFData GetKinesisStream where

instance ToHeaders GetKinesisStream where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Firehose_20150804.GetKinesisStream" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetKinesisStream where
        toJSON GetKinesisStream'{..}
          = object
              (catMaybes
                 [Just
                    ("DeliveryStreamARN" .= _gksDeliveryStreamARN)])

instance ToPath GetKinesisStream where
        toPath = const "/"

instance ToQuery GetKinesisStream where
        toQuery = const mempty

-- | /See:/ 'getKinesisStreamResponse' smart constructor.
data GetKinesisStreamResponse = GetKinesisStreamResponse'
  { _gksrsCredentialsForReadingKinesisStream :: !(Maybe SessionCredentials)
  , _gksrsKinesisStreamARN                   :: !(Maybe Text)
  , _gksrsResponseStatus                     :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetKinesisStreamResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gksrsCredentialsForReadingKinesisStream' - Undocumented member.
--
-- * 'gksrsKinesisStreamARN' - Undocumented member.
--
-- * 'gksrsResponseStatus' - -- | The response status code.
getKinesisStreamResponse
    :: Int -- ^ 'gksrsResponseStatus'
    -> GetKinesisStreamResponse
getKinesisStreamResponse pResponseStatus_ =
  GetKinesisStreamResponse'
  { _gksrsCredentialsForReadingKinesisStream = Nothing
  , _gksrsKinesisStreamARN = Nothing
  , _gksrsResponseStatus = pResponseStatus_
  }


-- | Undocumented member.
gksrsCredentialsForReadingKinesisStream :: Lens' GetKinesisStreamResponse (Maybe SessionCredentials)
gksrsCredentialsForReadingKinesisStream = lens _gksrsCredentialsForReadingKinesisStream (\ s a -> s{_gksrsCredentialsForReadingKinesisStream = a});

-- | Undocumented member.
gksrsKinesisStreamARN :: Lens' GetKinesisStreamResponse (Maybe Text)
gksrsKinesisStreamARN = lens _gksrsKinesisStreamARN (\ s a -> s{_gksrsKinesisStreamARN = a});

-- | -- | The response status code.
gksrsResponseStatus :: Lens' GetKinesisStreamResponse Int
gksrsResponseStatus = lens _gksrsResponseStatus (\ s a -> s{_gksrsResponseStatus = a});

instance NFData GetKinesisStreamResponse where
