{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.ListSamples
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about samples.
--
-- /See:/ <http://docs.aws.amazon.com/devicefarm/latest/APIReference/API_ListSamples.html AWS API Reference> for ListSamples.
module Network.AWS.DeviceFarm.ListSamples
    (
    -- * Creating a Request
      ListSamples
    , listSamples
    -- * Request Lenses
    , lsNextToken
    , lsArn

    -- * Destructuring the Response
    , ListSamplesResponse
    , listSamplesResponse
    -- * Response Lenses
    , lsrsNextToken
    , lsrsSamples
    , lsrsStatus
    ) where

import           Network.AWS.DeviceFarm.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents a request to the list samples operation.
--
-- /See:/ 'listSamples' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lsNextToken'
--
-- * 'lsArn'
data ListSamples = ListSamples'
    { _lsNextToken :: !(Maybe Text)
    , _lsArn       :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListSamples' smart constructor.
listSamples :: Text -> ListSamples
listSamples pArn_ =
    ListSamples'
    { _lsNextToken = Nothing
    , _lsArn = pArn_
    }

-- | An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
lsNextToken :: Lens' ListSamples (Maybe Text)
lsNextToken = lens _lsNextToken (\ s a -> s{_lsNextToken = a});

-- | The samples\' ARNs.
lsArn :: Lens' ListSamples Text
lsArn = lens _lsArn (\ s a -> s{_lsArn = a});

instance AWSRequest ListSamples where
        type Sv ListSamples = DeviceFarm
        type Rs ListSamples = ListSamplesResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 ListSamplesResponse' <$>
                   (x .?> "nextToken") <*> (x .?> "samples" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance ToHeaders ListSamples where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DeviceFarm_20150623.ListSamples" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListSamples where
        toJSON ListSamples'{..}
          = object
              ["nextToken" .= _lsNextToken, "arn" .= _lsArn]

instance ToPath ListSamples where
        toPath = const "/"

instance ToQuery ListSamples where
        toQuery = const mempty

-- | Represents the result of a list samples request.
--
-- /See:/ 'listSamplesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lsrsNextToken'
--
-- * 'lsrsSamples'
--
-- * 'lsrsStatus'
data ListSamplesResponse = ListSamplesResponse'
    { _lsrsNextToken :: !(Maybe Text)
    , _lsrsSamples   :: !(Maybe [Sample])
    , _lsrsStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListSamplesResponse' smart constructor.
listSamplesResponse :: Int -> ListSamplesResponse
listSamplesResponse pStatus_ =
    ListSamplesResponse'
    { _lsrsNextToken = Nothing
    , _lsrsSamples = Nothing
    , _lsrsStatus = pStatus_
    }

-- | If the number of items that are returned is significantly large, this is
-- an identifier that is also returned, which can be used in a subsequent
-- call to this operation to return the next set of items in the list.
lsrsNextToken :: Lens' ListSamplesResponse (Maybe Text)
lsrsNextToken = lens _lsrsNextToken (\ s a -> s{_lsrsNextToken = a});

-- | Information about the samples.
lsrsSamples :: Lens' ListSamplesResponse [Sample]
lsrsSamples = lens _lsrsSamples (\ s a -> s{_lsrsSamples = a}) . _Default . _Coerce;

-- | Undocumented member.
lsrsStatus :: Lens' ListSamplesResponse Int
lsrsStatus = lens _lsrsStatus (\ s a -> s{_lsrsStatus = a});
