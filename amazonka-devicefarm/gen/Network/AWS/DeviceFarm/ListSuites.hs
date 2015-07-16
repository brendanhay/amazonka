{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.ListSuites
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Gets information about suites.
--
-- <http://docs.aws.amazon.com/devicefarm/latest/APIReference/API_ListSuites.html>
module Network.AWS.DeviceFarm.ListSuites
    (
    -- * Request
      ListSuites
    -- ** Request constructor
    , listSuites
    -- ** Request lenses
    , lNextToken
    , lArn

    -- * Response
    , ListSuitesResponse
    -- ** Response constructor
    , listSuitesResponse
    -- ** Response lenses
    , lisNextToken
    , lisSuites
    , lisStatus
    ) where

import           Network.AWS.DeviceFarm.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents a request to the list suites operation.
--
-- /See:/ 'listSuites' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lNextToken'
--
-- * 'lArn'
data ListSuites = ListSuites'
    { _lNextToken :: !(Maybe Text)
    , _lArn       :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListSuites' smart constructor.
listSuites :: Text -> ListSuites
listSuites pArn =
    ListSuites'
    { _lNextToken = Nothing
    , _lArn = pArn
    }

-- | An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
lNextToken :: Lens' ListSuites (Maybe Text)
lNextToken = lens _lNextToken (\ s a -> s{_lNextToken = a});

-- | The suites\' ARNs.
lArn :: Lens' ListSuites Text
lArn = lens _lArn (\ s a -> s{_lArn = a});

instance AWSRequest ListSuites where
        type Sv ListSuites = DeviceFarm
        type Rs ListSuites = ListSuitesResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 ListSuitesResponse' <$>
                   (x .?> "nextToken") <*> (x .?> "suites" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance ToHeaders ListSuites where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DeviceFarm_20150623.ListSuites" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListSuites where
        toJSON ListSuites'{..}
          = object ["nextToken" .= _lNextToken, "arn" .= _lArn]

instance ToPath ListSuites where
        toPath = const "/"

instance ToQuery ListSuites where
        toQuery = const mempty

-- | Represents the result of a list suites request.
--
-- /See:/ 'listSuitesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lisNextToken'
--
-- * 'lisSuites'
--
-- * 'lisStatus'
data ListSuitesResponse = ListSuitesResponse'
    { _lisNextToken :: !(Maybe Text)
    , _lisSuites    :: !(Maybe [Suite])
    , _lisStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListSuitesResponse' smart constructor.
listSuitesResponse :: Int -> ListSuitesResponse
listSuitesResponse pStatus =
    ListSuitesResponse'
    { _lisNextToken = Nothing
    , _lisSuites = Nothing
    , _lisStatus = pStatus
    }

-- | If the number of items that are returned is significantly large, this is
-- an identifier that is also returned, which can be used in a subsequent
-- call to this operation to return the next set of items in the list.
lisNextToken :: Lens' ListSuitesResponse (Maybe Text)
lisNextToken = lens _lisNextToken (\ s a -> s{_lisNextToken = a});

-- | Information about the suites.
lisSuites :: Lens' ListSuitesResponse [Suite]
lisSuites = lens _lisSuites (\ s a -> s{_lisSuites = a}) . _Default;

-- | FIXME: Undocumented member.
lisStatus :: Lens' ListSuitesResponse Int
lisStatus = lens _lisStatus (\ s a -> s{_lisStatus = a});
