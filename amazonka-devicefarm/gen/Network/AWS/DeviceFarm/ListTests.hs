{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.ListTests
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Gets information about tests.
--
-- <http://docs.aws.amazon.com/devicefarm/latest/APIReference/API_ListTests.html>
module Network.AWS.DeviceFarm.ListTests
    (
    -- * Request
      ListTests
    -- ** Request constructor
    , listTests
    -- ** Request lenses
    , ltNextToken
    , ltArn

    -- * Response
    , ListTestsResponse
    -- ** Response constructor
    , listTestsResponse
    -- ** Response lenses
    , ltrsTests
    , ltrsNextToken
    , ltrsStatus
    ) where

import           Network.AWS.DeviceFarm.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents a request to the list tests operation.
--
-- /See:/ 'listTests' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ltNextToken'
--
-- * 'ltArn'
data ListTests = ListTests'
    { _ltNextToken :: !(Maybe Text)
    , _ltArn       :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListTests' smart constructor.
listTests :: Text -> ListTests
listTests pArn_ =
    ListTests'
    { _ltNextToken = Nothing
    , _ltArn = pArn_
    }

-- | An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
ltNextToken :: Lens' ListTests (Maybe Text)
ltNextToken = lens _ltNextToken (\ s a -> s{_ltNextToken = a});

-- | The tests\' ARNs.
ltArn :: Lens' ListTests Text
ltArn = lens _ltArn (\ s a -> s{_ltArn = a});

instance AWSRequest ListTests where
        type Sv ListTests = DeviceFarm
        type Rs ListTests = ListTestsResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 ListTestsResponse' <$>
                   (x .?> "tests" .!@ mempty) <*> (x .?> "nextToken")
                     <*> (pure (fromEnum s)))

instance ToHeaders ListTests where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DeviceFarm_20150623.ListTests" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListTests where
        toJSON ListTests'{..}
          = object
              ["nextToken" .= _ltNextToken, "arn" .= _ltArn]

instance ToPath ListTests where
        toPath = const "/"

instance ToQuery ListTests where
        toQuery = const mempty

-- | Represents the result of a list tests request.
--
-- /See:/ 'listTestsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ltrsTests'
--
-- * 'ltrsNextToken'
--
-- * 'ltrsStatus'
data ListTestsResponse = ListTestsResponse'
    { _ltrsTests     :: !(Maybe [Test])
    , _ltrsNextToken :: !(Maybe Text)
    , _ltrsStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListTestsResponse' smart constructor.
listTestsResponse :: Int -> ListTestsResponse
listTestsResponse pStatus_ =
    ListTestsResponse'
    { _ltrsTests = Nothing
    , _ltrsNextToken = Nothing
    , _ltrsStatus = pStatus_
    }

-- | Information about the tests.
ltrsTests :: Lens' ListTestsResponse [Test]
ltrsTests = lens _ltrsTests (\ s a -> s{_ltrsTests = a}) . _Default;

-- | If the number of items that are returned is significantly large, this is
-- an identifier that is also returned, which can be used in a subsequent
-- call to this operation to return the next set of items in the list.
ltrsNextToken :: Lens' ListTestsResponse (Maybe Text)
ltrsNextToken = lens _ltrsNextToken (\ s a -> s{_ltrsNextToken = a});

-- | FIXME: Undocumented member.
ltrsStatus :: Lens' ListTestsResponse Int
ltrsStatus = lens _ltrsStatus (\ s a -> s{_ltrsStatus = a});
