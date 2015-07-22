{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.ListRuns
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Gets information about runs.
--
-- <http://docs.aws.amazon.com/devicefarm/latest/APIReference/API_ListRuns.html>
module Network.AWS.DeviceFarm.ListRuns
    (
    -- * Request
      ListRuns
    -- ** Request constructor
    , listRuns
    -- ** Request lenses
    , lrrqNextToken
    , lrrqArn

    -- * Response
    , ListRunsResponse
    -- ** Response constructor
    , listRunsResponse
    -- ** Response lenses
    , lrrsRuns
    , lrrsNextToken
    , lrrsStatus
    ) where

import           Network.AWS.DeviceFarm.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents a request to the list runs operation.
--
-- /See:/ 'listRuns' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lrrqNextToken'
--
-- * 'lrrqArn'
data ListRuns = ListRuns'
    { _lrrqNextToken :: !(Maybe Text)
    , _lrrqArn       :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListRuns' smart constructor.
listRuns :: Text -> ListRuns
listRuns pArn =
    ListRuns'
    { _lrrqNextToken = Nothing
    , _lrrqArn = pArn
    }

-- | An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
lrrqNextToken :: Lens' ListRuns (Maybe Text)
lrrqNextToken = lens _lrrqNextToken (\ s a -> s{_lrrqNextToken = a});

-- | The runs\' ARNs.
lrrqArn :: Lens' ListRuns Text
lrrqArn = lens _lrrqArn (\ s a -> s{_lrrqArn = a});

instance AWSRequest ListRuns where
        type Sv ListRuns = DeviceFarm
        type Rs ListRuns = ListRunsResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 ListRunsResponse' <$>
                   (x .?> "runs" .!@ mempty) <*> (x .?> "nextToken") <*>
                     (pure (fromEnum s)))

instance ToHeaders ListRuns where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DeviceFarm_20150623.ListRuns" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListRuns where
        toJSON ListRuns'{..}
          = object
              ["nextToken" .= _lrrqNextToken, "arn" .= _lrrqArn]

instance ToPath ListRuns where
        toPath = const "/"

instance ToQuery ListRuns where
        toQuery = const mempty

-- | Represents the result of a list runs request.
--
-- /See:/ 'listRunsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lrrsRuns'
--
-- * 'lrrsNextToken'
--
-- * 'lrrsStatus'
data ListRunsResponse = ListRunsResponse'
    { _lrrsRuns      :: !(Maybe [Run])
    , _lrrsNextToken :: !(Maybe Text)
    , _lrrsStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListRunsResponse' smart constructor.
listRunsResponse :: Int -> ListRunsResponse
listRunsResponse pStatus =
    ListRunsResponse'
    { _lrrsRuns = Nothing
    , _lrrsNextToken = Nothing
    , _lrrsStatus = pStatus
    }

-- | Information about the runs.
lrrsRuns :: Lens' ListRunsResponse [Run]
lrrsRuns = lens _lrrsRuns (\ s a -> s{_lrrsRuns = a}) . _Default;

-- | If the number of items that are returned is significantly large, this is
-- an identifier that is also returned, which can be used in a subsequent
-- call to this operation to return the next set of items in the list.
lrrsNextToken :: Lens' ListRunsResponse (Maybe Text)
lrrsNextToken = lens _lrrsNextToken (\ s a -> s{_lrrsNextToken = a});

-- | FIXME: Undocumented member.
lrrsStatus :: Lens' ListRunsResponse Int
lrrsStatus = lens _lrrsStatus (\ s a -> s{_lrrsStatus = a});
