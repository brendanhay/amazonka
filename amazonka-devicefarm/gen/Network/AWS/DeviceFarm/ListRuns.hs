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
    , lrNextToken
    , lrArn

    -- * Response
    , ListRunsResponse
    -- ** Response constructor
    , listRunsResponse
    -- ** Response lenses
    , lrrRuns
    , lrrNextToken
    , lrrStatus
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
-- * 'lrNextToken'
--
-- * 'lrArn'
data ListRuns = ListRuns'
    { _lrNextToken :: !(Maybe Text)
    , _lrArn       :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListRuns' smart constructor.
listRuns :: Text -> ListRuns
listRuns pArn =
    ListRuns'
    { _lrNextToken = Nothing
    , _lrArn = pArn
    }

-- | An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
lrNextToken :: Lens' ListRuns (Maybe Text)
lrNextToken = lens _lrNextToken (\ s a -> s{_lrNextToken = a});

-- | The runs\' ARNs.
lrArn :: Lens' ListRuns Text
lrArn = lens _lrArn (\ s a -> s{_lrArn = a});

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
              ["nextToken" .= _lrNextToken, "arn" .= _lrArn]

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
-- * 'lrrRuns'
--
-- * 'lrrNextToken'
--
-- * 'lrrStatus'
data ListRunsResponse = ListRunsResponse'
    { _lrrRuns      :: !(Maybe [Run])
    , _lrrNextToken :: !(Maybe Text)
    , _lrrStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListRunsResponse' smart constructor.
listRunsResponse :: Int -> ListRunsResponse
listRunsResponse pStatus =
    ListRunsResponse'
    { _lrrRuns = Nothing
    , _lrrNextToken = Nothing
    , _lrrStatus = pStatus
    }

-- | Information about the runs.
lrrRuns :: Lens' ListRunsResponse [Run]
lrrRuns = lens _lrrRuns (\ s a -> s{_lrrRuns = a}) . _Default;

-- | If the number of items that are returned is significantly large, this is
-- an identifier that is also returned, which can be used in a subsequent
-- call to this operation to return the next set of items in the list.
lrrNextToken :: Lens' ListRunsResponse (Maybe Text)
lrrNextToken = lens _lrrNextToken (\ s a -> s{_lrrNextToken = a});

-- | FIXME: Undocumented member.
lrrStatus :: Lens' ListRunsResponse Int
lrrStatus = lens _lrrStatus (\ s a -> s{_lrrStatus = a});
