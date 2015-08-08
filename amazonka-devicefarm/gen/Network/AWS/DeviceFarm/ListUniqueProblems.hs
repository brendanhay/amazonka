{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.ListUniqueProblems
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Gets information about unique problems.
--
-- /See:/ <http://docs.aws.amazon.com/devicefarm/latest/APIReference/API_ListUniqueProblems.html AWS API Reference> for ListUniqueProblems.
module Network.AWS.DeviceFarm.ListUniqueProblems
    (
    -- * Creating a Request
      ListUniqueProblems
    , listUniqueProblems
    -- * Request Lenses
    , lupNextToken
    , lupArn

    -- * Destructuring the Response
    , ListUniqueProblemsResponse
    , listUniqueProblemsResponse
    -- * Response Lenses
    , luprsNextToken
    , luprsUniqueProblems
    , luprsStatus
    ) where

import           Network.AWS.DeviceFarm.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents a request to the list unique problems operation.
--
-- /See:/ 'listUniqueProblems' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lupNextToken'
--
-- * 'lupArn'
data ListUniqueProblems = ListUniqueProblems'
    { _lupNextToken :: !(Maybe Text)
    , _lupArn       :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListUniqueProblems' smart constructor.
listUniqueProblems :: Text -> ListUniqueProblems
listUniqueProblems pArn_ =
    ListUniqueProblems'
    { _lupNextToken = Nothing
    , _lupArn = pArn_
    }

-- | An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
lupNextToken :: Lens' ListUniqueProblems (Maybe Text)
lupNextToken = lens _lupNextToken (\ s a -> s{_lupNextToken = a});

-- | The unique problems\' ARNs.
lupArn :: Lens' ListUniqueProblems Text
lupArn = lens _lupArn (\ s a -> s{_lupArn = a});

instance AWSRequest ListUniqueProblems where
        type Sv ListUniqueProblems = DeviceFarm
        type Rs ListUniqueProblems =
             ListUniqueProblemsResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 ListUniqueProblemsResponse' <$>
                   (x .?> "nextToken") <*>
                     (x .?> "uniqueProblems" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance ToHeaders ListUniqueProblems where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DeviceFarm_20150623.ListUniqueProblems" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListUniqueProblems where
        toJSON ListUniqueProblems'{..}
          = object
              ["nextToken" .= _lupNextToken, "arn" .= _lupArn]

instance ToPath ListUniqueProblems where
        toPath = const "/"

instance ToQuery ListUniqueProblems where
        toQuery = const mempty

-- | Represents the result of a list unique problems request.
--
-- /See:/ 'listUniqueProblemsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'luprsNextToken'
--
-- * 'luprsUniqueProblems'
--
-- * 'luprsStatus'
data ListUniqueProblemsResponse = ListUniqueProblemsResponse'
    { _luprsNextToken      :: !(Maybe Text)
    , _luprsUniqueProblems :: !(Maybe (Map ExecutionResult [UniqueProblem]))
    , _luprsStatus         :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListUniqueProblemsResponse' smart constructor.
listUniqueProblemsResponse :: Int -> ListUniqueProblemsResponse
listUniqueProblemsResponse pStatus_ =
    ListUniqueProblemsResponse'
    { _luprsNextToken = Nothing
    , _luprsUniqueProblems = Nothing
    , _luprsStatus = pStatus_
    }

-- | If the number of items that are returned is significantly large, this is
-- an identifier that is also returned, which can be used in a subsequent
-- call to this operation to return the next set of items in the list.
luprsNextToken :: Lens' ListUniqueProblemsResponse (Maybe Text)
luprsNextToken = lens _luprsNextToken (\ s a -> s{_luprsNextToken = a});

-- | Information about the unique problems.
--
-- Allowed values include:
--
-- -   ERRORED: An error condition.
--
-- -   FAILED: A failed condition.
--
-- -   SKIPPED: A skipped condition.
--
-- -   STOPPED: A stopped condition.
--
-- -   PASSED: A passing condition.
--
-- -   PENDING: A pending condition.
--
-- -   WARNED: A warning condition.
--
luprsUniqueProblems :: Lens' ListUniqueProblemsResponse (HashMap ExecutionResult [UniqueProblem])
luprsUniqueProblems = lens _luprsUniqueProblems (\ s a -> s{_luprsUniqueProblems = a}) . _Default . _Map;

-- | Undocumented member.
luprsStatus :: Lens' ListUniqueProblemsResponse Int
luprsStatus = lens _luprsStatus (\ s a -> s{_luprsStatus = a});
