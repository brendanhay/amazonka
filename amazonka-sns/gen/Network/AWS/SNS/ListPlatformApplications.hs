{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.SNS.ListPlatformApplications
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Lists the platform application objects for the supported push
-- notification services, such as APNS and GCM. The results for
-- @ListPlatformApplications@ are paginated and return a limited list of
-- applications, up to 100. If additional records are available after the
-- first page results, then a NextToken string will be returned. To receive
-- the next page, you call @ListPlatformApplications@ using the NextToken
-- string received from the previous call. When there are no more records
-- to return, NextToken will be null. For more information, see
-- <http://docs.aws.amazon.com/sns/latest/dg/SNSMobilePush.html Using Amazon SNS Mobile Push Notifications>.
--
-- <http://docs.aws.amazon.com/sns/latest/api/API_ListPlatformApplications.html>
module Network.AWS.SNS.ListPlatformApplications
    (
    -- * Request
      ListPlatformApplications
    -- ** Request constructor
    , listPlatformApplications
    -- ** Request lenses
    , lpaNextToken

    -- * Response
    , ListPlatformApplicationsResponse
    -- ** Response constructor
    , listPlatformApplicationsResponse
    -- ** Response lenses
    , lparPlatformApplications
    , lparNextToken
    , lparStatus
    ) where

import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SNS.Types

-- | Input for ListPlatformApplications action.
--
-- /See:/ 'listPlatformApplications' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lpaNextToken'
newtype ListPlatformApplications = ListPlatformApplications'
    { _lpaNextToken :: Maybe Text
    } deriving (Eq,Read,Show)

-- | 'ListPlatformApplications' smart constructor.
listPlatformApplications :: ListPlatformApplications
listPlatformApplications =
    ListPlatformApplications'
    { _lpaNextToken = Nothing
    }

-- | NextToken string is used when calling ListPlatformApplications action to
-- retrieve additional records that are available after the first page
-- results.
lpaNextToken :: Lens' ListPlatformApplications (Maybe Text)
lpaNextToken = lens _lpaNextToken (\ s a -> s{_lpaNextToken = a});

instance AWSPager ListPlatformApplications where
        page rq rs
          | stop (rs ^. lparNextToken) = Nothing
          | stop (rs ^. lparPlatformApplications) = Nothing
          | otherwise =
            Just $ rq & lpaNextToken .~ rs ^. lparNextToken

instance AWSRequest ListPlatformApplications where
        type Sv ListPlatformApplications = SNS
        type Rs ListPlatformApplications =
             ListPlatformApplicationsResponse
        request = post
        response
          = receiveXMLWrapper "ListPlatformApplicationsResult"
              (\ s h x ->
                 ListPlatformApplicationsResponse' <$>
                   (x .@? "PlatformApplications" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (x .@? "NextToken")
                     <*> (pure (fromEnum s)))

instance ToHeaders ListPlatformApplications where
        toHeaders = const mempty

instance ToPath ListPlatformApplications where
        toPath = const "/"

instance ToQuery ListPlatformApplications where
        toQuery ListPlatformApplications'{..}
          = mconcat
              ["Action" =:
                 ("ListPlatformApplications" :: ByteString),
               "Version" =: ("2010-03-31" :: ByteString),
               "NextToken" =: _lpaNextToken]

-- | Response for ListPlatformApplications action.
--
-- /See:/ 'listPlatformApplicationsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lparPlatformApplications'
--
-- * 'lparNextToken'
--
-- * 'lparStatus'
data ListPlatformApplicationsResponse = ListPlatformApplicationsResponse'
    { _lparPlatformApplications :: !(Maybe [PlatformApplication])
    , _lparNextToken            :: !(Maybe Text)
    , _lparStatus               :: !Int
    } deriving (Eq,Read,Show)

-- | 'ListPlatformApplicationsResponse' smart constructor.
listPlatformApplicationsResponse :: Int -> ListPlatformApplicationsResponse
listPlatformApplicationsResponse pStatus =
    ListPlatformApplicationsResponse'
    { _lparPlatformApplications = Nothing
    , _lparNextToken = Nothing
    , _lparStatus = pStatus
    }

-- | Platform applications returned when calling ListPlatformApplications
-- action.
lparPlatformApplications :: Lens' ListPlatformApplicationsResponse [PlatformApplication]
lparPlatformApplications = lens _lparPlatformApplications (\ s a -> s{_lparPlatformApplications = a}) . _Default;

-- | NextToken string is returned when calling ListPlatformApplications
-- action if additional records are available after the first page results.
lparNextToken :: Lens' ListPlatformApplicationsResponse (Maybe Text)
lparNextToken = lens _lparNextToken (\ s a -> s{_lparNextToken = a});

-- | FIXME: Undocumented member.
lparStatus :: Lens' ListPlatformApplicationsResponse Int
lparStatus = lens _lparStatus (\ s a -> s{_lparStatus = a});
