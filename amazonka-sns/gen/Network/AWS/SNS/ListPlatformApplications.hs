{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS.ListPlatformApplications
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Lists the platform application objects for the supported push
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
    , lparqNextToken

    -- * Response
    , ListPlatformApplicationsResponse
    -- ** Response constructor
    , listPlatformApplicationsResponse
    -- ** Response lenses
    , lparsPlatformApplications
    , lparsNextToken
    , lparsStatus
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
-- * 'lparqNextToken'
newtype ListPlatformApplications = ListPlatformApplications'
    { _lparqNextToken :: Maybe Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListPlatformApplications' smart constructor.
listPlatformApplications :: ListPlatformApplications
listPlatformApplications =
    ListPlatformApplications'
    { _lparqNextToken = Nothing
    }

-- | NextToken string is used when calling ListPlatformApplications action to
-- retrieve additional records that are available after the first page
-- results.
lparqNextToken :: Lens' ListPlatformApplications (Maybe Text)
lparqNextToken = lens _lparqNextToken (\ s a -> s{_lparqNextToken = a});

instance AWSPager ListPlatformApplications where
        page rq rs
          | stop (rs ^. lparsNextToken) = Nothing
          | stop (rs ^. lparsPlatformApplications) = Nothing
          | otherwise =
            Just $ rq & lparqNextToken .~ rs ^. lparsNextToken

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
               "NextToken" =: _lparqNextToken]

-- | Response for ListPlatformApplications action.
--
-- /See:/ 'listPlatformApplicationsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lparsPlatformApplications'
--
-- * 'lparsNextToken'
--
-- * 'lparsStatus'
data ListPlatformApplicationsResponse = ListPlatformApplicationsResponse'
    { _lparsPlatformApplications :: !(Maybe [PlatformApplication])
    , _lparsNextToken            :: !(Maybe Text)
    , _lparsStatus               :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListPlatformApplicationsResponse' smart constructor.
listPlatformApplicationsResponse :: Int -> ListPlatformApplicationsResponse
listPlatformApplicationsResponse pStatus =
    ListPlatformApplicationsResponse'
    { _lparsPlatformApplications = Nothing
    , _lparsNextToken = Nothing
    , _lparsStatus = pStatus
    }

-- | Platform applications returned when calling ListPlatformApplications
-- action.
lparsPlatformApplications :: Lens' ListPlatformApplicationsResponse [PlatformApplication]
lparsPlatformApplications = lens _lparsPlatformApplications (\ s a -> s{_lparsPlatformApplications = a}) . _Default;

-- | NextToken string is returned when calling ListPlatformApplications
-- action if additional records are available after the first page results.
lparsNextToken :: Lens' ListPlatformApplicationsResponse (Maybe Text)
lparsNextToken = lens _lparsNextToken (\ s a -> s{_lparsNextToken = a});

-- | FIXME: Undocumented member.
lparsStatus :: Lens' ListPlatformApplicationsResponse Int
lparsStatus = lens _lparsStatus (\ s a -> s{_lparsStatus = a});
