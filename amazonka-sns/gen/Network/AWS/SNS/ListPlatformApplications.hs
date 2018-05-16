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
-- Module      : Network.AWS.SNS.ListPlatformApplications
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the platform application objects for the supported push notification services, such as APNS and GCM. The results for @ListPlatformApplications@ are paginated and return a limited list of applications, up to 100. If additional records are available after the first page results, then a NextToken string will be returned. To receive the next page, you call @ListPlatformApplications@ using the NextToken string received from the previous call. When there are no more records to return, NextToken will be null. For more information, see <http://docs.aws.amazon.com/sns/latest/dg/SNSMobilePush.html Using Amazon SNS Mobile Push Notifications> .
--
--
--
-- This operation returns paginated results.
module Network.AWS.SNS.ListPlatformApplications
    (
    -- * Creating a Request
      listPlatformApplications
    , ListPlatformApplications
    -- * Request Lenses
    , lpaNextToken

    -- * Destructuring the Response
    , listPlatformApplicationsResponse
    , ListPlatformApplicationsResponse
    -- * Response Lenses
    , lparsPlatformApplications
    , lparsNextToken
    , lparsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SNS.Types
import Network.AWS.SNS.Types.Product

-- | Input for ListPlatformApplications action.
--
--
--
-- /See:/ 'listPlatformApplications' smart constructor.
newtype ListPlatformApplications = ListPlatformApplications'
  { _lpaNextToken :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListPlatformApplications' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lpaNextToken' - NextToken string is used when calling ListPlatformApplications action to retrieve additional records that are available after the first page results.
listPlatformApplications
    :: ListPlatformApplications
listPlatformApplications = ListPlatformApplications' {_lpaNextToken = Nothing}


-- | NextToken string is used when calling ListPlatformApplications action to retrieve additional records that are available after the first page results.
lpaNextToken :: Lens' ListPlatformApplications (Maybe Text)
lpaNextToken = lens _lpaNextToken (\ s a -> s{_lpaNextToken = a})

instance AWSPager ListPlatformApplications where
        page rq rs
          | stop (rs ^. lparsNextToken) = Nothing
          | stop (rs ^. lparsPlatformApplications) = Nothing
          | otherwise =
            Just $ rq & lpaNextToken .~ rs ^. lparsNextToken

instance AWSRequest ListPlatformApplications where
        type Rs ListPlatformApplications =
             ListPlatformApplicationsResponse
        request = postQuery sns
        response
          = receiveXMLWrapper "ListPlatformApplicationsResult"
              (\ s h x ->
                 ListPlatformApplicationsResponse' <$>
                   (x .@? "PlatformApplications" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (x .@? "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListPlatformApplications where

instance NFData ListPlatformApplications where

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
--
--
-- /See:/ 'listPlatformApplicationsResponse' smart constructor.
data ListPlatformApplicationsResponse = ListPlatformApplicationsResponse'
  { _lparsPlatformApplications :: !(Maybe [PlatformApplication])
  , _lparsNextToken            :: !(Maybe Text)
  , _lparsResponseStatus       :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListPlatformApplicationsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lparsPlatformApplications' - Platform applications returned when calling ListPlatformApplications action.
--
-- * 'lparsNextToken' - NextToken string is returned when calling ListPlatformApplications action if additional records are available after the first page results.
--
-- * 'lparsResponseStatus' - -- | The response status code.
listPlatformApplicationsResponse
    :: Int -- ^ 'lparsResponseStatus'
    -> ListPlatformApplicationsResponse
listPlatformApplicationsResponse pResponseStatus_ =
  ListPlatformApplicationsResponse'
    { _lparsPlatformApplications = Nothing
    , _lparsNextToken = Nothing
    , _lparsResponseStatus = pResponseStatus_
    }


-- | Platform applications returned when calling ListPlatformApplications action.
lparsPlatformApplications :: Lens' ListPlatformApplicationsResponse [PlatformApplication]
lparsPlatformApplications = lens _lparsPlatformApplications (\ s a -> s{_lparsPlatformApplications = a}) . _Default . _Coerce

-- | NextToken string is returned when calling ListPlatformApplications action if additional records are available after the first page results.
lparsNextToken :: Lens' ListPlatformApplicationsResponse (Maybe Text)
lparsNextToken = lens _lparsNextToken (\ s a -> s{_lparsNextToken = a})

-- | -- | The response status code.
lparsResponseStatus :: Lens' ListPlatformApplicationsResponse Int
lparsResponseStatus = lens _lparsResponseStatus (\ s a -> s{_lparsResponseStatus = a})

instance NFData ListPlatformApplicationsResponse
         where
