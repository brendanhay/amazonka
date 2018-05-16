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
-- Module      : Network.AWS.CloudWatch.ListDashboards
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of the dashboards for your account. If you include @DashboardNamePrefix@ , only those dashboards with names starting with the prefix are listed. Otherwise, all dashboards in your account are listed.
--
--
--
-- This operation returns paginated results.
module Network.AWS.CloudWatch.ListDashboards
    (
    -- * Creating a Request
      listDashboards
    , ListDashboards
    -- * Request Lenses
    , ldDashboardNamePrefix
    , ldNextToken

    -- * Destructuring the Response
    , listDashboardsResponse
    , ListDashboardsResponse
    -- * Response Lenses
    , ldrsDashboardEntries
    , ldrsNextToken
    , ldrsResponseStatus
    ) where

import Network.AWS.CloudWatch.Types
import Network.AWS.CloudWatch.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listDashboards' smart constructor.
data ListDashboards = ListDashboards'
  { _ldDashboardNamePrefix :: !(Maybe Text)
  , _ldNextToken           :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListDashboards' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldDashboardNamePrefix' - If you specify this parameter, only the dashboards with names starting with the specified string are listed. The maximum length is 255, and valid characters are A-Z, a-z, 0-9, ".", "-", and "_".
--
-- * 'ldNextToken' - The token returned by a previous call to indicate that there is more data available.
listDashboards
    :: ListDashboards
listDashboards =
  ListDashboards' {_ldDashboardNamePrefix = Nothing, _ldNextToken = Nothing}


-- | If you specify this parameter, only the dashboards with names starting with the specified string are listed. The maximum length is 255, and valid characters are A-Z, a-z, 0-9, ".", "-", and "_".
ldDashboardNamePrefix :: Lens' ListDashboards (Maybe Text)
ldDashboardNamePrefix = lens _ldDashboardNamePrefix (\ s a -> s{_ldDashboardNamePrefix = a})

-- | The token returned by a previous call to indicate that there is more data available.
ldNextToken :: Lens' ListDashboards (Maybe Text)
ldNextToken = lens _ldNextToken (\ s a -> s{_ldNextToken = a})

instance AWSPager ListDashboards where
        page rq rs
          | stop (rs ^. ldrsNextToken) = Nothing
          | stop (rs ^. ldrsDashboardEntries) = Nothing
          | otherwise =
            Just $ rq & ldNextToken .~ rs ^. ldrsNextToken

instance AWSRequest ListDashboards where
        type Rs ListDashboards = ListDashboardsResponse
        request = postQuery cloudWatch
        response
          = receiveXMLWrapper "ListDashboardsResult"
              (\ s h x ->
                 ListDashboardsResponse' <$>
                   (x .@? "DashboardEntries" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (x .@? "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListDashboards where

instance NFData ListDashboards where

instance ToHeaders ListDashboards where
        toHeaders = const mempty

instance ToPath ListDashboards where
        toPath = const "/"

instance ToQuery ListDashboards where
        toQuery ListDashboards'{..}
          = mconcat
              ["Action" =: ("ListDashboards" :: ByteString),
               "Version" =: ("2010-08-01" :: ByteString),
               "DashboardNamePrefix" =: _ldDashboardNamePrefix,
               "NextToken" =: _ldNextToken]

-- | /See:/ 'listDashboardsResponse' smart constructor.
data ListDashboardsResponse = ListDashboardsResponse'
  { _ldrsDashboardEntries :: !(Maybe [DashboardEntry])
  , _ldrsNextToken        :: !(Maybe Text)
  , _ldrsResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListDashboardsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldrsDashboardEntries' - The list of matching dashboards.
--
-- * 'ldrsNextToken' - The token that marks the start of the next batch of returned results.
--
-- * 'ldrsResponseStatus' - -- | The response status code.
listDashboardsResponse
    :: Int -- ^ 'ldrsResponseStatus'
    -> ListDashboardsResponse
listDashboardsResponse pResponseStatus_ =
  ListDashboardsResponse'
    { _ldrsDashboardEntries = Nothing
    , _ldrsNextToken = Nothing
    , _ldrsResponseStatus = pResponseStatus_
    }


-- | The list of matching dashboards.
ldrsDashboardEntries :: Lens' ListDashboardsResponse [DashboardEntry]
ldrsDashboardEntries = lens _ldrsDashboardEntries (\ s a -> s{_ldrsDashboardEntries = a}) . _Default . _Coerce

-- | The token that marks the start of the next batch of returned results.
ldrsNextToken :: Lens' ListDashboardsResponse (Maybe Text)
ldrsNextToken = lens _ldrsNextToken (\ s a -> s{_ldrsNextToken = a})

-- | -- | The response status code.
ldrsResponseStatus :: Lens' ListDashboardsResponse Int
ldrsResponseStatus = lens _ldrsResponseStatus (\ s a -> s{_ldrsResponseStatus = a})

instance NFData ListDashboardsResponse where
