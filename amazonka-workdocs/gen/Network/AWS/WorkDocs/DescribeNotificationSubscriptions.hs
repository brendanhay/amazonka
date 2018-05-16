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
-- Module      : Network.AWS.WorkDocs.DescribeNotificationSubscriptions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the specified notification subscriptions.
--
--
module Network.AWS.WorkDocs.DescribeNotificationSubscriptions
    (
    -- * Creating a Request
      describeNotificationSubscriptions
    , DescribeNotificationSubscriptions
    -- * Request Lenses
    , dMarker
    , dLimit
    , dOrganizationId

    -- * Destructuring the Response
    , describeNotificationSubscriptionsResponse
    , DescribeNotificationSubscriptionsResponse
    -- * Response Lenses
    , dnsrsMarker
    , dnsrsSubscriptions
    , dnsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkDocs.Types
import Network.AWS.WorkDocs.Types.Product

-- | /See:/ 'describeNotificationSubscriptions' smart constructor.
data DescribeNotificationSubscriptions = DescribeNotificationSubscriptions'
  { _dMarker         :: !(Maybe Text)
  , _dLimit          :: !(Maybe Nat)
  , _dOrganizationId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeNotificationSubscriptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dMarker' - The marker for the next set of results. (You received this marker from a previous call.)
--
-- * 'dLimit' - The maximum number of items to return with this call.
--
-- * 'dOrganizationId' - The ID of the organization.
describeNotificationSubscriptions
    :: Text -- ^ 'dOrganizationId'
    -> DescribeNotificationSubscriptions
describeNotificationSubscriptions pOrganizationId_ =
  DescribeNotificationSubscriptions'
    {_dMarker = Nothing, _dLimit = Nothing, _dOrganizationId = pOrganizationId_}


-- | The marker for the next set of results. (You received this marker from a previous call.)
dMarker :: Lens' DescribeNotificationSubscriptions (Maybe Text)
dMarker = lens _dMarker (\ s a -> s{_dMarker = a})

-- | The maximum number of items to return with this call.
dLimit :: Lens' DescribeNotificationSubscriptions (Maybe Natural)
dLimit = lens _dLimit (\ s a -> s{_dLimit = a}) . mapping _Nat

-- | The ID of the organization.
dOrganizationId :: Lens' DescribeNotificationSubscriptions Text
dOrganizationId = lens _dOrganizationId (\ s a -> s{_dOrganizationId = a})

instance AWSRequest DescribeNotificationSubscriptions
         where
        type Rs DescribeNotificationSubscriptions =
             DescribeNotificationSubscriptionsResponse
        request = get workDocs
        response
          = receiveJSON
              (\ s h x ->
                 DescribeNotificationSubscriptionsResponse' <$>
                   (x .?> "Marker") <*>
                     (x .?> "Subscriptions" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable DescribeNotificationSubscriptions
         where

instance NFData DescribeNotificationSubscriptions
         where

instance ToHeaders DescribeNotificationSubscriptions
         where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DescribeNotificationSubscriptions
         where
        toPath DescribeNotificationSubscriptions'{..}
          = mconcat
              ["/api/v1/organizations/", toBS _dOrganizationId,
               "/subscriptions"]

instance ToQuery DescribeNotificationSubscriptions
         where
        toQuery DescribeNotificationSubscriptions'{..}
          = mconcat ["marker" =: _dMarker, "limit" =: _dLimit]

-- | /See:/ 'describeNotificationSubscriptionsResponse' smart constructor.
data DescribeNotificationSubscriptionsResponse = DescribeNotificationSubscriptionsResponse'
  { _dnsrsMarker         :: !(Maybe Text)
  , _dnsrsSubscriptions  :: !(Maybe [Subscription])
  , _dnsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeNotificationSubscriptionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dnsrsMarker' - The marker to use when requesting the next set of results. If there are no additional results, the string is empty.
--
-- * 'dnsrsSubscriptions' - The subscriptions.
--
-- * 'dnsrsResponseStatus' - -- | The response status code.
describeNotificationSubscriptionsResponse
    :: Int -- ^ 'dnsrsResponseStatus'
    -> DescribeNotificationSubscriptionsResponse
describeNotificationSubscriptionsResponse pResponseStatus_ =
  DescribeNotificationSubscriptionsResponse'
    { _dnsrsMarker = Nothing
    , _dnsrsSubscriptions = Nothing
    , _dnsrsResponseStatus = pResponseStatus_
    }


-- | The marker to use when requesting the next set of results. If there are no additional results, the string is empty.
dnsrsMarker :: Lens' DescribeNotificationSubscriptionsResponse (Maybe Text)
dnsrsMarker = lens _dnsrsMarker (\ s a -> s{_dnsrsMarker = a})

-- | The subscriptions.
dnsrsSubscriptions :: Lens' DescribeNotificationSubscriptionsResponse [Subscription]
dnsrsSubscriptions = lens _dnsrsSubscriptions (\ s a -> s{_dnsrsSubscriptions = a}) . _Default . _Coerce

-- | -- | The response status code.
dnsrsResponseStatus :: Lens' DescribeNotificationSubscriptionsResponse Int
dnsrsResponseStatus = lens _dnsrsResponseStatus (\ s a -> s{_dnsrsResponseStatus = a})

instance NFData
           DescribeNotificationSubscriptionsResponse
         where
