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
-- Module      : Network.AWS.Route53AutoNaming.ListInstances
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists summary information about the instances that you registered by using a specified service.
--
--
--
-- This operation returns paginated results.
module Network.AWS.Route53AutoNaming.ListInstances
    (
    -- * Creating a Request
      listInstances
    , ListInstances
    -- * Request Lenses
    , liNextToken
    , liMaxResults
    , liServiceId

    -- * Destructuring the Response
    , listInstancesResponse
    , ListInstancesResponse
    -- * Response Lenses
    , lirsNextToken
    , lirsInstances
    , lirsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Route53AutoNaming.Types
import Network.AWS.Route53AutoNaming.Types.Product

-- | /See:/ 'listInstances' smart constructor.
data ListInstances = ListInstances'
  { _liNextToken  :: !(Maybe Text)
  , _liMaxResults :: !(Maybe Nat)
  , _liServiceId  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListInstances' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'liNextToken' - For the first @ListInstances@ request, omit this value. If more than @MaxResults@ instances match the specified criteria, you can submit another @ListInstances@ request to get the next group of results. Specify the value of @NextToken@ from the previous response in the next request.
--
-- * 'liMaxResults' - The maximum number of instances that you want Amazon Route 53 to return in the response to a @ListInstances@ request. If you don't specify a value for @MaxResults@ , Route 53 returns up to 100 instances.
--
-- * 'liServiceId' - The ID of the service that you want to list instances for.
listInstances
    :: Text -- ^ 'liServiceId'
    -> ListInstances
listInstances pServiceId_ =
  ListInstances'
    { _liNextToken = Nothing
    , _liMaxResults = Nothing
    , _liServiceId = pServiceId_
    }


-- | For the first @ListInstances@ request, omit this value. If more than @MaxResults@ instances match the specified criteria, you can submit another @ListInstances@ request to get the next group of results. Specify the value of @NextToken@ from the previous response in the next request.
liNextToken :: Lens' ListInstances (Maybe Text)
liNextToken = lens _liNextToken (\ s a -> s{_liNextToken = a})

-- | The maximum number of instances that you want Amazon Route 53 to return in the response to a @ListInstances@ request. If you don't specify a value for @MaxResults@ , Route 53 returns up to 100 instances.
liMaxResults :: Lens' ListInstances (Maybe Natural)
liMaxResults = lens _liMaxResults (\ s a -> s{_liMaxResults = a}) . mapping _Nat

-- | The ID of the service that you want to list instances for.
liServiceId :: Lens' ListInstances Text
liServiceId = lens _liServiceId (\ s a -> s{_liServiceId = a})

instance AWSPager ListInstances where
        page rq rs
          | stop (rs ^. lirsNextToken) = Nothing
          | stop (rs ^. lirsInstances) = Nothing
          | otherwise =
            Just $ rq & liNextToken .~ rs ^. lirsNextToken

instance AWSRequest ListInstances where
        type Rs ListInstances = ListInstancesResponse
        request = postJSON route53AutoNaming
        response
          = receiveJSON
              (\ s h x ->
                 ListInstancesResponse' <$>
                   (x .?> "NextToken") <*>
                     (x .?> "Instances" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListInstances where

instance NFData ListInstances where

instance ToHeaders ListInstances where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Route53AutoNaming_v20170314.ListInstances" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListInstances where
        toJSON ListInstances'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _liNextToken,
                  ("MaxResults" .=) <$> _liMaxResults,
                  Just ("ServiceId" .= _liServiceId)])

instance ToPath ListInstances where
        toPath = const "/"

instance ToQuery ListInstances where
        toQuery = const mempty

-- | /See:/ 'listInstancesResponse' smart constructor.
data ListInstancesResponse = ListInstancesResponse'
  { _lirsNextToken      :: !(Maybe Text)
  , _lirsInstances      :: !(Maybe [InstanceSummary])
  , _lirsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListInstancesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lirsNextToken' - If more than @MaxResults@ instances match the specified criteria, you can submit another @ListInstances@ request to get the next group of results. Specify the value of @NextToken@ from the previous response in the next request.
--
-- * 'lirsInstances' - Summary information about the instances that are associated with the specified service.
--
-- * 'lirsResponseStatus' - -- | The response status code.
listInstancesResponse
    :: Int -- ^ 'lirsResponseStatus'
    -> ListInstancesResponse
listInstancesResponse pResponseStatus_ =
  ListInstancesResponse'
    { _lirsNextToken = Nothing
    , _lirsInstances = Nothing
    , _lirsResponseStatus = pResponseStatus_
    }


-- | If more than @MaxResults@ instances match the specified criteria, you can submit another @ListInstances@ request to get the next group of results. Specify the value of @NextToken@ from the previous response in the next request.
lirsNextToken :: Lens' ListInstancesResponse (Maybe Text)
lirsNextToken = lens _lirsNextToken (\ s a -> s{_lirsNextToken = a})

-- | Summary information about the instances that are associated with the specified service.
lirsInstances :: Lens' ListInstancesResponse [InstanceSummary]
lirsInstances = lens _lirsInstances (\ s a -> s{_lirsInstances = a}) . _Default . _Coerce

-- | -- | The response status code.
lirsResponseStatus :: Lens' ListInstancesResponse Int
lirsResponseStatus = lens _lirsResponseStatus (\ s a -> s{_lirsResponseStatus = a})

instance NFData ListInstancesResponse where
