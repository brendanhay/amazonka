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
-- Module      : Network.AWS.Inspector.ListApplications
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the ARNs of the applications within this AWS account. For more
-- information about applications, see
-- <https://docs.aws.amazon.com/inspector/latest/userguide//inspector_applications.html Inspector Applications>.
module Network.AWS.Inspector.ListApplications
    (
    -- * Creating a Request
      listApplications
    , ListApplications
    -- * Request Lenses
    , lNextToken
    , lFilter
    , lMaxResults

    -- * Destructuring the Response
    , listApplicationsResponse
    , ListApplicationsResponse
    -- * Response Lenses
    , lrsApplicationARNList
    , lrsNextToken
    , lrsResponseStatus
    ) where

import           Network.AWS.Inspector.Types
import           Network.AWS.Inspector.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'listApplications' smart constructor.
data ListApplications = ListApplications'
    { _lNextToken  :: !(Maybe Text)
    , _lFilter     :: !(Maybe ApplicationsFilter)
    , _lMaxResults :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListApplications' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lNextToken'
--
-- * 'lFilter'
--
-- * 'lMaxResults'
listApplications
    :: ListApplications
listApplications =
    ListApplications'
    { _lNextToken = Nothing
    , _lFilter = Nothing
    , _lMaxResults = Nothing
    }

-- | You can use this parameter when paginating results. Set the value of
-- this parameter to \'null\' on your first call to the
-- __ListApplications__ action. Subsequent calls to the action fill
-- __nextToken__ in the request with the value of __NextToken__ from
-- previous response to continue listing data.
lNextToken :: Lens' ListApplications (Maybe Text)
lNextToken = lens _lNextToken (\ s a -> s{_lNextToken = a});

-- | You can use this parameter to specify a subset of data to be included in
-- the action\'s response.
--
-- For a record to match a filter, all specified filter attributes must
-- match. When multiple values are specified for a filter attribute, any of
-- the values can match.
lFilter :: Lens' ListApplications (Maybe ApplicationsFilter)
lFilter = lens _lFilter (\ s a -> s{_lFilter = a});

-- | You can use this parameter to indicate the maximum number of items you
-- want in the response. The default value is 10. The maximum value is 500.
lMaxResults :: Lens' ListApplications (Maybe Int)
lMaxResults = lens _lMaxResults (\ s a -> s{_lMaxResults = a});

instance AWSRequest ListApplications where
        type Rs ListApplications = ListApplicationsResponse
        request = postJSON inspector
        response
          = receiveJSON
              (\ s h x ->
                 ListApplicationsResponse' <$>
                   (x .?> "applicationArnList" .!@ mempty) <*>
                     (x .?> "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListApplications

instance NFData ListApplications

instance ToHeaders ListApplications where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("InspectorService.ListApplications" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListApplications where
        toJSON ListApplications'{..}
          = object
              (catMaybes
                 [("nextToken" .=) <$> _lNextToken,
                  ("filter" .=) <$> _lFilter,
                  ("maxResults" .=) <$> _lMaxResults])

instance ToPath ListApplications where
        toPath = const "/"

instance ToQuery ListApplications where
        toQuery = const mempty

-- | /See:/ 'listApplicationsResponse' smart constructor.
data ListApplicationsResponse = ListApplicationsResponse'
    { _lrsApplicationARNList :: !(Maybe [Text])
    , _lrsNextToken          :: !(Maybe Text)
    , _lrsResponseStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListApplicationsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrsApplicationARNList'
--
-- * 'lrsNextToken'
--
-- * 'lrsResponseStatus'
listApplicationsResponse
    :: Int -- ^ 'lrsResponseStatus'
    -> ListApplicationsResponse
listApplicationsResponse pResponseStatus_ =
    ListApplicationsResponse'
    { _lrsApplicationARNList = Nothing
    , _lrsNextToken = Nothing
    , _lrsResponseStatus = pResponseStatus_
    }

-- | A list of ARNs specifying the applications returned by the action.
lrsApplicationARNList :: Lens' ListApplicationsResponse [Text]
lrsApplicationARNList = lens _lrsApplicationARNList (\ s a -> s{_lrsApplicationARNList = a}) . _Default . _Coerce;

-- | When a response is generated, if there is more data to be listed, this
-- parameter is present in the response and contains the value to use for
-- the __nextToken__ parameter in a subsequent pagination request. If there
-- is no more data to be listed, this parameter is set to \'null\'.
lrsNextToken :: Lens' ListApplicationsResponse (Maybe Text)
lrsNextToken = lens _lrsNextToken (\ s a -> s{_lrsNextToken = a});

-- | The response status code.
lrsResponseStatus :: Lens' ListApplicationsResponse Int
lrsResponseStatus = lens _lrsResponseStatus (\ s a -> s{_lrsResponseStatus = a});

instance NFData ListApplicationsResponse
