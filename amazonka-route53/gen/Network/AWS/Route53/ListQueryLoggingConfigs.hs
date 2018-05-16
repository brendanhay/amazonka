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
-- Module      : Network.AWS.Route53.ListQueryLoggingConfigs
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the configurations for DNS query logging that are associated with the current AWS account or the configuration that is associated with a specified hosted zone.
--
--
-- For more information about DNS query logs, see 'CreateQueryLoggingConfig' . Additional information, including the format of DNS query logs, appears in <http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/query-logs.html Logging DNS Queries> in the /Amazon Route 53 Developer Guide/ .
--
module Network.AWS.Route53.ListQueryLoggingConfigs
    (
    -- * Creating a Request
      listQueryLoggingConfigs
    , ListQueryLoggingConfigs
    -- * Request Lenses
    , lqlcHostedZoneId
    , lqlcNextToken
    , lqlcMaxResults

    -- * Destructuring the Response
    , listQueryLoggingConfigsResponse
    , ListQueryLoggingConfigsResponse
    -- * Response Lenses
    , lqlcrsNextToken
    , lqlcrsResponseStatus
    , lqlcrsQueryLoggingConfigs
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Route53.Types
import Network.AWS.Route53.Types.Product

-- | /See:/ 'listQueryLoggingConfigs' smart constructor.
data ListQueryLoggingConfigs = ListQueryLoggingConfigs'
  { _lqlcHostedZoneId :: !(Maybe ResourceId)
  , _lqlcNextToken    :: !(Maybe Text)
  , _lqlcMaxResults   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListQueryLoggingConfigs' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lqlcHostedZoneId' - (Optional) If you want to list the query logging configuration that is associated with a hosted zone, specify the ID in @HostedZoneId@ .  If you don't specify a hosted zone ID, @ListQueryLoggingConfigs@ returns all of the configurations that are associated with the current AWS account.
--
-- * 'lqlcNextToken' - (Optional) If the current AWS account has more than @MaxResults@ query logging configurations, use @NextToken@ to get the second and subsequent pages of results. For the first @ListQueryLoggingConfigs@ request, omit this value. For the second and subsequent requests, get the value of @NextToken@ from the previous response and specify that value for @NextToken@ in the request.
--
-- * 'lqlcMaxResults' - (Optional) The maximum number of query logging configurations that you want Amazon Route 53 to return in response to the current request. If the current AWS account has more than @MaxResults@ configurations, use the value of 'ListQueryLoggingConfigsResponse$NextToken' in the response to get the next page of results. If you don't specify a value for @MaxResults@ , Amazon Route 53 returns up to 100 configurations.
listQueryLoggingConfigs
    :: ListQueryLoggingConfigs
listQueryLoggingConfigs =
  ListQueryLoggingConfigs'
    { _lqlcHostedZoneId = Nothing
    , _lqlcNextToken = Nothing
    , _lqlcMaxResults = Nothing
    }


-- | (Optional) If you want to list the query logging configuration that is associated with a hosted zone, specify the ID in @HostedZoneId@ .  If you don't specify a hosted zone ID, @ListQueryLoggingConfigs@ returns all of the configurations that are associated with the current AWS account.
lqlcHostedZoneId :: Lens' ListQueryLoggingConfigs (Maybe ResourceId)
lqlcHostedZoneId = lens _lqlcHostedZoneId (\ s a -> s{_lqlcHostedZoneId = a})

-- | (Optional) If the current AWS account has more than @MaxResults@ query logging configurations, use @NextToken@ to get the second and subsequent pages of results. For the first @ListQueryLoggingConfigs@ request, omit this value. For the second and subsequent requests, get the value of @NextToken@ from the previous response and specify that value for @NextToken@ in the request.
lqlcNextToken :: Lens' ListQueryLoggingConfigs (Maybe Text)
lqlcNextToken = lens _lqlcNextToken (\ s a -> s{_lqlcNextToken = a})

-- | (Optional) The maximum number of query logging configurations that you want Amazon Route 53 to return in response to the current request. If the current AWS account has more than @MaxResults@ configurations, use the value of 'ListQueryLoggingConfigsResponse$NextToken' in the response to get the next page of results. If you don't specify a value for @MaxResults@ , Amazon Route 53 returns up to 100 configurations.
lqlcMaxResults :: Lens' ListQueryLoggingConfigs (Maybe Text)
lqlcMaxResults = lens _lqlcMaxResults (\ s a -> s{_lqlcMaxResults = a})

instance AWSRequest ListQueryLoggingConfigs where
        type Rs ListQueryLoggingConfigs =
             ListQueryLoggingConfigsResponse
        request = get route53
        response
          = receiveXML
              (\ s h x ->
                 ListQueryLoggingConfigsResponse' <$>
                   (x .@? "NextToken") <*> (pure (fromEnum s)) <*>
                     (x .@? "QueryLoggingConfigs" .!@ mempty >>=
                        parseXMLList "QueryLoggingConfig"))

instance Hashable ListQueryLoggingConfigs where

instance NFData ListQueryLoggingConfigs where

instance ToHeaders ListQueryLoggingConfigs where
        toHeaders = const mempty

instance ToPath ListQueryLoggingConfigs where
        toPath = const "/2013-04-01/queryloggingconfig"

instance ToQuery ListQueryLoggingConfigs where
        toQuery ListQueryLoggingConfigs'{..}
          = mconcat
              ["hostedzoneid" =: _lqlcHostedZoneId,
               "nexttoken" =: _lqlcNextToken,
               "maxresults" =: _lqlcMaxResults]

-- | /See:/ 'listQueryLoggingConfigsResponse' smart constructor.
data ListQueryLoggingConfigsResponse = ListQueryLoggingConfigsResponse'
  { _lqlcrsNextToken           :: !(Maybe Text)
  , _lqlcrsResponseStatus      :: !Int
  , _lqlcrsQueryLoggingConfigs :: ![QueryLoggingConfig]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListQueryLoggingConfigsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lqlcrsNextToken' - If a response includes the last of the query logging configurations that are associated with the current AWS account, @NextToken@ doesn't appear in the response. If a response doesn't include the last of the configurations, you can get more configurations by submitting another 'ListQueryLoggingConfigs' request. Get the value of @NextToken@ that Amazon Route 53 returned in the previous response and include it in @NextToken@ in the next request.
--
-- * 'lqlcrsResponseStatus' - -- | The response status code.
--
-- * 'lqlcrsQueryLoggingConfigs' - An array that contains one 'QueryLoggingConfig' element for each configuration for DNS query logging that is associated with the current AWS account.
listQueryLoggingConfigsResponse
    :: Int -- ^ 'lqlcrsResponseStatus'
    -> ListQueryLoggingConfigsResponse
listQueryLoggingConfigsResponse pResponseStatus_ =
  ListQueryLoggingConfigsResponse'
    { _lqlcrsNextToken = Nothing
    , _lqlcrsResponseStatus = pResponseStatus_
    , _lqlcrsQueryLoggingConfigs = mempty
    }


-- | If a response includes the last of the query logging configurations that are associated with the current AWS account, @NextToken@ doesn't appear in the response. If a response doesn't include the last of the configurations, you can get more configurations by submitting another 'ListQueryLoggingConfigs' request. Get the value of @NextToken@ that Amazon Route 53 returned in the previous response and include it in @NextToken@ in the next request.
lqlcrsNextToken :: Lens' ListQueryLoggingConfigsResponse (Maybe Text)
lqlcrsNextToken = lens _lqlcrsNextToken (\ s a -> s{_lqlcrsNextToken = a})

-- | -- | The response status code.
lqlcrsResponseStatus :: Lens' ListQueryLoggingConfigsResponse Int
lqlcrsResponseStatus = lens _lqlcrsResponseStatus (\ s a -> s{_lqlcrsResponseStatus = a})

-- | An array that contains one 'QueryLoggingConfig' element for each configuration for DNS query logging that is associated with the current AWS account.
lqlcrsQueryLoggingConfigs :: Lens' ListQueryLoggingConfigsResponse [QueryLoggingConfig]
lqlcrsQueryLoggingConfigs = lens _lqlcrsQueryLoggingConfigs (\ s a -> s{_lqlcrsQueryLoggingConfigs = a}) . _Coerce

instance NFData ListQueryLoggingConfigsResponse where
