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
-- Module      : Network.AWS.WAF.GetSampledRequests
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets detailed information about a specified number of requests--a sample--that AWS WAF randomly selects from among the first 5,000 requests that your AWS resource received during a time range that you choose. You can specify a sample size of up to 500 requests, and you can specify any time range in the previous three hours.
--
--
-- @GetSampledRequests@ returns a time range, which is usually the time range that you specified. However, if your resource (such as a CloudFront distribution) received 5,000 requests before the specified time range elapsed, @GetSampledRequests@ returns an updated time range. This new time range indicates the actual period during which AWS WAF selected the requests in the sample.
--
module Network.AWS.WAF.GetSampledRequests
    (
    -- * Creating a Request
      getSampledRequests
    , GetSampledRequests
    -- * Request Lenses
    , gsrWebACLId
    , gsrRuleId
    , gsrTimeWindow
    , gsrMaxItems

    -- * Destructuring the Response
    , getSampledRequestsResponse
    , GetSampledRequestsResponse
    -- * Response Lenses
    , gsrrsSampledRequests
    , gsrrsPopulationSize
    , gsrrsTimeWindow
    , gsrrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WAF.Types
import Network.AWS.WAF.Types.Product

-- | /See:/ 'getSampledRequests' smart constructor.
data GetSampledRequests = GetSampledRequests'
  { _gsrWebACLId   :: !Text
  , _gsrRuleId     :: !Text
  , _gsrTimeWindow :: !TimeWindow
  , _gsrMaxItems   :: !Nat
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetSampledRequests' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsrWebACLId' - The @WebACLId@ of the @WebACL@ for which you want @GetSampledRequests@ to return a sample of requests.
--
-- * 'gsrRuleId' - @RuleId@ is one of three values:     * The @RuleId@ of the @Rule@ or the @RuleGroupId@ of the @RuleGroup@ for which you want @GetSampledRequests@ to return a sample of requests.     * @Default_Action@ , which causes @GetSampledRequests@ to return a sample of the requests that didn't match any of the rules in the specified @WebACL@ .
--
-- * 'gsrTimeWindow' - The start date and time and the end date and time of the range for which you want @GetSampledRequests@ to return a sample of requests. Specify the date and time in the following format: @"2016-09-27T14:50Z"@ . You can specify any time range in the previous three hours.
--
-- * 'gsrMaxItems' - The number of requests that you want AWS WAF to return from among the first 5,000 requests that your AWS resource received during the time range. If your resource received fewer requests than the value of @MaxItems@ , @GetSampledRequests@ returns information about all of them.
getSampledRequests
    :: Text -- ^ 'gsrWebACLId'
    -> Text -- ^ 'gsrRuleId'
    -> TimeWindow -- ^ 'gsrTimeWindow'
    -> Natural -- ^ 'gsrMaxItems'
    -> GetSampledRequests
getSampledRequests pWebACLId_ pRuleId_ pTimeWindow_ pMaxItems_ =
  GetSampledRequests'
    { _gsrWebACLId = pWebACLId_
    , _gsrRuleId = pRuleId_
    , _gsrTimeWindow = pTimeWindow_
    , _gsrMaxItems = _Nat # pMaxItems_
    }


-- | The @WebACLId@ of the @WebACL@ for which you want @GetSampledRequests@ to return a sample of requests.
gsrWebACLId :: Lens' GetSampledRequests Text
gsrWebACLId = lens _gsrWebACLId (\ s a -> s{_gsrWebACLId = a})

-- | @RuleId@ is one of three values:     * The @RuleId@ of the @Rule@ or the @RuleGroupId@ of the @RuleGroup@ for which you want @GetSampledRequests@ to return a sample of requests.     * @Default_Action@ , which causes @GetSampledRequests@ to return a sample of the requests that didn't match any of the rules in the specified @WebACL@ .
gsrRuleId :: Lens' GetSampledRequests Text
gsrRuleId = lens _gsrRuleId (\ s a -> s{_gsrRuleId = a})

-- | The start date and time and the end date and time of the range for which you want @GetSampledRequests@ to return a sample of requests. Specify the date and time in the following format: @"2016-09-27T14:50Z"@ . You can specify any time range in the previous three hours.
gsrTimeWindow :: Lens' GetSampledRequests TimeWindow
gsrTimeWindow = lens _gsrTimeWindow (\ s a -> s{_gsrTimeWindow = a})

-- | The number of requests that you want AWS WAF to return from among the first 5,000 requests that your AWS resource received during the time range. If your resource received fewer requests than the value of @MaxItems@ , @GetSampledRequests@ returns information about all of them.
gsrMaxItems :: Lens' GetSampledRequests Natural
gsrMaxItems = lens _gsrMaxItems (\ s a -> s{_gsrMaxItems = a}) . _Nat

instance AWSRequest GetSampledRequests where
        type Rs GetSampledRequests =
             GetSampledRequestsResponse
        request = postJSON waf
        response
          = receiveJSON
              (\ s h x ->
                 GetSampledRequestsResponse' <$>
                   (x .?> "SampledRequests" .!@ mempty) <*>
                     (x .?> "PopulationSize")
                     <*> (x .?> "TimeWindow")
                     <*> (pure (fromEnum s)))

instance Hashable GetSampledRequests where

instance NFData GetSampledRequests where

instance ToHeaders GetSampledRequests where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSWAF_20150824.GetSampledRequests" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetSampledRequests where
        toJSON GetSampledRequests'{..}
          = object
              (catMaybes
                 [Just ("WebAclId" .= _gsrWebACLId),
                  Just ("RuleId" .= _gsrRuleId),
                  Just ("TimeWindow" .= _gsrTimeWindow),
                  Just ("MaxItems" .= _gsrMaxItems)])

instance ToPath GetSampledRequests where
        toPath = const "/"

instance ToQuery GetSampledRequests where
        toQuery = const mempty

-- | /See:/ 'getSampledRequestsResponse' smart constructor.
data GetSampledRequestsResponse = GetSampledRequestsResponse'
  { _gsrrsSampledRequests :: !(Maybe [SampledHTTPRequest])
  , _gsrrsPopulationSize  :: !(Maybe Integer)
  , _gsrrsTimeWindow      :: !(Maybe TimeWindow)
  , _gsrrsResponseStatus  :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetSampledRequestsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsrrsSampledRequests' - A complex type that contains detailed information about each of the requests in the sample.
--
-- * 'gsrrsPopulationSize' - The total number of requests from which @GetSampledRequests@ got a sample of @MaxItems@ requests. If @PopulationSize@ is less than @MaxItems@ , the sample includes every request that your AWS resource received during the specified time range.
--
-- * 'gsrrsTimeWindow' - Usually, @TimeWindow@ is the time range that you specified in the @GetSampledRequests@ request. However, if your AWS resource received more than 5,000 requests during the time range that you specified in the request, @GetSampledRequests@ returns the time range for the first 5,000 requests.
--
-- * 'gsrrsResponseStatus' - -- | The response status code.
getSampledRequestsResponse
    :: Int -- ^ 'gsrrsResponseStatus'
    -> GetSampledRequestsResponse
getSampledRequestsResponse pResponseStatus_ =
  GetSampledRequestsResponse'
    { _gsrrsSampledRequests = Nothing
    , _gsrrsPopulationSize = Nothing
    , _gsrrsTimeWindow = Nothing
    , _gsrrsResponseStatus = pResponseStatus_
    }


-- | A complex type that contains detailed information about each of the requests in the sample.
gsrrsSampledRequests :: Lens' GetSampledRequestsResponse [SampledHTTPRequest]
gsrrsSampledRequests = lens _gsrrsSampledRequests (\ s a -> s{_gsrrsSampledRequests = a}) . _Default . _Coerce

-- | The total number of requests from which @GetSampledRequests@ got a sample of @MaxItems@ requests. If @PopulationSize@ is less than @MaxItems@ , the sample includes every request that your AWS resource received during the specified time range.
gsrrsPopulationSize :: Lens' GetSampledRequestsResponse (Maybe Integer)
gsrrsPopulationSize = lens _gsrrsPopulationSize (\ s a -> s{_gsrrsPopulationSize = a})

-- | Usually, @TimeWindow@ is the time range that you specified in the @GetSampledRequests@ request. However, if your AWS resource received more than 5,000 requests during the time range that you specified in the request, @GetSampledRequests@ returns the time range for the first 5,000 requests.
gsrrsTimeWindow :: Lens' GetSampledRequestsResponse (Maybe TimeWindow)
gsrrsTimeWindow = lens _gsrrsTimeWindow (\ s a -> s{_gsrrsTimeWindow = a})

-- | -- | The response status code.
gsrrsResponseStatus :: Lens' GetSampledRequestsResponse Int
gsrrsResponseStatus = lens _gsrrsResponseStatus (\ s a -> s{_gsrrsResponseStatus = a})

instance NFData GetSampledRequestsResponse where
