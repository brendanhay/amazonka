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
-- Module      : Network.AWS.WAF.CreateRateBasedRule
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a 'RateBasedRule' . The @RateBasedRule@ contains a @RateLimit@ , which specifies the maximum number of requests that AWS WAF allows from a specified IP address in a five-minute period. The @RateBasedRule@ also contains the @IPSet@ objects, @ByteMatchSet@ objects, and other predicates that identify the requests that you want to count or block if these requests exceed the @RateLimit@ .
--
--
-- If you add more than one predicate to a @RateBasedRule@ , a request not only must exceed the @RateLimit@ , but it also must match all the specifications to be counted or blocked. For example, suppose you add the following to a @RateBasedRule@ :
--
--     * An @IPSet@ that matches the IP address @192.0.2.44/32@
--
--     * A @ByteMatchSet@ that matches @BadBot@ in the @User-Agent@ header
--
--
--
-- Further, you specify a @RateLimit@ of 15,000.
--
-- You then add the @RateBasedRule@ to a @WebACL@ and specify that you want to block requests that meet the conditions in the rule. For a request to be blocked, it must come from the IP address 192.0.2.44 /and/ the @User-Agent@ header in the request must contain the value @BadBot@ . Further, requests that match these two conditions must be received at a rate of more than 15,000 requests every five minutes. If both conditions are met and the rate is exceeded, AWS WAF blocks the requests. If the rate drops below 15,000 for a five-minute period, AWS WAF no longer blocks the requests.
--
-- As a second example, suppose you want to limit requests to a particular page on your site. To do this, you could add the following to a @RateBasedRule@ :
--
--     * A @ByteMatchSet@ with @FieldToMatch@ of @URI@
--
--     * A @PositionalConstraint@ of @STARTS_WITH@
--
--     * A @TargetString@ of @login@
--
--
--
-- Further, you specify a @RateLimit@ of 15,000.
--
-- By adding this @RateBasedRule@ to a @WebACL@ , you could limit requests to your login page without affecting the rest of your site.
--
-- To create and configure a @RateBasedRule@ , perform the following steps:
--
--     * Create and update the predicates that you want to include in the rule. For more information, see 'CreateByteMatchSet' , 'CreateIPSet' , and 'CreateSqlInjectionMatchSet' .
--
--     * Use 'GetChangeToken' to get the change token that you provide in the @ChangeToken@ parameter of a @CreateRule@ request.
--
--     * Submit a @CreateRateBasedRule@ request.
--
--     * Use @GetChangeToken@ to get the change token that you provide in the @ChangeToken@ parameter of an 'UpdateRule' request.
--
--     * Submit an @UpdateRateBasedRule@ request to specify the predicates that you want to include in the rule.
--
--     * Create and update a @WebACL@ that contains the @RateBasedRule@ . For more information, see 'CreateWebACL' .
--
--
--
-- For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <http://docs.aws.amazon.com/waf/latest/developerguide/ AWS WAF Developer Guide> .
--
module Network.AWS.WAF.CreateRateBasedRule
    (
    -- * Creating a Request
      createRateBasedRule
    , CreateRateBasedRule
    -- * Request Lenses
    , crbrName
    , crbrMetricName
    , crbrRateKey
    , crbrRateLimit
    , crbrChangeToken

    -- * Destructuring the Response
    , createRateBasedRuleResponse
    , CreateRateBasedRuleResponse
    -- * Response Lenses
    , crbrrsRule
    , crbrrsChangeToken
    , crbrrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WAF.Types
import Network.AWS.WAF.Types.Product

-- | /See:/ 'createRateBasedRule' smart constructor.
data CreateRateBasedRule = CreateRateBasedRule'
  { _crbrName        :: !Text
  , _crbrMetricName  :: !Text
  , _crbrRateKey     :: !RateKey
  , _crbrRateLimit   :: !Nat
  , _crbrChangeToken :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateRateBasedRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crbrName' - A friendly name or description of the 'RateBasedRule' . You can't change the name of a @RateBasedRule@ after you create it.
--
-- * 'crbrMetricName' - A friendly name or description for the metrics for this @RateBasedRule@ . The name can contain only alphanumeric characters (A-Z, a-z, 0-9); the name can't contain whitespace. You can't change the name of the metric after you create the @RateBasedRule@ .
--
-- * 'crbrRateKey' - The field that AWS WAF uses to determine if requests are likely arriving from a single source and thus subject to rate monitoring. The only valid value for @RateKey@ is @IP@ . @IP@ indicates that requests that arrive from the same IP address are subject to the @RateLimit@ that is specified in the @RateBasedRule@ .
--
-- * 'crbrRateLimit' - The maximum number of requests, which have an identical value in the field that is specified by @RateKey@ , allowed in a five-minute period. If the number of requests exceeds the @RateLimit@ and the other predicates specified in the rule are also met, AWS WAF triggers the action that is specified for this rule.
--
-- * 'crbrChangeToken' - The @ChangeToken@ that you used to submit the @CreateRateBasedRule@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
createRateBasedRule
    :: Text -- ^ 'crbrName'
    -> Text -- ^ 'crbrMetricName'
    -> RateKey -- ^ 'crbrRateKey'
    -> Natural -- ^ 'crbrRateLimit'
    -> Text -- ^ 'crbrChangeToken'
    -> CreateRateBasedRule
createRateBasedRule pName_ pMetricName_ pRateKey_ pRateLimit_ pChangeToken_ =
  CreateRateBasedRule'
    { _crbrName = pName_
    , _crbrMetricName = pMetricName_
    , _crbrRateKey = pRateKey_
    , _crbrRateLimit = _Nat # pRateLimit_
    , _crbrChangeToken = pChangeToken_
    }


-- | A friendly name or description of the 'RateBasedRule' . You can't change the name of a @RateBasedRule@ after you create it.
crbrName :: Lens' CreateRateBasedRule Text
crbrName = lens _crbrName (\ s a -> s{_crbrName = a})

-- | A friendly name or description for the metrics for this @RateBasedRule@ . The name can contain only alphanumeric characters (A-Z, a-z, 0-9); the name can't contain whitespace. You can't change the name of the metric after you create the @RateBasedRule@ .
crbrMetricName :: Lens' CreateRateBasedRule Text
crbrMetricName = lens _crbrMetricName (\ s a -> s{_crbrMetricName = a})

-- | The field that AWS WAF uses to determine if requests are likely arriving from a single source and thus subject to rate monitoring. The only valid value for @RateKey@ is @IP@ . @IP@ indicates that requests that arrive from the same IP address are subject to the @RateLimit@ that is specified in the @RateBasedRule@ .
crbrRateKey :: Lens' CreateRateBasedRule RateKey
crbrRateKey = lens _crbrRateKey (\ s a -> s{_crbrRateKey = a})

-- | The maximum number of requests, which have an identical value in the field that is specified by @RateKey@ , allowed in a five-minute period. If the number of requests exceeds the @RateLimit@ and the other predicates specified in the rule are also met, AWS WAF triggers the action that is specified for this rule.
crbrRateLimit :: Lens' CreateRateBasedRule Natural
crbrRateLimit = lens _crbrRateLimit (\ s a -> s{_crbrRateLimit = a}) . _Nat

-- | The @ChangeToken@ that you used to submit the @CreateRateBasedRule@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
crbrChangeToken :: Lens' CreateRateBasedRule Text
crbrChangeToken = lens _crbrChangeToken (\ s a -> s{_crbrChangeToken = a})

instance AWSRequest CreateRateBasedRule where
        type Rs CreateRateBasedRule =
             CreateRateBasedRuleResponse
        request = postJSON waf
        response
          = receiveJSON
              (\ s h x ->
                 CreateRateBasedRuleResponse' <$>
                   (x .?> "Rule") <*> (x .?> "ChangeToken") <*>
                     (pure (fromEnum s)))

instance Hashable CreateRateBasedRule where

instance NFData CreateRateBasedRule where

instance ToHeaders CreateRateBasedRule where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSWAF_20150824.CreateRateBasedRule" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateRateBasedRule where
        toJSON CreateRateBasedRule'{..}
          = object
              (catMaybes
                 [Just ("Name" .= _crbrName),
                  Just ("MetricName" .= _crbrMetricName),
                  Just ("RateKey" .= _crbrRateKey),
                  Just ("RateLimit" .= _crbrRateLimit),
                  Just ("ChangeToken" .= _crbrChangeToken)])

instance ToPath CreateRateBasedRule where
        toPath = const "/"

instance ToQuery CreateRateBasedRule where
        toQuery = const mempty

-- | /See:/ 'createRateBasedRuleResponse' smart constructor.
data CreateRateBasedRuleResponse = CreateRateBasedRuleResponse'
  { _crbrrsRule           :: !(Maybe RateBasedRule)
  , _crbrrsChangeToken    :: !(Maybe Text)
  , _crbrrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateRateBasedRuleResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crbrrsRule' - The 'RateBasedRule' that is returned in the @CreateRateBasedRule@ response.
--
-- * 'crbrrsChangeToken' - The @ChangeToken@ that you used to submit the @CreateRateBasedRule@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- * 'crbrrsResponseStatus' - -- | The response status code.
createRateBasedRuleResponse
    :: Int -- ^ 'crbrrsResponseStatus'
    -> CreateRateBasedRuleResponse
createRateBasedRuleResponse pResponseStatus_ =
  CreateRateBasedRuleResponse'
    { _crbrrsRule = Nothing
    , _crbrrsChangeToken = Nothing
    , _crbrrsResponseStatus = pResponseStatus_
    }


-- | The 'RateBasedRule' that is returned in the @CreateRateBasedRule@ response.
crbrrsRule :: Lens' CreateRateBasedRuleResponse (Maybe RateBasedRule)
crbrrsRule = lens _crbrrsRule (\ s a -> s{_crbrrsRule = a})

-- | The @ChangeToken@ that you used to submit the @CreateRateBasedRule@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
crbrrsChangeToken :: Lens' CreateRateBasedRuleResponse (Maybe Text)
crbrrsChangeToken = lens _crbrrsChangeToken (\ s a -> s{_crbrrsChangeToken = a})

-- | -- | The response status code.
crbrrsResponseStatus :: Lens' CreateRateBasedRuleResponse Int
crbrrsResponseStatus = lens _crbrrsResponseStatus (\ s a -> s{_crbrrsResponseStatus = a})

instance NFData CreateRateBasedRuleResponse where
