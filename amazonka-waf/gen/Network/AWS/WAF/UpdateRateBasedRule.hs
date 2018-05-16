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
-- Module      : Network.AWS.WAF.UpdateRateBasedRule
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Inserts or deletes 'Predicate' objects in a rule and updates the @RateLimit@ in the rule.
--
--
-- Each @Predicate@ object identifies a predicate, such as a 'ByteMatchSet' or an 'IPSet' , that specifies the web requests that you want to block or count. The @RateLimit@ specifies the number of requests every five minutes that triggers the rule.
--
-- If you add more than one predicate to a @RateBasedRule@ , a request must match all the predicates and exceed the @RateLimit@ to be counted or blocked. For example, suppose you add the following to a @RateBasedRule@ :
--
--     * An @IPSet@ that matches the IP address @192.0.2.44/32@
--
--     * A @ByteMatchSet@ that matches @BadBot@ in the @User-Agent@ header
--
--
--
-- Further, you specify a @RateLimit@ of 15,000.
--
-- You then add the @RateBasedRule@ to a @WebACL@ and specify that you want to block requests that satisfy the rule. For a request to be blocked, it must come from the IP address 192.0.2.44 /and/ the @User-Agent@ header in the request must contain the value @BadBot@ . Further, requests that match these two conditions much be received at a rate of more than 15,000 every five minutes. If the rate drops below this limit, AWS WAF no longer blocks the requests.
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
module Network.AWS.WAF.UpdateRateBasedRule
    (
    -- * Creating a Request
      updateRateBasedRule
    , UpdateRateBasedRule
    -- * Request Lenses
    , urbrRuleId
    , urbrChangeToken
    , urbrUpdates
    , urbrRateLimit

    -- * Destructuring the Response
    , updateRateBasedRuleResponse
    , UpdateRateBasedRuleResponse
    -- * Response Lenses
    , urbrrsChangeToken
    , urbrrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WAF.Types
import Network.AWS.WAF.Types.Product

-- | /See:/ 'updateRateBasedRule' smart constructor.
data UpdateRateBasedRule = UpdateRateBasedRule'
  { _urbrRuleId      :: !Text
  , _urbrChangeToken :: !Text
  , _urbrUpdates     :: ![RuleUpdate]
  , _urbrRateLimit   :: !Nat
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateRateBasedRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'urbrRuleId' - The @RuleId@ of the @RateBasedRule@ that you want to update. @RuleId@ is returned by @CreateRateBasedRule@ and by 'ListRateBasedRules' .
--
-- * 'urbrChangeToken' - The value returned by the most recent call to 'GetChangeToken' .
--
-- * 'urbrUpdates' - An array of @RuleUpdate@ objects that you want to insert into or delete from a 'RateBasedRule' .
--
-- * 'urbrRateLimit' - The maximum number of requests, which have an identical value in the field specified by the @RateKey@ , allowed in a five-minute period. If the number of requests exceeds the @RateLimit@ and the other predicates specified in the rule are also met, AWS WAF triggers the action that is specified for this rule.
updateRateBasedRule
    :: Text -- ^ 'urbrRuleId'
    -> Text -- ^ 'urbrChangeToken'
    -> Natural -- ^ 'urbrRateLimit'
    -> UpdateRateBasedRule
updateRateBasedRule pRuleId_ pChangeToken_ pRateLimit_ =
  UpdateRateBasedRule'
    { _urbrRuleId = pRuleId_
    , _urbrChangeToken = pChangeToken_
    , _urbrUpdates = mempty
    , _urbrRateLimit = _Nat # pRateLimit_
    }


-- | The @RuleId@ of the @RateBasedRule@ that you want to update. @RuleId@ is returned by @CreateRateBasedRule@ and by 'ListRateBasedRules' .
urbrRuleId :: Lens' UpdateRateBasedRule Text
urbrRuleId = lens _urbrRuleId (\ s a -> s{_urbrRuleId = a})

-- | The value returned by the most recent call to 'GetChangeToken' .
urbrChangeToken :: Lens' UpdateRateBasedRule Text
urbrChangeToken = lens _urbrChangeToken (\ s a -> s{_urbrChangeToken = a})

-- | An array of @RuleUpdate@ objects that you want to insert into or delete from a 'RateBasedRule' .
urbrUpdates :: Lens' UpdateRateBasedRule [RuleUpdate]
urbrUpdates = lens _urbrUpdates (\ s a -> s{_urbrUpdates = a}) . _Coerce

-- | The maximum number of requests, which have an identical value in the field specified by the @RateKey@ , allowed in a five-minute period. If the number of requests exceeds the @RateLimit@ and the other predicates specified in the rule are also met, AWS WAF triggers the action that is specified for this rule.
urbrRateLimit :: Lens' UpdateRateBasedRule Natural
urbrRateLimit = lens _urbrRateLimit (\ s a -> s{_urbrRateLimit = a}) . _Nat

instance AWSRequest UpdateRateBasedRule where
        type Rs UpdateRateBasedRule =
             UpdateRateBasedRuleResponse
        request = postJSON waf
        response
          = receiveJSON
              (\ s h x ->
                 UpdateRateBasedRuleResponse' <$>
                   (x .?> "ChangeToken") <*> (pure (fromEnum s)))

instance Hashable UpdateRateBasedRule where

instance NFData UpdateRateBasedRule where

instance ToHeaders UpdateRateBasedRule where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSWAF_20150824.UpdateRateBasedRule" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateRateBasedRule where
        toJSON UpdateRateBasedRule'{..}
          = object
              (catMaybes
                 [Just ("RuleId" .= _urbrRuleId),
                  Just ("ChangeToken" .= _urbrChangeToken),
                  Just ("Updates" .= _urbrUpdates),
                  Just ("RateLimit" .= _urbrRateLimit)])

instance ToPath UpdateRateBasedRule where
        toPath = const "/"

instance ToQuery UpdateRateBasedRule where
        toQuery = const mempty

-- | /See:/ 'updateRateBasedRuleResponse' smart constructor.
data UpdateRateBasedRuleResponse = UpdateRateBasedRuleResponse'
  { _urbrrsChangeToken    :: !(Maybe Text)
  , _urbrrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateRateBasedRuleResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'urbrrsChangeToken' - The @ChangeToken@ that you used to submit the @UpdateRateBasedRule@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- * 'urbrrsResponseStatus' - -- | The response status code.
updateRateBasedRuleResponse
    :: Int -- ^ 'urbrrsResponseStatus'
    -> UpdateRateBasedRuleResponse
updateRateBasedRuleResponse pResponseStatus_ =
  UpdateRateBasedRuleResponse'
    {_urbrrsChangeToken = Nothing, _urbrrsResponseStatus = pResponseStatus_}


-- | The @ChangeToken@ that you used to submit the @UpdateRateBasedRule@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
urbrrsChangeToken :: Lens' UpdateRateBasedRuleResponse (Maybe Text)
urbrrsChangeToken = lens _urbrrsChangeToken (\ s a -> s{_urbrrsChangeToken = a})

-- | -- | The response status code.
urbrrsResponseStatus :: Lens' UpdateRateBasedRuleResponse Int
urbrrsResponseStatus = lens _urbrrsResponseStatus (\ s a -> s{_urbrrsResponseStatus = a})

instance NFData UpdateRateBasedRuleResponse where
