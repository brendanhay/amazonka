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
-- Module      : Network.AWS.WAFRegional.UpdateRule
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Inserts or deletes 'Predicate' objects in a @Rule@ . Each @Predicate@ object identifies a predicate, such as a 'ByteMatchSet' or an 'IPSet' , that specifies the web requests that you want to allow, block, or count. If you add more than one predicate to a @Rule@ , a request must match all of the specifications to be allowed, blocked, or counted. For example, suppose you add the following to a @Rule@ :
--
--
--     * A @ByteMatchSet@ that matches the value @BadBot@ in the @User-Agent@ header
--
--     * An @IPSet@ that matches the IP address @192.0.2.44@
--
--
--
-- You then add the @Rule@ to a @WebACL@ and specify that you want to block requests that satisfy the @Rule@ . For a request to be blocked, the @User-Agent@ header in the request must contain the value @BadBot@ /and/ the request must originate from the IP address 192.0.2.44.
--
-- To create and configure a @Rule@ , perform the following steps:
--
--     * Create and update the predicates that you want to include in the @Rule@ .
--
--     * Create the @Rule@ . See 'CreateRule' .
--
--     * Use @GetChangeToken@ to get the change token that you provide in the @ChangeToken@ parameter of an 'UpdateRule' request.
--
--     * Submit an @UpdateRule@ request to add predicates to the @Rule@ .
--
--     * Create and update a @WebACL@ that contains the @Rule@ . See 'CreateWebACL' .
--
--
--
-- If you want to replace one @ByteMatchSet@ or @IPSet@ with another, you delete the existing one and add the new one.
--
-- For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <http://docs.aws.amazon.com/waf/latest/developerguide/ AWS WAF Developer Guide> .
--
module Network.AWS.WAFRegional.UpdateRule
    (
    -- * Creating a Request
      updateRule
    , UpdateRule
    -- * Request Lenses
    , urRuleId
    , urChangeToken
    , urUpdates

    -- * Destructuring the Response
    , updateRuleResponse
    , UpdateRuleResponse
    -- * Response Lenses
    , urrsChangeToken
    , urrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WAFRegional.Types
import Network.AWS.WAFRegional.Types.Product

-- | /See:/ 'updateRule' smart constructor.
data UpdateRule = UpdateRule'
  { _urRuleId      :: !Text
  , _urChangeToken :: !Text
  , _urUpdates     :: ![RuleUpdate]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'urRuleId' - The @RuleId@ of the @Rule@ that you want to update. @RuleId@ is returned by @CreateRule@ and by 'ListRules' .
--
-- * 'urChangeToken' - The value returned by the most recent call to 'GetChangeToken' .
--
-- * 'urUpdates' - An array of @RuleUpdate@ objects that you want to insert into or delete from a 'Rule' . For more information, see the applicable data types:     * 'RuleUpdate' : Contains @Action@ and @Predicate@      * 'Predicate' : Contains @DataId@ , @Negated@ , and @Type@      * 'FieldToMatch' : Contains @Data@ and @Type@
updateRule
    :: Text -- ^ 'urRuleId'
    -> Text -- ^ 'urChangeToken'
    -> UpdateRule
updateRule pRuleId_ pChangeToken_ =
  UpdateRule'
    {_urRuleId = pRuleId_, _urChangeToken = pChangeToken_, _urUpdates = mempty}


-- | The @RuleId@ of the @Rule@ that you want to update. @RuleId@ is returned by @CreateRule@ and by 'ListRules' .
urRuleId :: Lens' UpdateRule Text
urRuleId = lens _urRuleId (\ s a -> s{_urRuleId = a})

-- | The value returned by the most recent call to 'GetChangeToken' .
urChangeToken :: Lens' UpdateRule Text
urChangeToken = lens _urChangeToken (\ s a -> s{_urChangeToken = a})

-- | An array of @RuleUpdate@ objects that you want to insert into or delete from a 'Rule' . For more information, see the applicable data types:     * 'RuleUpdate' : Contains @Action@ and @Predicate@      * 'Predicate' : Contains @DataId@ , @Negated@ , and @Type@      * 'FieldToMatch' : Contains @Data@ and @Type@
urUpdates :: Lens' UpdateRule [RuleUpdate]
urUpdates = lens _urUpdates (\ s a -> s{_urUpdates = a}) . _Coerce

instance AWSRequest UpdateRule where
        type Rs UpdateRule = UpdateRuleResponse
        request = postJSON wAFRegional
        response
          = receiveJSON
              (\ s h x ->
                 UpdateRuleResponse' <$>
                   (x .?> "ChangeToken") <*> (pure (fromEnum s)))

instance Hashable UpdateRule where

instance NFData UpdateRule where

instance ToHeaders UpdateRule where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSWAF_Regional_20161128.UpdateRule" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateRule where
        toJSON UpdateRule'{..}
          = object
              (catMaybes
                 [Just ("RuleId" .= _urRuleId),
                  Just ("ChangeToken" .= _urChangeToken),
                  Just ("Updates" .= _urUpdates)])

instance ToPath UpdateRule where
        toPath = const "/"

instance ToQuery UpdateRule where
        toQuery = const mempty

-- | /See:/ 'updateRuleResponse' smart constructor.
data UpdateRuleResponse = UpdateRuleResponse'
  { _urrsChangeToken    :: !(Maybe Text)
  , _urrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateRuleResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'urrsChangeToken' - The @ChangeToken@ that you used to submit the @UpdateRule@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- * 'urrsResponseStatus' - -- | The response status code.
updateRuleResponse
    :: Int -- ^ 'urrsResponseStatus'
    -> UpdateRuleResponse
updateRuleResponse pResponseStatus_ =
  UpdateRuleResponse'
    {_urrsChangeToken = Nothing, _urrsResponseStatus = pResponseStatus_}


-- | The @ChangeToken@ that you used to submit the @UpdateRule@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
urrsChangeToken :: Lens' UpdateRuleResponse (Maybe Text)
urrsChangeToken = lens _urrsChangeToken (\ s a -> s{_urrsChangeToken = a})

-- | -- | The response status code.
urrsResponseStatus :: Lens' UpdateRuleResponse Int
urrsResponseStatus = lens _urrsResponseStatus (\ s a -> s{_urrsResponseStatus = a})

instance NFData UpdateRuleResponse where
