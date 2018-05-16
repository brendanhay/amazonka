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
-- Module      : Network.AWS.WAFRegional.DeleteRateBasedRule
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently deletes a 'RateBasedRule' . You can't delete a rule if it's still used in any @WebACL@ objects or if it still includes any predicates, such as @ByteMatchSet@ objects.
--
--
-- If you just want to remove a rule from a @WebACL@ , use 'UpdateWebACL' .
--
-- To permanently delete a @RateBasedRule@ from AWS WAF, perform the following steps:
--
--     * Update the @RateBasedRule@ to remove predicates, if any. For more information, see 'UpdateRateBasedRule' .
--
--     * Use 'GetChangeToken' to get the change token that you provide in the @ChangeToken@ parameter of a @DeleteRateBasedRule@ request.
--
--     * Submit a @DeleteRateBasedRule@ request.
--
--
--
module Network.AWS.WAFRegional.DeleteRateBasedRule
    (
    -- * Creating a Request
      deleteRateBasedRule
    , DeleteRateBasedRule
    -- * Request Lenses
    , drbrRuleId
    , drbrChangeToken

    -- * Destructuring the Response
    , deleteRateBasedRuleResponse
    , DeleteRateBasedRuleResponse
    -- * Response Lenses
    , drbrrsChangeToken
    , drbrrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WAFRegional.Types
import Network.AWS.WAFRegional.Types.Product

-- | /See:/ 'deleteRateBasedRule' smart constructor.
data DeleteRateBasedRule = DeleteRateBasedRule'
  { _drbrRuleId      :: !Text
  , _drbrChangeToken :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteRateBasedRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drbrRuleId' - The @RuleId@ of the 'RateBasedRule' that you want to delete. @RuleId@ is returned by 'CreateRateBasedRule' and by 'ListRateBasedRules' .
--
-- * 'drbrChangeToken' - The value returned by the most recent call to 'GetChangeToken' .
deleteRateBasedRule
    :: Text -- ^ 'drbrRuleId'
    -> Text -- ^ 'drbrChangeToken'
    -> DeleteRateBasedRule
deleteRateBasedRule pRuleId_ pChangeToken_ =
  DeleteRateBasedRule'
    {_drbrRuleId = pRuleId_, _drbrChangeToken = pChangeToken_}


-- | The @RuleId@ of the 'RateBasedRule' that you want to delete. @RuleId@ is returned by 'CreateRateBasedRule' and by 'ListRateBasedRules' .
drbrRuleId :: Lens' DeleteRateBasedRule Text
drbrRuleId = lens _drbrRuleId (\ s a -> s{_drbrRuleId = a})

-- | The value returned by the most recent call to 'GetChangeToken' .
drbrChangeToken :: Lens' DeleteRateBasedRule Text
drbrChangeToken = lens _drbrChangeToken (\ s a -> s{_drbrChangeToken = a})

instance AWSRequest DeleteRateBasedRule where
        type Rs DeleteRateBasedRule =
             DeleteRateBasedRuleResponse
        request = postJSON wAFRegional
        response
          = receiveJSON
              (\ s h x ->
                 DeleteRateBasedRuleResponse' <$>
                   (x .?> "ChangeToken") <*> (pure (fromEnum s)))

instance Hashable DeleteRateBasedRule where

instance NFData DeleteRateBasedRule where

instance ToHeaders DeleteRateBasedRule where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSWAF_Regional_20161128.DeleteRateBasedRule" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteRateBasedRule where
        toJSON DeleteRateBasedRule'{..}
          = object
              (catMaybes
                 [Just ("RuleId" .= _drbrRuleId),
                  Just ("ChangeToken" .= _drbrChangeToken)])

instance ToPath DeleteRateBasedRule where
        toPath = const "/"

instance ToQuery DeleteRateBasedRule where
        toQuery = const mempty

-- | /See:/ 'deleteRateBasedRuleResponse' smart constructor.
data DeleteRateBasedRuleResponse = DeleteRateBasedRuleResponse'
  { _drbrrsChangeToken    :: !(Maybe Text)
  , _drbrrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteRateBasedRuleResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drbrrsChangeToken' - The @ChangeToken@ that you used to submit the @DeleteRateBasedRule@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- * 'drbrrsResponseStatus' - -- | The response status code.
deleteRateBasedRuleResponse
    :: Int -- ^ 'drbrrsResponseStatus'
    -> DeleteRateBasedRuleResponse
deleteRateBasedRuleResponse pResponseStatus_ =
  DeleteRateBasedRuleResponse'
    {_drbrrsChangeToken = Nothing, _drbrrsResponseStatus = pResponseStatus_}


-- | The @ChangeToken@ that you used to submit the @DeleteRateBasedRule@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
drbrrsChangeToken :: Lens' DeleteRateBasedRuleResponse (Maybe Text)
drbrrsChangeToken = lens _drbrrsChangeToken (\ s a -> s{_drbrrsChangeToken = a})

-- | -- | The response status code.
drbrrsResponseStatus :: Lens' DeleteRateBasedRuleResponse Int
drbrrsResponseStatus = lens _drbrrsResponseStatus (\ s a -> s{_drbrrsResponseStatus = a})

instance NFData DeleteRateBasedRuleResponse where
