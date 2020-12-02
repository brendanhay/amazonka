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
-- Module      : Network.AWS.WAF.DeleteRule
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently deletes a 'Rule' . You can't delete a @Rule@ if it's still used in any @WebACL@ objects or if it still includes any predicates, such as @ByteMatchSet@ objects.
--
--
-- If you just want to remove a @Rule@ from a @WebACL@ , use 'UpdateWebACL' .
--
-- To permanently delete a @Rule@ from AWS WAF, perform the following steps:
--
--     * Update the @Rule@ to remove predicates, if any. For more information, see 'UpdateRule' .
--
--     * Use 'GetChangeToken' to get the change token that you provide in the @ChangeToken@ parameter of a @DeleteRule@ request.
--
--     * Submit a @DeleteRule@ request.
--
--
--
module Network.AWS.WAF.DeleteRule
    (
    -- * Creating a Request
      deleteRule
    , DeleteRule
    -- * Request Lenses
    , drRuleId
    , drChangeToken

    -- * Destructuring the Response
    , deleteRuleResponse
    , DeleteRuleResponse
    -- * Response Lenses
    , drrsChangeToken
    , drrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WAF.Types
import Network.AWS.WAF.Types.Product

-- | /See:/ 'deleteRule' smart constructor.
data DeleteRule = DeleteRule'
  { _drRuleId      :: !Text
  , _drChangeToken :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drRuleId' - The @RuleId@ of the 'Rule' that you want to delete. @RuleId@ is returned by 'CreateRule' and by 'ListRules' .
--
-- * 'drChangeToken' - The value returned by the most recent call to 'GetChangeToken' .
deleteRule
    :: Text -- ^ 'drRuleId'
    -> Text -- ^ 'drChangeToken'
    -> DeleteRule
deleteRule pRuleId_ pChangeToken_ =
  DeleteRule' {_drRuleId = pRuleId_, _drChangeToken = pChangeToken_}


-- | The @RuleId@ of the 'Rule' that you want to delete. @RuleId@ is returned by 'CreateRule' and by 'ListRules' .
drRuleId :: Lens' DeleteRule Text
drRuleId = lens _drRuleId (\ s a -> s{_drRuleId = a})

-- | The value returned by the most recent call to 'GetChangeToken' .
drChangeToken :: Lens' DeleteRule Text
drChangeToken = lens _drChangeToken (\ s a -> s{_drChangeToken = a})

instance AWSRequest DeleteRule where
        type Rs DeleteRule = DeleteRuleResponse
        request = postJSON waf
        response
          = receiveJSON
              (\ s h x ->
                 DeleteRuleResponse' <$>
                   (x .?> "ChangeToken") <*> (pure (fromEnum s)))

instance Hashable DeleteRule where

instance NFData DeleteRule where

instance ToHeaders DeleteRule where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSWAF_20150824.DeleteRule" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteRule where
        toJSON DeleteRule'{..}
          = object
              (catMaybes
                 [Just ("RuleId" .= _drRuleId),
                  Just ("ChangeToken" .= _drChangeToken)])

instance ToPath DeleteRule where
        toPath = const "/"

instance ToQuery DeleteRule where
        toQuery = const mempty

-- | /See:/ 'deleteRuleResponse' smart constructor.
data DeleteRuleResponse = DeleteRuleResponse'
  { _drrsChangeToken    :: !(Maybe Text)
  , _drrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteRuleResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drrsChangeToken' - The @ChangeToken@ that you used to submit the @DeleteRule@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- * 'drrsResponseStatus' - -- | The response status code.
deleteRuleResponse
    :: Int -- ^ 'drrsResponseStatus'
    -> DeleteRuleResponse
deleteRuleResponse pResponseStatus_ =
  DeleteRuleResponse'
    {_drrsChangeToken = Nothing, _drrsResponseStatus = pResponseStatus_}


-- | The @ChangeToken@ that you used to submit the @DeleteRule@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
drrsChangeToken :: Lens' DeleteRuleResponse (Maybe Text)
drrsChangeToken = lens _drrsChangeToken (\ s a -> s{_drrsChangeToken = a})

-- | -- | The response status code.
drrsResponseStatus :: Lens' DeleteRuleResponse Int
drrsResponseStatus = lens _drrsResponseStatus (\ s a -> s{_drrsResponseStatus = a})

instance NFData DeleteRuleResponse where
