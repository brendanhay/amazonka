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
-- Module      : Network.AWS.WAFRegional.DeleteRuleGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently deletes a 'RuleGroup' . You can't delete a @RuleGroup@ if it's still used in any @WebACL@ objects or if it still includes any rules.
--
--
-- If you just want to remove a @RuleGroup@ from a @WebACL@ , use 'UpdateWebACL' .
--
-- To permanently delete a @RuleGroup@ from AWS WAF, perform the following steps:
--
--     * Update the @RuleGroup@ to remove rules, if any. For more information, see 'UpdateRuleGroup' .
--
--     * Use 'GetChangeToken' to get the change token that you provide in the @ChangeToken@ parameter of a @DeleteRuleGroup@ request.
--
--     * Submit a @DeleteRuleGroup@ request.
--
--
--
module Network.AWS.WAFRegional.DeleteRuleGroup
    (
    -- * Creating a Request
      deleteRuleGroup
    , DeleteRuleGroup
    -- * Request Lenses
    , drgRuleGroupId
    , drgChangeToken

    -- * Destructuring the Response
    , deleteRuleGroupResponse
    , DeleteRuleGroupResponse
    -- * Response Lenses
    , drgrsChangeToken
    , drgrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WAFRegional.Types
import Network.AWS.WAFRegional.Types.Product

-- | /See:/ 'deleteRuleGroup' smart constructor.
data DeleteRuleGroup = DeleteRuleGroup'
  { _drgRuleGroupId :: !Text
  , _drgChangeToken :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteRuleGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drgRuleGroupId' - The @RuleGroupId@ of the 'RuleGroup' that you want to delete. @RuleGroupId@ is returned by 'CreateRuleGroup' and by 'ListRuleGroups' .
--
-- * 'drgChangeToken' - The value returned by the most recent call to 'GetChangeToken' .
deleteRuleGroup
    :: Text -- ^ 'drgRuleGroupId'
    -> Text -- ^ 'drgChangeToken'
    -> DeleteRuleGroup
deleteRuleGroup pRuleGroupId_ pChangeToken_ =
  DeleteRuleGroup'
    {_drgRuleGroupId = pRuleGroupId_, _drgChangeToken = pChangeToken_}


-- | The @RuleGroupId@ of the 'RuleGroup' that you want to delete. @RuleGroupId@ is returned by 'CreateRuleGroup' and by 'ListRuleGroups' .
drgRuleGroupId :: Lens' DeleteRuleGroup Text
drgRuleGroupId = lens _drgRuleGroupId (\ s a -> s{_drgRuleGroupId = a})

-- | The value returned by the most recent call to 'GetChangeToken' .
drgChangeToken :: Lens' DeleteRuleGroup Text
drgChangeToken = lens _drgChangeToken (\ s a -> s{_drgChangeToken = a})

instance AWSRequest DeleteRuleGroup where
        type Rs DeleteRuleGroup = DeleteRuleGroupResponse
        request = postJSON wAFRegional
        response
          = receiveJSON
              (\ s h x ->
                 DeleteRuleGroupResponse' <$>
                   (x .?> "ChangeToken") <*> (pure (fromEnum s)))

instance Hashable DeleteRuleGroup where

instance NFData DeleteRuleGroup where

instance ToHeaders DeleteRuleGroup where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSWAF_Regional_20161128.DeleteRuleGroup" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteRuleGroup where
        toJSON DeleteRuleGroup'{..}
          = object
              (catMaybes
                 [Just ("RuleGroupId" .= _drgRuleGroupId),
                  Just ("ChangeToken" .= _drgChangeToken)])

instance ToPath DeleteRuleGroup where
        toPath = const "/"

instance ToQuery DeleteRuleGroup where
        toQuery = const mempty

-- | /See:/ 'deleteRuleGroupResponse' smart constructor.
data DeleteRuleGroupResponse = DeleteRuleGroupResponse'
  { _drgrsChangeToken    :: !(Maybe Text)
  , _drgrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteRuleGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drgrsChangeToken' - The @ChangeToken@ that you used to submit the @DeleteRuleGroup@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- * 'drgrsResponseStatus' - -- | The response status code.
deleteRuleGroupResponse
    :: Int -- ^ 'drgrsResponseStatus'
    -> DeleteRuleGroupResponse
deleteRuleGroupResponse pResponseStatus_ =
  DeleteRuleGroupResponse'
    {_drgrsChangeToken = Nothing, _drgrsResponseStatus = pResponseStatus_}


-- | The @ChangeToken@ that you used to submit the @DeleteRuleGroup@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
drgrsChangeToken :: Lens' DeleteRuleGroupResponse (Maybe Text)
drgrsChangeToken = lens _drgrsChangeToken (\ s a -> s{_drgrsChangeToken = a})

-- | -- | The response status code.
drgrsResponseStatus :: Lens' DeleteRuleGroupResponse Int
drgrsResponseStatus = lens _drgrsResponseStatus (\ s a -> s{_drgrsResponseStatus = a})

instance NFData DeleteRuleGroupResponse where
