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
-- Module      : Network.AWS.WAFRegional.UpdateRuleGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Inserts or deletes 'ActivatedRule' objects in a @RuleGroup@ .
--
--
-- You can only insert @REGULAR@ rules into a rule group.
--
-- You can have a maximum of ten rules per rule group.
--
-- To create and configure a @RuleGroup@ , perform the following steps:
--
--     * Create and update the @Rules@ that you want to include in the @RuleGroup@ . See 'CreateRule' .
--
--     * Use @GetChangeToken@ to get the change token that you provide in the @ChangeToken@ parameter of an 'UpdateRuleGroup' request.
--
--     * Submit an @UpdateRuleGroup@ request to add @Rules@ to the @RuleGroup@ .
--
--     * Create and update a @WebACL@ that contains the @RuleGroup@ . See 'CreateWebACL' .
--
--
--
-- If you want to replace one @Rule@ with another, you delete the existing one and add the new one.
--
-- For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <http://docs.aws.amazon.com/waf/latest/developerguide/ AWS WAF Developer Guide> .
--
module Network.AWS.WAFRegional.UpdateRuleGroup
    (
    -- * Creating a Request
      updateRuleGroup
    , UpdateRuleGroup
    -- * Request Lenses
    , urgRuleGroupId
    , urgUpdates
    , urgChangeToken

    -- * Destructuring the Response
    , updateRuleGroupResponse
    , UpdateRuleGroupResponse
    -- * Response Lenses
    , urgrsChangeToken
    , urgrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WAFRegional.Types
import Network.AWS.WAFRegional.Types.Product

-- | /See:/ 'updateRuleGroup' smart constructor.
data UpdateRuleGroup = UpdateRuleGroup'
  { _urgRuleGroupId :: !Text
  , _urgUpdates     :: !(List1 RuleGroupUpdate)
  , _urgChangeToken :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateRuleGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'urgRuleGroupId' - The @RuleGroupId@ of the 'RuleGroup' that you want to update. @RuleGroupId@ is returned by 'CreateRuleGroup' and by 'ListRuleGroups' .
--
-- * 'urgUpdates' - An array of @RuleGroupUpdate@ objects that you want to insert into or delete from a 'RuleGroup' . You can only insert @REGULAR@ rules into a rule group. @ActivatedRule|OverrideAction@ applies only when updating or adding a @RuleGroup@ to a @WebACL@ . In this case you do not use @ActivatedRule|Action@ . For all other update requests, @ActivatedRule|Action@ is used instead of @ActivatedRule|OverrideAction@ .
--
-- * 'urgChangeToken' - The value returned by the most recent call to 'GetChangeToken' .
updateRuleGroup
    :: Text -- ^ 'urgRuleGroupId'
    -> NonEmpty RuleGroupUpdate -- ^ 'urgUpdates'
    -> Text -- ^ 'urgChangeToken'
    -> UpdateRuleGroup
updateRuleGroup pRuleGroupId_ pUpdates_ pChangeToken_ =
  UpdateRuleGroup'
    { _urgRuleGroupId = pRuleGroupId_
    , _urgUpdates = _List1 # pUpdates_
    , _urgChangeToken = pChangeToken_
    }


-- | The @RuleGroupId@ of the 'RuleGroup' that you want to update. @RuleGroupId@ is returned by 'CreateRuleGroup' and by 'ListRuleGroups' .
urgRuleGroupId :: Lens' UpdateRuleGroup Text
urgRuleGroupId = lens _urgRuleGroupId (\ s a -> s{_urgRuleGroupId = a})

-- | An array of @RuleGroupUpdate@ objects that you want to insert into or delete from a 'RuleGroup' . You can only insert @REGULAR@ rules into a rule group. @ActivatedRule|OverrideAction@ applies only when updating or adding a @RuleGroup@ to a @WebACL@ . In this case you do not use @ActivatedRule|Action@ . For all other update requests, @ActivatedRule|Action@ is used instead of @ActivatedRule|OverrideAction@ .
urgUpdates :: Lens' UpdateRuleGroup (NonEmpty RuleGroupUpdate)
urgUpdates = lens _urgUpdates (\ s a -> s{_urgUpdates = a}) . _List1

-- | The value returned by the most recent call to 'GetChangeToken' .
urgChangeToken :: Lens' UpdateRuleGroup Text
urgChangeToken = lens _urgChangeToken (\ s a -> s{_urgChangeToken = a})

instance AWSRequest UpdateRuleGroup where
        type Rs UpdateRuleGroup = UpdateRuleGroupResponse
        request = postJSON wAFRegional
        response
          = receiveJSON
              (\ s h x ->
                 UpdateRuleGroupResponse' <$>
                   (x .?> "ChangeToken") <*> (pure (fromEnum s)))

instance Hashable UpdateRuleGroup where

instance NFData UpdateRuleGroup where

instance ToHeaders UpdateRuleGroup where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSWAF_Regional_20161128.UpdateRuleGroup" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateRuleGroup where
        toJSON UpdateRuleGroup'{..}
          = object
              (catMaybes
                 [Just ("RuleGroupId" .= _urgRuleGroupId),
                  Just ("Updates" .= _urgUpdates),
                  Just ("ChangeToken" .= _urgChangeToken)])

instance ToPath UpdateRuleGroup where
        toPath = const "/"

instance ToQuery UpdateRuleGroup where
        toQuery = const mempty

-- | /See:/ 'updateRuleGroupResponse' smart constructor.
data UpdateRuleGroupResponse = UpdateRuleGroupResponse'
  { _urgrsChangeToken    :: !(Maybe Text)
  , _urgrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateRuleGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'urgrsChangeToken' - The @ChangeToken@ that you used to submit the @UpdateRuleGroup@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- * 'urgrsResponseStatus' - -- | The response status code.
updateRuleGroupResponse
    :: Int -- ^ 'urgrsResponseStatus'
    -> UpdateRuleGroupResponse
updateRuleGroupResponse pResponseStatus_ =
  UpdateRuleGroupResponse'
    {_urgrsChangeToken = Nothing, _urgrsResponseStatus = pResponseStatus_}


-- | The @ChangeToken@ that you used to submit the @UpdateRuleGroup@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
urgrsChangeToken :: Lens' UpdateRuleGroupResponse (Maybe Text)
urgrsChangeToken = lens _urgrsChangeToken (\ s a -> s{_urgrsChangeToken = a})

-- | -- | The response status code.
urgrsResponseStatus :: Lens' UpdateRuleGroupResponse Int
urgrsResponseStatus = lens _urgrsResponseStatus (\ s a -> s{_urgrsResponseStatus = a})

instance NFData UpdateRuleGroupResponse where
