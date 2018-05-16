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
-- Module      : Network.AWS.ELBv2.CreateRule
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a rule for the specified listener. The listener must be associated with an Application Load Balancer.
--
--
-- Rules are evaluated in priority order, from the lowest value to the highest value. When the condition for a rule is met, the specified action is taken. If no conditions are met, the action for the default rule is taken. For more information, see <http://docs.aws.amazon.com/elasticloadbalancing/latest/application/load-balancer-listeners.html#listener-rules Listener Rules> in the /Application Load Balancers Guide/ .
--
-- To view your current rules, use 'DescribeRules' . To update a rule, use 'ModifyRule' . To set the priorities of your rules, use 'SetRulePriorities' . To delete a rule, use 'DeleteRule' .
--
module Network.AWS.ELBv2.CreateRule
    (
    -- * Creating a Request
      createRule
    , CreateRule
    -- * Request Lenses
    , crListenerARN
    , crConditions
    , crPriority
    , crActions

    -- * Destructuring the Response
    , createRuleResponse
    , CreateRuleResponse
    -- * Response Lenses
    , crrsRules
    , crrsResponseStatus
    ) where

import Network.AWS.ELBv2.Types
import Network.AWS.ELBv2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createRule' smart constructor.
data CreateRule = CreateRule'
  { _crListenerARN :: !Text
  , _crConditions  :: ![RuleCondition]
  , _crPriority    :: !Nat
  , _crActions     :: ![Action]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crListenerARN' - The Amazon Resource Name (ARN) of the listener.
--
-- * 'crConditions' - The conditions. Each condition specifies a field name and a single value. If the field name is @host-header@ , you can specify a single host name (for example, my.example.com). A host name is case insensitive, can be up to 128 characters in length, and can contain any of the following characters. Note that you can include up to three wildcard characters.     * A-Z, a-z, 0-9     * - .     * * (matches 0 or more characters)     * ? (matches exactly 1 character) If the field name is @path-pattern@ , you can specify a single path pattern. A path pattern is case sensitive, can be up to 128 characters in length, and can contain any of the following characters. Note that you can include up to three wildcard characters.     * A-Z, a-z, 0-9     * _ - . $ / ~ " ' @ : +     * & (using &amp;)     * * (matches 0 or more characters)     * ? (matches exactly 1 character)
--
-- * 'crPriority' - The priority for the rule. A listener can't have multiple rules with the same priority.
--
-- * 'crActions' - An action. Each action has the type @forward@ and specifies a target group.
createRule
    :: Text -- ^ 'crListenerARN'
    -> Natural -- ^ 'crPriority'
    -> CreateRule
createRule pListenerARN_ pPriority_ =
  CreateRule'
    { _crListenerARN = pListenerARN_
    , _crConditions = mempty
    , _crPriority = _Nat # pPriority_
    , _crActions = mempty
    }


-- | The Amazon Resource Name (ARN) of the listener.
crListenerARN :: Lens' CreateRule Text
crListenerARN = lens _crListenerARN (\ s a -> s{_crListenerARN = a})

-- | The conditions. Each condition specifies a field name and a single value. If the field name is @host-header@ , you can specify a single host name (for example, my.example.com). A host name is case insensitive, can be up to 128 characters in length, and can contain any of the following characters. Note that you can include up to three wildcard characters.     * A-Z, a-z, 0-9     * - .     * * (matches 0 or more characters)     * ? (matches exactly 1 character) If the field name is @path-pattern@ , you can specify a single path pattern. A path pattern is case sensitive, can be up to 128 characters in length, and can contain any of the following characters. Note that you can include up to three wildcard characters.     * A-Z, a-z, 0-9     * _ - . $ / ~ " ' @ : +     * & (using &amp;)     * * (matches 0 or more characters)     * ? (matches exactly 1 character)
crConditions :: Lens' CreateRule [RuleCondition]
crConditions = lens _crConditions (\ s a -> s{_crConditions = a}) . _Coerce

-- | The priority for the rule. A listener can't have multiple rules with the same priority.
crPriority :: Lens' CreateRule Natural
crPriority = lens _crPriority (\ s a -> s{_crPriority = a}) . _Nat

-- | An action. Each action has the type @forward@ and specifies a target group.
crActions :: Lens' CreateRule [Action]
crActions = lens _crActions (\ s a -> s{_crActions = a}) . _Coerce

instance AWSRequest CreateRule where
        type Rs CreateRule = CreateRuleResponse
        request = postQuery eLBv2
        response
          = receiveXMLWrapper "CreateRuleResult"
              (\ s h x ->
                 CreateRuleResponse' <$>
                   (x .@? "Rules" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

instance Hashable CreateRule where

instance NFData CreateRule where

instance ToHeaders CreateRule where
        toHeaders = const mempty

instance ToPath CreateRule where
        toPath = const "/"

instance ToQuery CreateRule where
        toQuery CreateRule'{..}
          = mconcat
              ["Action" =: ("CreateRule" :: ByteString),
               "Version" =: ("2015-12-01" :: ByteString),
               "ListenerArn" =: _crListenerARN,
               "Conditions" =: toQueryList "member" _crConditions,
               "Priority" =: _crPriority,
               "Actions" =: toQueryList "member" _crActions]

-- | /See:/ 'createRuleResponse' smart constructor.
data CreateRuleResponse = CreateRuleResponse'
  { _crrsRules          :: !(Maybe [Rule])
  , _crrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateRuleResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crrsRules' - Information about the rule.
--
-- * 'crrsResponseStatus' - -- | The response status code.
createRuleResponse
    :: Int -- ^ 'crrsResponseStatus'
    -> CreateRuleResponse
createRuleResponse pResponseStatus_ =
  CreateRuleResponse'
    {_crrsRules = Nothing, _crrsResponseStatus = pResponseStatus_}


-- | Information about the rule.
crrsRules :: Lens' CreateRuleResponse [Rule]
crrsRules = lens _crrsRules (\ s a -> s{_crrsRules = a}) . _Default . _Coerce

-- | -- | The response status code.
crrsResponseStatus :: Lens' CreateRuleResponse Int
crrsResponseStatus = lens _crrsResponseStatus (\ s a -> s{_crrsResponseStatus = a})

instance NFData CreateRuleResponse where
