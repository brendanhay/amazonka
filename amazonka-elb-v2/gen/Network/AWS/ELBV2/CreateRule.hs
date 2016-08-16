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
-- Module      : Network.AWS.ELBV2.CreateRule
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a rule for the specified listener.
--
-- A rule consists conditions and actions. Rules are evaluated in priority order, from the lowest value to the highest value. When the conditions for a rule are met, the specified actions are taken. If no rule\'s conditions are met, the default actions for the listener are taken.
--
-- To view your current rules, use < DescribeRules>. To update a rule, use < ModifyRule>. To set the priorities of your rules, use < SetRulePriorities>. To delete a rule, use < DeleteRule>.
module Network.AWS.ELBV2.CreateRule
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

import           Network.AWS.ELBV2.Types
import           Network.AWS.ELBV2.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Contains the parameters for CreateRule.
--
-- /See:/ 'createRule' smart constructor.
data CreateRule = CreateRule'
    { _crListenerARN :: !Text
    , _crConditions  :: ![RuleCondition]
    , _crPriority    :: !Nat
    , _crActions     :: ![Action]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crListenerARN'
--
-- * 'crConditions'
--
-- * 'crPriority'
--
-- * 'crActions'
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
crListenerARN = lens _crListenerARN (\ s a -> s{_crListenerARN = a});

-- | The conditions.
crConditions :: Lens' CreateRule [RuleCondition]
crConditions = lens _crConditions (\ s a -> s{_crConditions = a}) . _Coerce;

-- | The priority for the rule. A listener can\'t have multiple rules with the same priority.
crPriority :: Lens' CreateRule Natural
crPriority = lens _crPriority (\ s a -> s{_crPriority = a}) . _Nat;

-- | The actions for the rule.
crActions :: Lens' CreateRule [Action]
crActions = lens _crActions (\ s a -> s{_crActions = a}) . _Coerce;

instance AWSRequest CreateRule where
        type Rs CreateRule = CreateRuleResponse
        request = postQuery elbv2
        response
          = receiveXMLWrapper "CreateRuleResult"
              (\ s h x ->
                 CreateRuleResponse' <$>
                   (x .@? "Rules" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

instance Hashable CreateRule

instance NFData CreateRule

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

-- | Contains the output of CreateRule.
--
-- /See:/ 'createRuleResponse' smart constructor.
data CreateRuleResponse = CreateRuleResponse'
    { _crrsRules          :: !(Maybe [Rule])
    , _crrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateRuleResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crrsRules'
--
-- * 'crrsResponseStatus'
createRuleResponse
    :: Int -- ^ 'crrsResponseStatus'
    -> CreateRuleResponse
createRuleResponse pResponseStatus_ =
    CreateRuleResponse'
    { _crrsRules = Nothing
    , _crrsResponseStatus = pResponseStatus_
    }

-- | Information about the rule.
crrsRules :: Lens' CreateRuleResponse [Rule]
crrsRules = lens _crrsRules (\ s a -> s{_crrsRules = a}) . _Default . _Coerce;

-- | The response status code.
crrsResponseStatus :: Lens' CreateRuleResponse Int
crrsResponseStatus = lens _crrsResponseStatus (\ s a -> s{_crrsResponseStatus = a});

instance NFData CreateRuleResponse
