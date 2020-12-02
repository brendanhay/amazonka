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
-- Module      : Network.AWS.ELBv2.ModifyRule
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the specified rule.
--
--
-- Any existing properties that you do not modify retain their current values.
--
-- To modify the default action, use 'ModifyListener' .
--
module Network.AWS.ELBv2.ModifyRule
    (
    -- * Creating a Request
      modifyRule
    , ModifyRule
    -- * Request Lenses
    , mrActions
    , mrConditions
    , mrRuleARN

    -- * Destructuring the Response
    , modifyRuleResponse
    , ModifyRuleResponse
    -- * Response Lenses
    , mrrsRules
    , mrrsResponseStatus
    ) where

import Network.AWS.ELBv2.Types
import Network.AWS.ELBv2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'modifyRule' smart constructor.
data ModifyRule = ModifyRule'
  { _mrActions    :: !(Maybe [Action])
  , _mrConditions :: !(Maybe [RuleCondition])
  , _mrRuleARN    :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mrActions' - The actions. The target group must use the HTTP or HTTPS protocol.
--
-- * 'mrConditions' - The conditions.
--
-- * 'mrRuleARN' - The Amazon Resource Name (ARN) of the rule.
modifyRule
    :: Text -- ^ 'mrRuleARN'
    -> ModifyRule
modifyRule pRuleARN_ =
  ModifyRule'
    {_mrActions = Nothing, _mrConditions = Nothing, _mrRuleARN = pRuleARN_}


-- | The actions. The target group must use the HTTP or HTTPS protocol.
mrActions :: Lens' ModifyRule [Action]
mrActions = lens _mrActions (\ s a -> s{_mrActions = a}) . _Default . _Coerce

-- | The conditions.
mrConditions :: Lens' ModifyRule [RuleCondition]
mrConditions = lens _mrConditions (\ s a -> s{_mrConditions = a}) . _Default . _Coerce

-- | The Amazon Resource Name (ARN) of the rule.
mrRuleARN :: Lens' ModifyRule Text
mrRuleARN = lens _mrRuleARN (\ s a -> s{_mrRuleARN = a})

instance AWSRequest ModifyRule where
        type Rs ModifyRule = ModifyRuleResponse
        request = postQuery eLBv2
        response
          = receiveXMLWrapper "ModifyRuleResult"
              (\ s h x ->
                 ModifyRuleResponse' <$>
                   (x .@? "Rules" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

instance Hashable ModifyRule where

instance NFData ModifyRule where

instance ToHeaders ModifyRule where
        toHeaders = const mempty

instance ToPath ModifyRule where
        toPath = const "/"

instance ToQuery ModifyRule where
        toQuery ModifyRule'{..}
          = mconcat
              ["Action" =: ("ModifyRule" :: ByteString),
               "Version" =: ("2015-12-01" :: ByteString),
               "Actions" =:
                 toQuery (toQueryList "member" <$> _mrActions),
               "Conditions" =:
                 toQuery (toQueryList "member" <$> _mrConditions),
               "RuleArn" =: _mrRuleARN]

-- | /See:/ 'modifyRuleResponse' smart constructor.
data ModifyRuleResponse = ModifyRuleResponse'
  { _mrrsRules          :: !(Maybe [Rule])
  , _mrrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyRuleResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mrrsRules' - Information about the rule.
--
-- * 'mrrsResponseStatus' - -- | The response status code.
modifyRuleResponse
    :: Int -- ^ 'mrrsResponseStatus'
    -> ModifyRuleResponse
modifyRuleResponse pResponseStatus_ =
  ModifyRuleResponse'
    {_mrrsRules = Nothing, _mrrsResponseStatus = pResponseStatus_}


-- | Information about the rule.
mrrsRules :: Lens' ModifyRuleResponse [Rule]
mrrsRules = lens _mrrsRules (\ s a -> s{_mrrsRules = a}) . _Default . _Coerce

-- | -- | The response status code.
mrrsResponseStatus :: Lens' ModifyRuleResponse Int
mrrsResponseStatus = lens _mrrsResponseStatus (\ s a -> s{_mrrsResponseStatus = a})

instance NFData ModifyRuleResponse where
