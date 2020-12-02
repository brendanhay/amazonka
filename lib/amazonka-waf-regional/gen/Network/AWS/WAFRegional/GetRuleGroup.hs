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
-- Module      : Network.AWS.WAFRegional.GetRuleGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the 'RuleGroup' that is specified by the @RuleGroupId@ that you included in the @GetRuleGroup@ request.
--
--
-- To view the rules in a rule group, use 'ListActivatedRulesInRuleGroup' .
--
module Network.AWS.WAFRegional.GetRuleGroup
    (
    -- * Creating a Request
      getRuleGroup
    , GetRuleGroup
    -- * Request Lenses
    , grgRuleGroupId

    -- * Destructuring the Response
    , getRuleGroupResponse
    , GetRuleGroupResponse
    -- * Response Lenses
    , grgrsRuleGroup
    , grgrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WAFRegional.Types
import Network.AWS.WAFRegional.Types.Product

-- | /See:/ 'getRuleGroup' smart constructor.
newtype GetRuleGroup = GetRuleGroup'
  { _grgRuleGroupId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetRuleGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grgRuleGroupId' - The @RuleGroupId@ of the 'RuleGroup' that you want to get. @RuleGroupId@ is returned by 'CreateRuleGroup' and by 'ListRuleGroups' .
getRuleGroup
    :: Text -- ^ 'grgRuleGroupId'
    -> GetRuleGroup
getRuleGroup pRuleGroupId_ = GetRuleGroup' {_grgRuleGroupId = pRuleGroupId_}


-- | The @RuleGroupId@ of the 'RuleGroup' that you want to get. @RuleGroupId@ is returned by 'CreateRuleGroup' and by 'ListRuleGroups' .
grgRuleGroupId :: Lens' GetRuleGroup Text
grgRuleGroupId = lens _grgRuleGroupId (\ s a -> s{_grgRuleGroupId = a})

instance AWSRequest GetRuleGroup where
        type Rs GetRuleGroup = GetRuleGroupResponse
        request = postJSON wAFRegional
        response
          = receiveJSON
              (\ s h x ->
                 GetRuleGroupResponse' <$>
                   (x .?> "RuleGroup") <*> (pure (fromEnum s)))

instance Hashable GetRuleGroup where

instance NFData GetRuleGroup where

instance ToHeaders GetRuleGroup where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSWAF_Regional_20161128.GetRuleGroup" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetRuleGroup where
        toJSON GetRuleGroup'{..}
          = object
              (catMaybes [Just ("RuleGroupId" .= _grgRuleGroupId)])

instance ToPath GetRuleGroup where
        toPath = const "/"

instance ToQuery GetRuleGroup where
        toQuery = const mempty

-- | /See:/ 'getRuleGroupResponse' smart constructor.
data GetRuleGroupResponse = GetRuleGroupResponse'
  { _grgrsRuleGroup      :: !(Maybe RuleGroup)
  , _grgrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetRuleGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grgrsRuleGroup' - Information about the 'RuleGroup' that you specified in the @GetRuleGroup@ request.
--
-- * 'grgrsResponseStatus' - -- | The response status code.
getRuleGroupResponse
    :: Int -- ^ 'grgrsResponseStatus'
    -> GetRuleGroupResponse
getRuleGroupResponse pResponseStatus_ =
  GetRuleGroupResponse'
    {_grgrsRuleGroup = Nothing, _grgrsResponseStatus = pResponseStatus_}


-- | Information about the 'RuleGroup' that you specified in the @GetRuleGroup@ request.
grgrsRuleGroup :: Lens' GetRuleGroupResponse (Maybe RuleGroup)
grgrsRuleGroup = lens _grgrsRuleGroup (\ s a -> s{_grgrsRuleGroup = a})

-- | -- | The response status code.
grgrsResponseStatus :: Lens' GetRuleGroupResponse Int
grgrsResponseStatus = lens _grgrsResponseStatus (\ s a -> s{_grgrsResponseStatus = a})

instance NFData GetRuleGroupResponse where
