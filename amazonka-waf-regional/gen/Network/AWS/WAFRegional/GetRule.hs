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
-- Module      : Network.AWS.WAFRegional.GetRule
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the 'Rule' that is specified by the @RuleId@ that you included in the @GetRule@ request.
--
--
module Network.AWS.WAFRegional.GetRule
    (
    -- * Creating a Request
      getRule
    , GetRule
    -- * Request Lenses
    , grRuleId

    -- * Destructuring the Response
    , getRuleResponse
    , GetRuleResponse
    -- * Response Lenses
    , grrsRule
    , grrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WAFRegional.Types
import Network.AWS.WAFRegional.Types.Product

-- | /See:/ 'getRule' smart constructor.
newtype GetRule = GetRule'
  { _grRuleId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grRuleId' - The @RuleId@ of the 'Rule' that you want to get. @RuleId@ is returned by 'CreateRule' and by 'ListRules' .
getRule
    :: Text -- ^ 'grRuleId'
    -> GetRule
getRule pRuleId_ = GetRule' {_grRuleId = pRuleId_}


-- | The @RuleId@ of the 'Rule' that you want to get. @RuleId@ is returned by 'CreateRule' and by 'ListRules' .
grRuleId :: Lens' GetRule Text
grRuleId = lens _grRuleId (\ s a -> s{_grRuleId = a})

instance AWSRequest GetRule where
        type Rs GetRule = GetRuleResponse
        request = postJSON wAFRegional
        response
          = receiveJSON
              (\ s h x ->
                 GetRuleResponse' <$>
                   (x .?> "Rule") <*> (pure (fromEnum s)))

instance Hashable GetRule where

instance NFData GetRule where

instance ToHeaders GetRule where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSWAF_Regional_20161128.GetRule" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetRule where
        toJSON GetRule'{..}
          = object (catMaybes [Just ("RuleId" .= _grRuleId)])

instance ToPath GetRule where
        toPath = const "/"

instance ToQuery GetRule where
        toQuery = const mempty

-- | /See:/ 'getRuleResponse' smart constructor.
data GetRuleResponse = GetRuleResponse'
  { _grrsRule           :: !(Maybe Rule)
  , _grrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetRuleResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grrsRule' - Information about the 'Rule' that you specified in the @GetRule@ request. For more information, see the following topics:     * 'Rule' : Contains @MetricName@ , @Name@ , an array of @Predicate@ objects, and @RuleId@      * 'Predicate' : Each @Predicate@ object contains @DataId@ , @Negated@ , and @Type@
--
-- * 'grrsResponseStatus' - -- | The response status code.
getRuleResponse
    :: Int -- ^ 'grrsResponseStatus'
    -> GetRuleResponse
getRuleResponse pResponseStatus_ =
  GetRuleResponse' {_grrsRule = Nothing, _grrsResponseStatus = pResponseStatus_}


-- | Information about the 'Rule' that you specified in the @GetRule@ request. For more information, see the following topics:     * 'Rule' : Contains @MetricName@ , @Name@ , an array of @Predicate@ objects, and @RuleId@      * 'Predicate' : Each @Predicate@ object contains @DataId@ , @Negated@ , and @Type@
grrsRule :: Lens' GetRuleResponse (Maybe Rule)
grrsRule = lens _grrsRule (\ s a -> s{_grrsRule = a})

-- | -- | The response status code.
grrsResponseStatus :: Lens' GetRuleResponse Int
grrsResponseStatus = lens _grrsResponseStatus (\ s a -> s{_grrsResponseStatus = a})

instance NFData GetRuleResponse where
