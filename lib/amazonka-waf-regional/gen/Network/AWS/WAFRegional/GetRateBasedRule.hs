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
-- Module      : Network.AWS.WAFRegional.GetRateBasedRule
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the 'RateBasedRule' that is specified by the @RuleId@ that you included in the @GetRateBasedRule@ request.
--
--
module Network.AWS.WAFRegional.GetRateBasedRule
    (
    -- * Creating a Request
      getRateBasedRule
    , GetRateBasedRule
    -- * Request Lenses
    , grbrRuleId

    -- * Destructuring the Response
    , getRateBasedRuleResponse
    , GetRateBasedRuleResponse
    -- * Response Lenses
    , grbrrsRule
    , grbrrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WAFRegional.Types
import Network.AWS.WAFRegional.Types.Product

-- | /See:/ 'getRateBasedRule' smart constructor.
newtype GetRateBasedRule = GetRateBasedRule'
  { _grbrRuleId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetRateBasedRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grbrRuleId' - The @RuleId@ of the 'RateBasedRule' that you want to get. @RuleId@ is returned by 'CreateRateBasedRule' and by 'ListRateBasedRules' .
getRateBasedRule
    :: Text -- ^ 'grbrRuleId'
    -> GetRateBasedRule
getRateBasedRule pRuleId_ = GetRateBasedRule' {_grbrRuleId = pRuleId_}


-- | The @RuleId@ of the 'RateBasedRule' that you want to get. @RuleId@ is returned by 'CreateRateBasedRule' and by 'ListRateBasedRules' .
grbrRuleId :: Lens' GetRateBasedRule Text
grbrRuleId = lens _grbrRuleId (\ s a -> s{_grbrRuleId = a})

instance AWSRequest GetRateBasedRule where
        type Rs GetRateBasedRule = GetRateBasedRuleResponse
        request = postJSON wAFRegional
        response
          = receiveJSON
              (\ s h x ->
                 GetRateBasedRuleResponse' <$>
                   (x .?> "Rule") <*> (pure (fromEnum s)))

instance Hashable GetRateBasedRule where

instance NFData GetRateBasedRule where

instance ToHeaders GetRateBasedRule where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSWAF_Regional_20161128.GetRateBasedRule" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetRateBasedRule where
        toJSON GetRateBasedRule'{..}
          = object (catMaybes [Just ("RuleId" .= _grbrRuleId)])

instance ToPath GetRateBasedRule where
        toPath = const "/"

instance ToQuery GetRateBasedRule where
        toQuery = const mempty

-- | /See:/ 'getRateBasedRuleResponse' smart constructor.
data GetRateBasedRuleResponse = GetRateBasedRuleResponse'
  { _grbrrsRule           :: !(Maybe RateBasedRule)
  , _grbrrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetRateBasedRuleResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grbrrsRule' - Information about the 'RateBasedRule' that you specified in the @GetRateBasedRule@ request.
--
-- * 'grbrrsResponseStatus' - -- | The response status code.
getRateBasedRuleResponse
    :: Int -- ^ 'grbrrsResponseStatus'
    -> GetRateBasedRuleResponse
getRateBasedRuleResponse pResponseStatus_ =
  GetRateBasedRuleResponse'
    {_grbrrsRule = Nothing, _grbrrsResponseStatus = pResponseStatus_}


-- | Information about the 'RateBasedRule' that you specified in the @GetRateBasedRule@ request.
grbrrsRule :: Lens' GetRateBasedRuleResponse (Maybe RateBasedRule)
grbrrsRule = lens _grbrrsRule (\ s a -> s{_grbrrsRule = a})

-- | -- | The response status code.
grbrrsResponseStatus :: Lens' GetRateBasedRuleResponse Int
grbrrsResponseStatus = lens _grbrrsResponseStatus (\ s a -> s{_grbrrsResponseStatus = a})

instance NFData GetRateBasedRuleResponse where
