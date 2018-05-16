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
-- Module      : Network.AWS.Config.GetComplianceSummaryByConfigRule
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the number of AWS Config rules that are compliant and noncompliant, up to a maximum of 25 for each.
--
--
module Network.AWS.Config.GetComplianceSummaryByConfigRule
    (
    -- * Creating a Request
      getComplianceSummaryByConfigRule
    , GetComplianceSummaryByConfigRule

    -- * Destructuring the Response
    , getComplianceSummaryByConfigRuleResponse
    , GetComplianceSummaryByConfigRuleResponse
    -- * Response Lenses
    , gcsbcrrsComplianceSummary
    , gcsbcrrsResponseStatus
    ) where

import Network.AWS.Config.Types
import Network.AWS.Config.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getComplianceSummaryByConfigRule' smart constructor.
data GetComplianceSummaryByConfigRule =
  GetComplianceSummaryByConfigRule'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetComplianceSummaryByConfigRule' with the minimum fields required to make a request.
--
getComplianceSummaryByConfigRule
    :: GetComplianceSummaryByConfigRule
getComplianceSummaryByConfigRule = GetComplianceSummaryByConfigRule'


instance AWSRequest GetComplianceSummaryByConfigRule
         where
        type Rs GetComplianceSummaryByConfigRule =
             GetComplianceSummaryByConfigRuleResponse
        request = postJSON config
        response
          = receiveJSON
              (\ s h x ->
                 GetComplianceSummaryByConfigRuleResponse' <$>
                   (x .?> "ComplianceSummary") <*> (pure (fromEnum s)))

instance Hashable GetComplianceSummaryByConfigRule
         where

instance NFData GetComplianceSummaryByConfigRule
         where

instance ToHeaders GetComplianceSummaryByConfigRule
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StarlingDoveService.GetComplianceSummaryByConfigRule"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetComplianceSummaryByConfigRule
         where
        toJSON = const (Object mempty)

instance ToPath GetComplianceSummaryByConfigRule
         where
        toPath = const "/"

instance ToQuery GetComplianceSummaryByConfigRule
         where
        toQuery = const mempty

-- |
--
--
--
-- /See:/ 'getComplianceSummaryByConfigRuleResponse' smart constructor.
data GetComplianceSummaryByConfigRuleResponse = GetComplianceSummaryByConfigRuleResponse'
  { _gcsbcrrsComplianceSummary :: !(Maybe ComplianceSummary)
  , _gcsbcrrsResponseStatus    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetComplianceSummaryByConfigRuleResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcsbcrrsComplianceSummary' - The number of AWS Config rules that are compliant and the number that are noncompliant, up to a maximum of 25 for each.
--
-- * 'gcsbcrrsResponseStatus' - -- | The response status code.
getComplianceSummaryByConfigRuleResponse
    :: Int -- ^ 'gcsbcrrsResponseStatus'
    -> GetComplianceSummaryByConfigRuleResponse
getComplianceSummaryByConfigRuleResponse pResponseStatus_ =
  GetComplianceSummaryByConfigRuleResponse'
    { _gcsbcrrsComplianceSummary = Nothing
    , _gcsbcrrsResponseStatus = pResponseStatus_
    }


-- | The number of AWS Config rules that are compliant and the number that are noncompliant, up to a maximum of 25 for each.
gcsbcrrsComplianceSummary :: Lens' GetComplianceSummaryByConfigRuleResponse (Maybe ComplianceSummary)
gcsbcrrsComplianceSummary = lens _gcsbcrrsComplianceSummary (\ s a -> s{_gcsbcrrsComplianceSummary = a})

-- | -- | The response status code.
gcsbcrrsResponseStatus :: Lens' GetComplianceSummaryByConfigRuleResponse Int
gcsbcrrsResponseStatus = lens _gcsbcrrsResponseStatus (\ s a -> s{_gcsbcrrsResponseStatus = a})

instance NFData
           GetComplianceSummaryByConfigRuleResponse
         where
