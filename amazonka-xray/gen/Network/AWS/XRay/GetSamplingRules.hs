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
-- Module      : Network.AWS.XRay.GetSamplingRules
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves all sampling rules.
--
--
--
-- This operation returns paginated results.
module Network.AWS.XRay.GetSamplingRules
    (
    -- * Creating a Request
      getSamplingRules
    , GetSamplingRules
    -- * Request Lenses
    , gsrNextToken

    -- * Destructuring the Response
    , getSamplingRulesResponse
    , GetSamplingRulesResponse
    -- * Response Lenses
    , gsrrsSamplingRuleRecords
    , gsrrsNextToken
    , gsrrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.XRay.Types
import Network.AWS.XRay.Types.Product

-- | /See:/ 'getSamplingRules' smart constructor.
newtype GetSamplingRules = GetSamplingRules'
  { _gsrNextToken :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetSamplingRules' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsrNextToken' - Pagination token. Not used.
getSamplingRules
    :: GetSamplingRules
getSamplingRules = GetSamplingRules' {_gsrNextToken = Nothing}


-- | Pagination token. Not used.
gsrNextToken :: Lens' GetSamplingRules (Maybe Text)
gsrNextToken = lens _gsrNextToken (\ s a -> s{_gsrNextToken = a})

instance AWSPager GetSamplingRules where
        page rq rs
          | stop (rs ^. gsrrsNextToken) = Nothing
          | stop (rs ^. gsrrsSamplingRuleRecords) = Nothing
          | otherwise =
            Just $ rq & gsrNextToken .~ rs ^. gsrrsNextToken

instance AWSRequest GetSamplingRules where
        type Rs GetSamplingRules = GetSamplingRulesResponse
        request = postJSON xRay
        response
          = receiveJSON
              (\ s h x ->
                 GetSamplingRulesResponse' <$>
                   (x .?> "SamplingRuleRecords" .!@ mempty) <*>
                     (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable GetSamplingRules where

instance NFData GetSamplingRules where

instance ToHeaders GetSamplingRules where
        toHeaders = const mempty

instance ToJSON GetSamplingRules where
        toJSON GetSamplingRules'{..}
          = object
              (catMaybes [("NextToken" .=) <$> _gsrNextToken])

instance ToPath GetSamplingRules where
        toPath = const "/GetSamplingRules"

instance ToQuery GetSamplingRules where
        toQuery = const mempty

-- | /See:/ 'getSamplingRulesResponse' smart constructor.
data GetSamplingRulesResponse = GetSamplingRulesResponse'
  { _gsrrsSamplingRuleRecords :: !(Maybe [SamplingRuleRecord])
  , _gsrrsNextToken :: !(Maybe Text)
  , _gsrrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetSamplingRulesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsrrsSamplingRuleRecords' - Rule definitions and metadata.
--
-- * 'gsrrsNextToken' - Pagination token. Not used.
--
-- * 'gsrrsResponseStatus' - -- | The response status code.
getSamplingRulesResponse
    :: Int -- ^ 'gsrrsResponseStatus'
    -> GetSamplingRulesResponse
getSamplingRulesResponse pResponseStatus_ =
  GetSamplingRulesResponse'
    { _gsrrsSamplingRuleRecords = Nothing
    , _gsrrsNextToken = Nothing
    , _gsrrsResponseStatus = pResponseStatus_
    }


-- | Rule definitions and metadata.
gsrrsSamplingRuleRecords :: Lens' GetSamplingRulesResponse [SamplingRuleRecord]
gsrrsSamplingRuleRecords = lens _gsrrsSamplingRuleRecords (\ s a -> s{_gsrrsSamplingRuleRecords = a}) . _Default . _Coerce

-- | Pagination token. Not used.
gsrrsNextToken :: Lens' GetSamplingRulesResponse (Maybe Text)
gsrrsNextToken = lens _gsrrsNextToken (\ s a -> s{_gsrrsNextToken = a})

-- | -- | The response status code.
gsrrsResponseStatus :: Lens' GetSamplingRulesResponse Int
gsrrsResponseStatus = lens _gsrrsResponseStatus (\ s a -> s{_gsrrsResponseStatus = a})

instance NFData GetSamplingRulesResponse where
