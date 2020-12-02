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
-- Module      : Network.AWS.Config.DescribeConfigRules
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns details about your AWS Config rules.
--
--
--
-- This operation returns paginated results.
module Network.AWS.Config.DescribeConfigRules
    (
    -- * Creating a Request
      describeConfigRules
    , DescribeConfigRules
    -- * Request Lenses
    , dcrConfigRuleNames
    , dcrNextToken

    -- * Destructuring the Response
    , describeConfigRulesResponse
    , DescribeConfigRulesResponse
    -- * Response Lenses
    , dcrrsConfigRules
    , dcrrsNextToken
    , dcrrsResponseStatus
    ) where

import Network.AWS.Config.Types
import Network.AWS.Config.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'describeConfigRules' smart constructor.
data DescribeConfigRules = DescribeConfigRules'
  { _dcrConfigRuleNames :: !(Maybe [Text])
  , _dcrNextToken       :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeConfigRules' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcrConfigRuleNames' - The names of the AWS Config rules for which you want details. If you do not specify any names, AWS Config returns details for all your rules.
--
-- * 'dcrNextToken' - The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
describeConfigRules
    :: DescribeConfigRules
describeConfigRules =
  DescribeConfigRules' {_dcrConfigRuleNames = Nothing, _dcrNextToken = Nothing}


-- | The names of the AWS Config rules for which you want details. If you do not specify any names, AWS Config returns details for all your rules.
dcrConfigRuleNames :: Lens' DescribeConfigRules [Text]
dcrConfigRuleNames = lens _dcrConfigRuleNames (\ s a -> s{_dcrConfigRuleNames = a}) . _Default . _Coerce

-- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
dcrNextToken :: Lens' DescribeConfigRules (Maybe Text)
dcrNextToken = lens _dcrNextToken (\ s a -> s{_dcrNextToken = a})

instance AWSPager DescribeConfigRules where
        page rq rs
          | stop (rs ^. dcrrsNextToken) = Nothing
          | stop (rs ^. dcrrsConfigRules) = Nothing
          | otherwise =
            Just $ rq & dcrNextToken .~ rs ^. dcrrsNextToken

instance AWSRequest DescribeConfigRules where
        type Rs DescribeConfigRules =
             DescribeConfigRulesResponse
        request = postJSON config
        response
          = receiveJSON
              (\ s h x ->
                 DescribeConfigRulesResponse' <$>
                   (x .?> "ConfigRules" .!@ mempty) <*>
                     (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeConfigRules where

instance NFData DescribeConfigRules where

instance ToHeaders DescribeConfigRules where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StarlingDoveService.DescribeConfigRules" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeConfigRules where
        toJSON DescribeConfigRules'{..}
          = object
              (catMaybes
                 [("ConfigRuleNames" .=) <$> _dcrConfigRuleNames,
                  ("NextToken" .=) <$> _dcrNextToken])

instance ToPath DescribeConfigRules where
        toPath = const "/"

instance ToQuery DescribeConfigRules where
        toQuery = const mempty

-- |
--
--
--
-- /See:/ 'describeConfigRulesResponse' smart constructor.
data DescribeConfigRulesResponse = DescribeConfigRulesResponse'
  { _dcrrsConfigRules    :: !(Maybe [ConfigRule])
  , _dcrrsNextToken      :: !(Maybe Text)
  , _dcrrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeConfigRulesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcrrsConfigRules' - The details about your AWS Config rules.
--
-- * 'dcrrsNextToken' - The string that you use in a subsequent request to get the next page of results in a paginated response.
--
-- * 'dcrrsResponseStatus' - -- | The response status code.
describeConfigRulesResponse
    :: Int -- ^ 'dcrrsResponseStatus'
    -> DescribeConfigRulesResponse
describeConfigRulesResponse pResponseStatus_ =
  DescribeConfigRulesResponse'
    { _dcrrsConfigRules = Nothing
    , _dcrrsNextToken = Nothing
    , _dcrrsResponseStatus = pResponseStatus_
    }


-- | The details about your AWS Config rules.
dcrrsConfigRules :: Lens' DescribeConfigRulesResponse [ConfigRule]
dcrrsConfigRules = lens _dcrrsConfigRules (\ s a -> s{_dcrrsConfigRules = a}) . _Default . _Coerce

-- | The string that you use in a subsequent request to get the next page of results in a paginated response.
dcrrsNextToken :: Lens' DescribeConfigRulesResponse (Maybe Text)
dcrrsNextToken = lens _dcrrsNextToken (\ s a -> s{_dcrrsNextToken = a})

-- | -- | The response status code.
dcrrsResponseStatus :: Lens' DescribeConfigRulesResponse Int
dcrrsResponseStatus = lens _dcrrsResponseStatus (\ s a -> s{_dcrrsResponseStatus = a})

instance NFData DescribeConfigRulesResponse where
