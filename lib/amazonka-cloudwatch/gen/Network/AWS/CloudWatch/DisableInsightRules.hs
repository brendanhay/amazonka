{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.DisableInsightRules
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables the specified Contributor Insights rules. When rules are disabled, they do not analyze log groups and do not incur costs.
module Network.AWS.CloudWatch.DisableInsightRules
  ( -- * Creating a Request
    disableInsightRules,
    DisableInsightRules,

    -- * Request Lenses
    dirRuleNames,

    -- * Destructuring the Response
    disableInsightRulesResponse,
    DisableInsightRulesResponse,

    -- * Response Lenses
    dirrsFailures,
    dirrsResponseStatus,
  )
where

import Network.AWS.CloudWatch.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'disableInsightRules' smart constructor.
newtype DisableInsightRules = DisableInsightRules'
  { _dirRuleNames ::
      [Text]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DisableInsightRules' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dirRuleNames' - An array of the rule names to disable. If you need to find out the names of your rules, use <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_DescribeInsightRules.html DescribeInsightRules> .
disableInsightRules ::
  DisableInsightRules
disableInsightRules = DisableInsightRules' {_dirRuleNames = mempty}

-- | An array of the rule names to disable. If you need to find out the names of your rules, use <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_DescribeInsightRules.html DescribeInsightRules> .
dirRuleNames :: Lens' DisableInsightRules [Text]
dirRuleNames = lens _dirRuleNames (\s a -> s {_dirRuleNames = a}) . _Coerce

instance AWSRequest DisableInsightRules where
  type Rs DisableInsightRules = DisableInsightRulesResponse
  request = postQuery cloudWatch
  response =
    receiveXMLWrapper
      "DisableInsightRulesResult"
      ( \s h x ->
          DisableInsightRulesResponse'
            <$> (x .@? "Failures" .!@ mempty >>= may (parseXMLList "member"))
            <*> (pure (fromEnum s))
      )

instance Hashable DisableInsightRules

instance NFData DisableInsightRules

instance ToHeaders DisableInsightRules where
  toHeaders = const mempty

instance ToPath DisableInsightRules where
  toPath = const "/"

instance ToQuery DisableInsightRules where
  toQuery DisableInsightRules' {..} =
    mconcat
      [ "Action" =: ("DisableInsightRules" :: ByteString),
        "Version" =: ("2010-08-01" :: ByteString),
        "RuleNames" =: toQueryList "member" _dirRuleNames
      ]

-- | /See:/ 'disableInsightRulesResponse' smart constructor.
data DisableInsightRulesResponse = DisableInsightRulesResponse'
  { _dirrsFailures ::
      !(Maybe [PartialFailure]),
    _dirrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DisableInsightRulesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dirrsFailures' - An array listing the rules that could not be disabled. You cannot disable built-in rules.
--
-- * 'dirrsResponseStatus' - -- | The response status code.
disableInsightRulesResponse ::
  -- | 'dirrsResponseStatus'
  Int ->
  DisableInsightRulesResponse
disableInsightRulesResponse pResponseStatus_ =
  DisableInsightRulesResponse'
    { _dirrsFailures = Nothing,
      _dirrsResponseStatus = pResponseStatus_
    }

-- | An array listing the rules that could not be disabled. You cannot disable built-in rules.
dirrsFailures :: Lens' DisableInsightRulesResponse [PartialFailure]
dirrsFailures = lens _dirrsFailures (\s a -> s {_dirrsFailures = a}) . _Default . _Coerce

-- | -- | The response status code.
dirrsResponseStatus :: Lens' DisableInsightRulesResponse Int
dirrsResponseStatus = lens _dirrsResponseStatus (\s a -> s {_dirrsResponseStatus = a})

instance NFData DisableInsightRulesResponse
