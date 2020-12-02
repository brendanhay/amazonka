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
-- Module      : Network.AWS.CloudWatch.EnableInsightRules
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the specified Contributor Insights rules. When rules are enabled, they immediately begin analyzing log data.
module Network.AWS.CloudWatch.EnableInsightRules
  ( -- * Creating a Request
    enableInsightRules,
    EnableInsightRules,

    -- * Request Lenses
    eirRuleNames,

    -- * Destructuring the Response
    enableInsightRulesResponse,
    EnableInsightRulesResponse,

    -- * Response Lenses
    eirrsFailures,
    eirrsResponseStatus,
  )
where

import Network.AWS.CloudWatch.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'enableInsightRules' smart constructor.
newtype EnableInsightRules = EnableInsightRules'
  { _eirRuleNames ::
      [Text]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EnableInsightRules' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eirRuleNames' - An array of the rule names to enable. If you need to find out the names of your rules, use <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_DescribeInsightRules.html DescribeInsightRules> .
enableInsightRules ::
  EnableInsightRules
enableInsightRules = EnableInsightRules' {_eirRuleNames = mempty}

-- | An array of the rule names to enable. If you need to find out the names of your rules, use <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_DescribeInsightRules.html DescribeInsightRules> .
eirRuleNames :: Lens' EnableInsightRules [Text]
eirRuleNames = lens _eirRuleNames (\s a -> s {_eirRuleNames = a}) . _Coerce

instance AWSRequest EnableInsightRules where
  type Rs EnableInsightRules = EnableInsightRulesResponse
  request = postQuery cloudWatch
  response =
    receiveXMLWrapper
      "EnableInsightRulesResult"
      ( \s h x ->
          EnableInsightRulesResponse'
            <$> (x .@? "Failures" .!@ mempty >>= may (parseXMLList "member"))
            <*> (pure (fromEnum s))
      )

instance Hashable EnableInsightRules

instance NFData EnableInsightRules

instance ToHeaders EnableInsightRules where
  toHeaders = const mempty

instance ToPath EnableInsightRules where
  toPath = const "/"

instance ToQuery EnableInsightRules where
  toQuery EnableInsightRules' {..} =
    mconcat
      [ "Action" =: ("EnableInsightRules" :: ByteString),
        "Version" =: ("2010-08-01" :: ByteString),
        "RuleNames" =: toQueryList "member" _eirRuleNames
      ]

-- | /See:/ 'enableInsightRulesResponse' smart constructor.
data EnableInsightRulesResponse = EnableInsightRulesResponse'
  { _eirrsFailures ::
      !(Maybe [PartialFailure]),
    _eirrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EnableInsightRulesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eirrsFailures' - An array listing the rules that could not be enabled. You cannot disable or enable built-in rules.
--
-- * 'eirrsResponseStatus' - -- | The response status code.
enableInsightRulesResponse ::
  -- | 'eirrsResponseStatus'
  Int ->
  EnableInsightRulesResponse
enableInsightRulesResponse pResponseStatus_ =
  EnableInsightRulesResponse'
    { _eirrsFailures = Nothing,
      _eirrsResponseStatus = pResponseStatus_
    }

-- | An array listing the rules that could not be enabled. You cannot disable or enable built-in rules.
eirrsFailures :: Lens' EnableInsightRulesResponse [PartialFailure]
eirrsFailures = lens _eirrsFailures (\s a -> s {_eirrsFailures = a}) . _Default . _Coerce

-- | -- | The response status code.
eirrsResponseStatus :: Lens' EnableInsightRulesResponse Int
eirrsResponseStatus = lens _eirrsResponseStatus (\s a -> s {_eirrsResponseStatus = a})

instance NFData EnableInsightRulesResponse
