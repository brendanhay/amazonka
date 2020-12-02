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
-- Module      : Network.AWS.CloudWatch.DeleteInsightRules
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently deletes the specified Contributor Insights rules.
--
--
-- If you create a rule, delete it, and then re-create it with the same name, historical data from the first time the rule was created might not be available.
module Network.AWS.CloudWatch.DeleteInsightRules
  ( -- * Creating a Request
    deleteInsightRules,
    DeleteInsightRules,

    -- * Request Lenses
    dRuleNames,

    -- * Destructuring the Response
    deleteInsightRulesResponse,
    DeleteInsightRulesResponse,

    -- * Response Lenses
    dirsrsFailures,
    dirsrsResponseStatus,
  )
where

import Network.AWS.CloudWatch.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteInsightRules' smart constructor.
newtype DeleteInsightRules = DeleteInsightRules'
  { _dRuleNames ::
      [Text]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteInsightRules' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dRuleNames' - An array of the rule names to delete. If you need to find out the names of your rules, use <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_DescribeInsightRules.html DescribeInsightRules> .
deleteInsightRules ::
  DeleteInsightRules
deleteInsightRules = DeleteInsightRules' {_dRuleNames = mempty}

-- | An array of the rule names to delete. If you need to find out the names of your rules, use <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_DescribeInsightRules.html DescribeInsightRules> .
dRuleNames :: Lens' DeleteInsightRules [Text]
dRuleNames = lens _dRuleNames (\s a -> s {_dRuleNames = a}) . _Coerce

instance AWSRequest DeleteInsightRules where
  type Rs DeleteInsightRules = DeleteInsightRulesResponse
  request = postQuery cloudWatch
  response =
    receiveXMLWrapper
      "DeleteInsightRulesResult"
      ( \s h x ->
          DeleteInsightRulesResponse'
            <$> (x .@? "Failures" .!@ mempty >>= may (parseXMLList "member"))
            <*> (pure (fromEnum s))
      )

instance Hashable DeleteInsightRules

instance NFData DeleteInsightRules

instance ToHeaders DeleteInsightRules where
  toHeaders = const mempty

instance ToPath DeleteInsightRules where
  toPath = const "/"

instance ToQuery DeleteInsightRules where
  toQuery DeleteInsightRules' {..} =
    mconcat
      [ "Action" =: ("DeleteInsightRules" :: ByteString),
        "Version" =: ("2010-08-01" :: ByteString),
        "RuleNames" =: toQueryList "member" _dRuleNames
      ]

-- | /See:/ 'deleteInsightRulesResponse' smart constructor.
data DeleteInsightRulesResponse = DeleteInsightRulesResponse'
  { _dirsrsFailures ::
      !(Maybe [PartialFailure]),
    _dirsrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteInsightRulesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dirsrsFailures' - An array listing the rules that could not be deleted. You cannot delete built-in rules.
--
-- * 'dirsrsResponseStatus' - -- | The response status code.
deleteInsightRulesResponse ::
  -- | 'dirsrsResponseStatus'
  Int ->
  DeleteInsightRulesResponse
deleteInsightRulesResponse pResponseStatus_ =
  DeleteInsightRulesResponse'
    { _dirsrsFailures = Nothing,
      _dirsrsResponseStatus = pResponseStatus_
    }

-- | An array listing the rules that could not be deleted. You cannot delete built-in rules.
dirsrsFailures :: Lens' DeleteInsightRulesResponse [PartialFailure]
dirsrsFailures = lens _dirsrsFailures (\s a -> s {_dirsrsFailures = a}) . _Default . _Coerce

-- | -- | The response status code.
dirsrsResponseStatus :: Lens' DeleteInsightRulesResponse Int
dirsrsResponseStatus = lens _dirsrsResponseStatus (\s a -> s {_dirsrsResponseStatus = a})

instance NFData DeleteInsightRulesResponse
