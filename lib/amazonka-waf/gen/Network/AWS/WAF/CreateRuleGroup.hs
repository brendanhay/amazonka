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
-- Module      : Network.AWS.WAF.CreateRuleGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a @RuleGroup@ . A rule group is a collection of predefined rules that you add to a web ACL. You use 'UpdateRuleGroup' to add rules to the rule group.
--
--
-- Rule groups are subject to the following limits:
--
--     * Three rule groups per account. You can request an increase to this limit by contacting customer support.
--
--     * One rule group per web ACL.
--
--     * Ten rules per rule group.
--
--
--
-- For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <https://docs.aws.amazon.com/waf/latest/developerguide/ AWS WAF Developer Guide> .
module Network.AWS.WAF.CreateRuleGroup
  ( -- * Creating a Request
    createRuleGroup,
    CreateRuleGroup,

    -- * Request Lenses
    crgTags,
    crgName,
    crgMetricName,
    crgChangeToken,

    -- * Destructuring the Response
    createRuleGroupResponse,
    CreateRuleGroupResponse,

    -- * Response Lenses
    crgrsChangeToken,
    crgrsRuleGroup,
    crgrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WAF.Types

-- | /See:/ 'createRuleGroup' smart constructor.
data CreateRuleGroup = CreateRuleGroup'
  { _crgTags ::
      !(Maybe (List1 Tag)),
    _crgName :: !Text,
    _crgMetricName :: !Text,
    _crgChangeToken :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateRuleGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crgTags' -
--
-- * 'crgName' - A friendly name or description of the 'RuleGroup' . You can't change @Name@ after you create a @RuleGroup@ .
--
-- * 'crgMetricName' - A friendly name or description for the metrics for this @RuleGroup@ . The name can contain only alphanumeric characters (A-Z, a-z, 0-9), with maximum length 128 and minimum length one. It can't contain whitespace or metric names reserved for AWS WAF, including "All" and "Default_Action." You can't change the name of the metric after you create the @RuleGroup@ .
--
-- * 'crgChangeToken' - The value returned by the most recent call to 'GetChangeToken' .
createRuleGroup ::
  -- | 'crgName'
  Text ->
  -- | 'crgMetricName'
  Text ->
  -- | 'crgChangeToken'
  Text ->
  CreateRuleGroup
createRuleGroup pName_ pMetricName_ pChangeToken_ =
  CreateRuleGroup'
    { _crgTags = Nothing,
      _crgName = pName_,
      _crgMetricName = pMetricName_,
      _crgChangeToken = pChangeToken_
    }

-- |
crgTags :: Lens' CreateRuleGroup (Maybe (NonEmpty Tag))
crgTags = lens _crgTags (\s a -> s {_crgTags = a}) . mapping _List1

-- | A friendly name or description of the 'RuleGroup' . You can't change @Name@ after you create a @RuleGroup@ .
crgName :: Lens' CreateRuleGroup Text
crgName = lens _crgName (\s a -> s {_crgName = a})

-- | A friendly name or description for the metrics for this @RuleGroup@ . The name can contain only alphanumeric characters (A-Z, a-z, 0-9), with maximum length 128 and minimum length one. It can't contain whitespace or metric names reserved for AWS WAF, including "All" and "Default_Action." You can't change the name of the metric after you create the @RuleGroup@ .
crgMetricName :: Lens' CreateRuleGroup Text
crgMetricName = lens _crgMetricName (\s a -> s {_crgMetricName = a})

-- | The value returned by the most recent call to 'GetChangeToken' .
crgChangeToken :: Lens' CreateRuleGroup Text
crgChangeToken = lens _crgChangeToken (\s a -> s {_crgChangeToken = a})

instance AWSRequest CreateRuleGroup where
  type Rs CreateRuleGroup = CreateRuleGroupResponse
  request = postJSON waf
  response =
    receiveJSON
      ( \s h x ->
          CreateRuleGroupResponse'
            <$> (x .?> "ChangeToken")
            <*> (x .?> "RuleGroup")
            <*> (pure (fromEnum s))
      )

instance Hashable CreateRuleGroup

instance NFData CreateRuleGroup

instance ToHeaders CreateRuleGroup where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AWSWAF_20150824.CreateRuleGroup" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateRuleGroup where
  toJSON CreateRuleGroup' {..} =
    object
      ( catMaybes
          [ ("Tags" .=) <$> _crgTags,
            Just ("Name" .= _crgName),
            Just ("MetricName" .= _crgMetricName),
            Just ("ChangeToken" .= _crgChangeToken)
          ]
      )

instance ToPath CreateRuleGroup where
  toPath = const "/"

instance ToQuery CreateRuleGroup where
  toQuery = const mempty

-- | /See:/ 'createRuleGroupResponse' smart constructor.
data CreateRuleGroupResponse = CreateRuleGroupResponse'
  { _crgrsChangeToken ::
      !(Maybe Text),
    _crgrsRuleGroup :: !(Maybe RuleGroup),
    _crgrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateRuleGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crgrsChangeToken' - The @ChangeToken@ that you used to submit the @CreateRuleGroup@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- * 'crgrsRuleGroup' - An empty 'RuleGroup' .
--
-- * 'crgrsResponseStatus' - -- | The response status code.
createRuleGroupResponse ::
  -- | 'crgrsResponseStatus'
  Int ->
  CreateRuleGroupResponse
createRuleGroupResponse pResponseStatus_ =
  CreateRuleGroupResponse'
    { _crgrsChangeToken = Nothing,
      _crgrsRuleGroup = Nothing,
      _crgrsResponseStatus = pResponseStatus_
    }

-- | The @ChangeToken@ that you used to submit the @CreateRuleGroup@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
crgrsChangeToken :: Lens' CreateRuleGroupResponse (Maybe Text)
crgrsChangeToken = lens _crgrsChangeToken (\s a -> s {_crgrsChangeToken = a})

-- | An empty 'RuleGroup' .
crgrsRuleGroup :: Lens' CreateRuleGroupResponse (Maybe RuleGroup)
crgrsRuleGroup = lens _crgrsRuleGroup (\s a -> s {_crgrsRuleGroup = a})

-- | -- | The response status code.
crgrsResponseStatus :: Lens' CreateRuleGroupResponse Int
crgrsResponseStatus = lens _crgrsResponseStatus (\s a -> s {_crgrsResponseStatus = a})

instance NFData CreateRuleGroupResponse
