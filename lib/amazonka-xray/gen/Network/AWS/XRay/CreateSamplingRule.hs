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
-- Module      : Network.AWS.XRay.CreateSamplingRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a rule to control sampling behavior for instrumented applications. Services retrieve rules with 'GetSamplingRules' , and evaluate each rule in ascending order of /priority/ for each request. If a rule matches, the service records a trace, borrowing it from the reservoir size. After 10 seconds, the service reports back to X-Ray with 'GetSamplingTargets' to get updated versions of each in-use rule. The updated rule contains a trace quota that the service can use instead of borrowing from the reservoir.
module Network.AWS.XRay.CreateSamplingRule
  ( -- * Creating a Request
    createSamplingRule,
    CreateSamplingRule,

    -- * Request Lenses
    csrTags,
    csrSamplingRule,

    -- * Destructuring the Response
    createSamplingRuleResponse,
    CreateSamplingRuleResponse,

    -- * Response Lenses
    csrrsSamplingRuleRecord,
    csrrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.XRay.Types

-- | /See:/ 'createSamplingRule' smart constructor.
data CreateSamplingRule = CreateSamplingRule'
  { _csrTags ::
      !(Maybe [Tag]),
    _csrSamplingRule :: !SamplingRule
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateSamplingRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csrTags' - A map that contains one or more tag keys and tag values to attach to an X-Ray sampling rule. For more information about ways to use tags, see <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS resources> in the /AWS General Reference/ . The following restrictions apply to tags:     * Maximum number of user-applied tags per resource: 50     * Maximum tag key length: 128 Unicode characters     * Maximum tag value length: 256 Unicode characters     * Valid values for key and value: a-z, A-Z, 0-9, space, and the following characters: _ . : / = + - and @     * Tag keys and values are case sensitive.     * Don't use @aws:@ as a prefix for keys; it's reserved for AWS use.
--
-- * 'csrSamplingRule' - The rule definition.
createSamplingRule ::
  -- | 'csrSamplingRule'
  SamplingRule ->
  CreateSamplingRule
createSamplingRule pSamplingRule_ =
  CreateSamplingRule'
    { _csrTags = Nothing,
      _csrSamplingRule = pSamplingRule_
    }

-- | A map that contains one or more tag keys and tag values to attach to an X-Ray sampling rule. For more information about ways to use tags, see <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS resources> in the /AWS General Reference/ . The following restrictions apply to tags:     * Maximum number of user-applied tags per resource: 50     * Maximum tag key length: 128 Unicode characters     * Maximum tag value length: 256 Unicode characters     * Valid values for key and value: a-z, A-Z, 0-9, space, and the following characters: _ . : / = + - and @     * Tag keys and values are case sensitive.     * Don't use @aws:@ as a prefix for keys; it's reserved for AWS use.
csrTags :: Lens' CreateSamplingRule [Tag]
csrTags = lens _csrTags (\s a -> s {_csrTags = a}) . _Default . _Coerce

-- | The rule definition.
csrSamplingRule :: Lens' CreateSamplingRule SamplingRule
csrSamplingRule = lens _csrSamplingRule (\s a -> s {_csrSamplingRule = a})

instance AWSRequest CreateSamplingRule where
  type Rs CreateSamplingRule = CreateSamplingRuleResponse
  request = postJSON xRay
  response =
    receiveJSON
      ( \s h x ->
          CreateSamplingRuleResponse'
            <$> (x .?> "SamplingRuleRecord") <*> (pure (fromEnum s))
      )

instance Hashable CreateSamplingRule

instance NFData CreateSamplingRule

instance ToHeaders CreateSamplingRule where
  toHeaders = const mempty

instance ToJSON CreateSamplingRule where
  toJSON CreateSamplingRule' {..} =
    object
      ( catMaybes
          [ ("Tags" .=) <$> _csrTags,
            Just ("SamplingRule" .= _csrSamplingRule)
          ]
      )

instance ToPath CreateSamplingRule where
  toPath = const "/CreateSamplingRule"

instance ToQuery CreateSamplingRule where
  toQuery = const mempty

-- | /See:/ 'createSamplingRuleResponse' smart constructor.
data CreateSamplingRuleResponse = CreateSamplingRuleResponse'
  { _csrrsSamplingRuleRecord ::
      !(Maybe SamplingRuleRecord),
    _csrrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateSamplingRuleResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csrrsSamplingRuleRecord' - The saved rule definition and metadata.
--
-- * 'csrrsResponseStatus' - -- | The response status code.
createSamplingRuleResponse ::
  -- | 'csrrsResponseStatus'
  Int ->
  CreateSamplingRuleResponse
createSamplingRuleResponse pResponseStatus_ =
  CreateSamplingRuleResponse'
    { _csrrsSamplingRuleRecord = Nothing,
      _csrrsResponseStatus = pResponseStatus_
    }

-- | The saved rule definition and metadata.
csrrsSamplingRuleRecord :: Lens' CreateSamplingRuleResponse (Maybe SamplingRuleRecord)
csrrsSamplingRuleRecord = lens _csrrsSamplingRuleRecord (\s a -> s {_csrrsSamplingRuleRecord = a})

-- | -- | The response status code.
csrrsResponseStatus :: Lens' CreateSamplingRuleResponse Int
csrrsResponseStatus = lens _csrrsResponseStatus (\s a -> s {_csrrsResponseStatus = a})

instance NFData CreateSamplingRuleResponse
