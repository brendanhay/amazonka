{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.Types.InsightRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatch.Types.InsightRule where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | This structure contains the definition for a Contributor Insights rule.
--
--
--
-- /See:/ 'insightRule' smart constructor.
data InsightRule = InsightRule'
  { _irName :: !Text,
    _irState :: !Text,
    _irSchema :: !Text,
    _irDefinition :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InsightRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'irName' - The name of the rule.
--
-- * 'irState' - Indicates whether the rule is enabled or disabled.
--
-- * 'irSchema' - For rules that you create, this is always @{"Name": "CloudWatchLogRule", "Version": 1}@ . For built-in rules, this is @{"Name": "ServiceLogRule", "Version": 1}@
--
-- * 'irDefinition' - The definition of the rule, as a JSON object. The definition contains the keywords used to define contributors, the value to aggregate on if this rule returns a sum instead of a count, and the filters. For details on the valid syntax, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/ContributorInsights-RuleSyntax.html Contributor Insights Rule Syntax> .
insightRule ::
  -- | 'irName'
  Text ->
  -- | 'irState'
  Text ->
  -- | 'irSchema'
  Text ->
  -- | 'irDefinition'
  Text ->
  InsightRule
insightRule pName_ pState_ pSchema_ pDefinition_ =
  InsightRule'
    { _irName = pName_,
      _irState = pState_,
      _irSchema = pSchema_,
      _irDefinition = pDefinition_
    }

-- | The name of the rule.
irName :: Lens' InsightRule Text
irName = lens _irName (\s a -> s {_irName = a})

-- | Indicates whether the rule is enabled or disabled.
irState :: Lens' InsightRule Text
irState = lens _irState (\s a -> s {_irState = a})

-- | For rules that you create, this is always @{"Name": "CloudWatchLogRule", "Version": 1}@ . For built-in rules, this is @{"Name": "ServiceLogRule", "Version": 1}@
irSchema :: Lens' InsightRule Text
irSchema = lens _irSchema (\s a -> s {_irSchema = a})

-- | The definition of the rule, as a JSON object. The definition contains the keywords used to define contributors, the value to aggregate on if this rule returns a sum instead of a count, and the filters. For details on the valid syntax, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/ContributorInsights-RuleSyntax.html Contributor Insights Rule Syntax> .
irDefinition :: Lens' InsightRule Text
irDefinition = lens _irDefinition (\s a -> s {_irDefinition = a})

instance FromXML InsightRule where
  parseXML x =
    InsightRule'
      <$> (x .@ "Name")
      <*> (x .@ "State")
      <*> (x .@ "Schema")
      <*> (x .@ "Definition")

instance Hashable InsightRule

instance NFData InsightRule
