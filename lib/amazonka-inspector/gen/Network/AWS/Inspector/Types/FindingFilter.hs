{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.FindingFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.FindingFilter where

import Network.AWS.Inspector.Types.Attribute
import Network.AWS.Inspector.Types.Severity
import Network.AWS.Inspector.Types.TimestampRange
import Network.AWS.Lens
import Network.AWS.Prelude

-- | This data type is used as a request parameter in the 'ListFindings' action.
--
--
--
-- /See:/ 'findingFilter' smart constructor.
data FindingFilter = FindingFilter'
  { _ffAgentIds :: !(Maybe [Text]),
    _ffRuleNames :: !(Maybe [Text]),
    _ffUserAttributes :: !(Maybe [Attribute]),
    _ffRulesPackageARNs :: !(Maybe [Text]),
    _ffAttributes :: !(Maybe [Attribute]),
    _ffSeverities :: !(Maybe [Severity]),
    _ffCreationTimeRange :: !(Maybe TimestampRange),
    _ffAutoScalingGroups :: !(Maybe [Text])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FindingFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ffAgentIds' - For a record to match a filter, one of the values that is specified for this data type property must be the exact match of the value of the __agentId__ property of the 'Finding' data type.
--
-- * 'ffRuleNames' - For a record to match a filter, one of the values that is specified for this data type property must be the exact match of the value of the __ruleName__ property of the 'Finding' data type.
--
-- * 'ffUserAttributes' - For a record to match a filter, the value that is specified for this data type property must be contained in the list of values of the __userAttributes__ property of the 'Finding' data type.
--
-- * 'ffRulesPackageARNs' - For a record to match a filter, one of the values that is specified for this data type property must be the exact match of the value of the __rulesPackageArn__ property of the 'Finding' data type.
--
-- * 'ffAttributes' - For a record to match a filter, the list of values that are specified for this data type property must be contained in the list of values of the __attributes__ property of the 'Finding' data type.
--
-- * 'ffSeverities' - For a record to match a filter, one of the values that is specified for this data type property must be the exact match of the value of the __severity__ property of the 'Finding' data type.
--
-- * 'ffCreationTimeRange' - The time range during which the finding is generated.
--
-- * 'ffAutoScalingGroups' - For a record to match a filter, one of the values that is specified for this data type property must be the exact match of the value of the __autoScalingGroup__ property of the 'Finding' data type.
findingFilter ::
  FindingFilter
findingFilter =
  FindingFilter'
    { _ffAgentIds = Nothing,
      _ffRuleNames = Nothing,
      _ffUserAttributes = Nothing,
      _ffRulesPackageARNs = Nothing,
      _ffAttributes = Nothing,
      _ffSeverities = Nothing,
      _ffCreationTimeRange = Nothing,
      _ffAutoScalingGroups = Nothing
    }

-- | For a record to match a filter, one of the values that is specified for this data type property must be the exact match of the value of the __agentId__ property of the 'Finding' data type.
ffAgentIds :: Lens' FindingFilter [Text]
ffAgentIds = lens _ffAgentIds (\s a -> s {_ffAgentIds = a}) . _Default . _Coerce

-- | For a record to match a filter, one of the values that is specified for this data type property must be the exact match of the value of the __ruleName__ property of the 'Finding' data type.
ffRuleNames :: Lens' FindingFilter [Text]
ffRuleNames = lens _ffRuleNames (\s a -> s {_ffRuleNames = a}) . _Default . _Coerce

-- | For a record to match a filter, the value that is specified for this data type property must be contained in the list of values of the __userAttributes__ property of the 'Finding' data type.
ffUserAttributes :: Lens' FindingFilter [Attribute]
ffUserAttributes = lens _ffUserAttributes (\s a -> s {_ffUserAttributes = a}) . _Default . _Coerce

-- | For a record to match a filter, one of the values that is specified for this data type property must be the exact match of the value of the __rulesPackageArn__ property of the 'Finding' data type.
ffRulesPackageARNs :: Lens' FindingFilter [Text]
ffRulesPackageARNs = lens _ffRulesPackageARNs (\s a -> s {_ffRulesPackageARNs = a}) . _Default . _Coerce

-- | For a record to match a filter, the list of values that are specified for this data type property must be contained in the list of values of the __attributes__ property of the 'Finding' data type.
ffAttributes :: Lens' FindingFilter [Attribute]
ffAttributes = lens _ffAttributes (\s a -> s {_ffAttributes = a}) . _Default . _Coerce

-- | For a record to match a filter, one of the values that is specified for this data type property must be the exact match of the value of the __severity__ property of the 'Finding' data type.
ffSeverities :: Lens' FindingFilter [Severity]
ffSeverities = lens _ffSeverities (\s a -> s {_ffSeverities = a}) . _Default . _Coerce

-- | The time range during which the finding is generated.
ffCreationTimeRange :: Lens' FindingFilter (Maybe TimestampRange)
ffCreationTimeRange = lens _ffCreationTimeRange (\s a -> s {_ffCreationTimeRange = a})

-- | For a record to match a filter, one of the values that is specified for this data type property must be the exact match of the value of the __autoScalingGroup__ property of the 'Finding' data type.
ffAutoScalingGroups :: Lens' FindingFilter [Text]
ffAutoScalingGroups = lens _ffAutoScalingGroups (\s a -> s {_ffAutoScalingGroups = a}) . _Default . _Coerce

instance Hashable FindingFilter

instance NFData FindingFilter

instance ToJSON FindingFilter where
  toJSON FindingFilter' {..} =
    object
      ( catMaybes
          [ ("agentIds" .=) <$> _ffAgentIds,
            ("ruleNames" .=) <$> _ffRuleNames,
            ("userAttributes" .=) <$> _ffUserAttributes,
            ("rulesPackageArns" .=) <$> _ffRulesPackageARNs,
            ("attributes" .=) <$> _ffAttributes,
            ("severities" .=) <$> _ffSeverities,
            ("creationTimeRange" .=) <$> _ffCreationTimeRange,
            ("autoScalingGroups" .=) <$> _ffAutoScalingGroups
          ]
      )
