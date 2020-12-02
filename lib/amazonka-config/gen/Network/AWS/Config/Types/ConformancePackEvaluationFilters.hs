{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ConformancePackEvaluationFilters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ConformancePackEvaluationFilters where

import Network.AWS.Config.Types.ConformancePackComplianceType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Filters a conformance pack by AWS Config rule names, compliance types, AWS resource types, and resource IDs.
--
--
--
-- /See:/ 'conformancePackEvaluationFilters' smart constructor.
data ConformancePackEvaluationFilters = ConformancePackEvaluationFilters'
  { _cpefResourceIds ::
      !(Maybe [Text]),
    _cpefResourceType ::
      !(Maybe Text),
    _cpefConfigRuleNames ::
      !(Maybe [Text]),
    _cpefComplianceType ::
      !( Maybe
           ConformancePackComplianceType
       )
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ConformancePackEvaluationFilters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpefResourceIds' - Filters the results by resource IDs.
--
-- * 'cpefResourceType' - Filters the results by the resource type (for example, @"AWS::EC2::Instance"@ ).
--
-- * 'cpefConfigRuleNames' - Filters the results by AWS Config rule names.
--
-- * 'cpefComplianceType' - Filters the results by compliance. The allowed values are @COMPLIANT@ and @NON_COMPLIANT@ .
conformancePackEvaluationFilters ::
  ConformancePackEvaluationFilters
conformancePackEvaluationFilters =
  ConformancePackEvaluationFilters'
    { _cpefResourceIds = Nothing,
      _cpefResourceType = Nothing,
      _cpefConfigRuleNames = Nothing,
      _cpefComplianceType = Nothing
    }

-- | Filters the results by resource IDs.
cpefResourceIds :: Lens' ConformancePackEvaluationFilters [Text]
cpefResourceIds = lens _cpefResourceIds (\s a -> s {_cpefResourceIds = a}) . _Default . _Coerce

-- | Filters the results by the resource type (for example, @"AWS::EC2::Instance"@ ).
cpefResourceType :: Lens' ConformancePackEvaluationFilters (Maybe Text)
cpefResourceType = lens _cpefResourceType (\s a -> s {_cpefResourceType = a})

-- | Filters the results by AWS Config rule names.
cpefConfigRuleNames :: Lens' ConformancePackEvaluationFilters [Text]
cpefConfigRuleNames = lens _cpefConfigRuleNames (\s a -> s {_cpefConfigRuleNames = a}) . _Default . _Coerce

-- | Filters the results by compliance. The allowed values are @COMPLIANT@ and @NON_COMPLIANT@ .
cpefComplianceType :: Lens' ConformancePackEvaluationFilters (Maybe ConformancePackComplianceType)
cpefComplianceType = lens _cpefComplianceType (\s a -> s {_cpefComplianceType = a})

instance Hashable ConformancePackEvaluationFilters

instance NFData ConformancePackEvaluationFilters

instance ToJSON ConformancePackEvaluationFilters where
  toJSON ConformancePackEvaluationFilters' {..} =
    object
      ( catMaybes
          [ ("ResourceIds" .=) <$> _cpefResourceIds,
            ("ResourceType" .=) <$> _cpefResourceType,
            ("ConfigRuleNames" .=) <$> _cpefConfigRuleNames,
            ("ComplianceType" .=) <$> _cpefComplianceType
          ]
      )
