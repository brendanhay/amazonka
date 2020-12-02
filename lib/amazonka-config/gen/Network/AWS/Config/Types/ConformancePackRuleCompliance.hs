{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ConformancePackRuleCompliance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ConformancePackRuleCompliance where

import Network.AWS.Config.Types.ConformancePackComplianceType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Compliance information of one or more AWS Config rules within a conformance pack. You can filter using AWS Config rule names and compliance types.
--
--
--
-- /See:/ 'conformancePackRuleCompliance' smart constructor.
data ConformancePackRuleCompliance = ConformancePackRuleCompliance'
  { _cprcConfigRuleName ::
      !(Maybe Text),
    _cprcComplianceType ::
      !( Maybe
           ConformancePackComplianceType
       )
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ConformancePackRuleCompliance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cprcConfigRuleName' - Name of the config rule.
--
-- * 'cprcComplianceType' - Compliance of the AWS Config rule The allowed values are @COMPLIANT@ and @NON_COMPLIANT@ .
conformancePackRuleCompliance ::
  ConformancePackRuleCompliance
conformancePackRuleCompliance =
  ConformancePackRuleCompliance'
    { _cprcConfigRuleName = Nothing,
      _cprcComplianceType = Nothing
    }

-- | Name of the config rule.
cprcConfigRuleName :: Lens' ConformancePackRuleCompliance (Maybe Text)
cprcConfigRuleName = lens _cprcConfigRuleName (\s a -> s {_cprcConfigRuleName = a})

-- | Compliance of the AWS Config rule The allowed values are @COMPLIANT@ and @NON_COMPLIANT@ .
cprcComplianceType :: Lens' ConformancePackRuleCompliance (Maybe ConformancePackComplianceType)
cprcComplianceType = lens _cprcComplianceType (\s a -> s {_cprcComplianceType = a})

instance FromJSON ConformancePackRuleCompliance where
  parseJSON =
    withObject
      "ConformancePackRuleCompliance"
      ( \x ->
          ConformancePackRuleCompliance'
            <$> (x .:? "ConfigRuleName") <*> (x .:? "ComplianceType")
      )

instance Hashable ConformancePackRuleCompliance

instance NFData ConformancePackRuleCompliance
