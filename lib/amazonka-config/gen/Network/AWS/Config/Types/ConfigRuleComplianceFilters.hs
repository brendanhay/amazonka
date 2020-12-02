{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ConfigRuleComplianceFilters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ConfigRuleComplianceFilters where

import Network.AWS.Config.Types.ComplianceType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Filters the compliance results based on account ID, region, compliance type, and rule name.
--
--
--
-- /See:/ 'configRuleComplianceFilters' smart constructor.
data ConfigRuleComplianceFilters = ConfigRuleComplianceFilters'
  { _crcfConfigRuleName ::
      !(Maybe Text),
    _crcfAccountId :: !(Maybe Text),
    _crcfComplianceType ::
      !(Maybe ComplianceType),
    _crcfAWSRegion :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ConfigRuleComplianceFilters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crcfConfigRuleName' - The name of the AWS Config rule.
--
-- * 'crcfAccountId' - The 12-digit account ID of the source account.
--
-- * 'crcfComplianceType' - The rule compliance status. For the @ConfigRuleComplianceFilters@ data type, AWS Config supports only @COMPLIANT@ and @NON_COMPLIANT@ . AWS Config does not support the @NOT_APPLICABLE@ and the @INSUFFICIENT_DATA@ values.
--
-- * 'crcfAWSRegion' - The source region where the data is aggregated.
configRuleComplianceFilters ::
  ConfigRuleComplianceFilters
configRuleComplianceFilters =
  ConfigRuleComplianceFilters'
    { _crcfConfigRuleName = Nothing,
      _crcfAccountId = Nothing,
      _crcfComplianceType = Nothing,
      _crcfAWSRegion = Nothing
    }

-- | The name of the AWS Config rule.
crcfConfigRuleName :: Lens' ConfigRuleComplianceFilters (Maybe Text)
crcfConfigRuleName = lens _crcfConfigRuleName (\s a -> s {_crcfConfigRuleName = a})

-- | The 12-digit account ID of the source account.
crcfAccountId :: Lens' ConfigRuleComplianceFilters (Maybe Text)
crcfAccountId = lens _crcfAccountId (\s a -> s {_crcfAccountId = a})

-- | The rule compliance status. For the @ConfigRuleComplianceFilters@ data type, AWS Config supports only @COMPLIANT@ and @NON_COMPLIANT@ . AWS Config does not support the @NOT_APPLICABLE@ and the @INSUFFICIENT_DATA@ values.
crcfComplianceType :: Lens' ConfigRuleComplianceFilters (Maybe ComplianceType)
crcfComplianceType = lens _crcfComplianceType (\s a -> s {_crcfComplianceType = a})

-- | The source region where the data is aggregated.
crcfAWSRegion :: Lens' ConfigRuleComplianceFilters (Maybe Text)
crcfAWSRegion = lens _crcfAWSRegion (\s a -> s {_crcfAWSRegion = a})

instance Hashable ConfigRuleComplianceFilters

instance NFData ConfigRuleComplianceFilters

instance ToJSON ConfigRuleComplianceFilters where
  toJSON ConfigRuleComplianceFilters' {..} =
    object
      ( catMaybes
          [ ("ConfigRuleName" .=) <$> _crcfConfigRuleName,
            ("AccountId" .=) <$> _crcfAccountId,
            ("ComplianceType" .=) <$> _crcfComplianceType,
            ("AwsRegion" .=) <$> _crcfAWSRegion
          ]
      )
