{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.AggregateComplianceByConfigRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.AggregateComplianceByConfigRule where

import Network.AWS.Config.Types.Compliance
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Indicates whether an AWS Config rule is compliant based on account ID, region, compliance, and rule name.
--
--
-- A rule is compliant if all of the resources that the rule evaluated comply with it. It is noncompliant if any of these resources do not comply.
--
--
-- /See:/ 'aggregateComplianceByConfigRule' smart constructor.
data AggregateComplianceByConfigRule = AggregateComplianceByConfigRule'
  { _acbcrCompliance ::
      !(Maybe Compliance),
    _acbcrConfigRuleName ::
      !(Maybe Text),
    _acbcrAccountId ::
      !(Maybe Text),
    _acbcrAWSRegion ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AggregateComplianceByConfigRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acbcrCompliance' - Indicates whether an AWS resource or AWS Config rule is compliant and provides the number of contributors that affect the compliance.
--
-- * 'acbcrConfigRuleName' - The name of the AWS Config rule.
--
-- * 'acbcrAccountId' - The 12-digit account ID of the source account.
--
-- * 'acbcrAWSRegion' - The source region from where the data is aggregated.
aggregateComplianceByConfigRule ::
  AggregateComplianceByConfigRule
aggregateComplianceByConfigRule =
  AggregateComplianceByConfigRule'
    { _acbcrCompliance = Nothing,
      _acbcrConfigRuleName = Nothing,
      _acbcrAccountId = Nothing,
      _acbcrAWSRegion = Nothing
    }

-- | Indicates whether an AWS resource or AWS Config rule is compliant and provides the number of contributors that affect the compliance.
acbcrCompliance :: Lens' AggregateComplianceByConfigRule (Maybe Compliance)
acbcrCompliance = lens _acbcrCompliance (\s a -> s {_acbcrCompliance = a})

-- | The name of the AWS Config rule.
acbcrConfigRuleName :: Lens' AggregateComplianceByConfigRule (Maybe Text)
acbcrConfigRuleName = lens _acbcrConfigRuleName (\s a -> s {_acbcrConfigRuleName = a})

-- | The 12-digit account ID of the source account.
acbcrAccountId :: Lens' AggregateComplianceByConfigRule (Maybe Text)
acbcrAccountId = lens _acbcrAccountId (\s a -> s {_acbcrAccountId = a})

-- | The source region from where the data is aggregated.
acbcrAWSRegion :: Lens' AggregateComplianceByConfigRule (Maybe Text)
acbcrAWSRegion = lens _acbcrAWSRegion (\s a -> s {_acbcrAWSRegion = a})

instance FromJSON AggregateComplianceByConfigRule where
  parseJSON =
    withObject
      "AggregateComplianceByConfigRule"
      ( \x ->
          AggregateComplianceByConfigRule'
            <$> (x .:? "Compliance")
            <*> (x .:? "ConfigRuleName")
            <*> (x .:? "AccountId")
            <*> (x .:? "AwsRegion")
      )

instance Hashable AggregateComplianceByConfigRule

instance NFData AggregateComplianceByConfigRule
