{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.Compliance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.Compliance where

import Network.AWS.Config.Types.ComplianceContributorCount
import Network.AWS.Config.Types.ComplianceType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Indicates whether an AWS resource or AWS Config rule is compliant and provides the number of contributors that affect the compliance.
--
--
--
-- /See:/ 'compliance' smart constructor.
data Compliance = Compliance'
  { _cComplianceContributorCount ::
      !(Maybe ComplianceContributorCount),
    _cComplianceType :: !(Maybe ComplianceType)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Compliance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cComplianceContributorCount' - The number of AWS resources or AWS Config rules that cause a result of @NON_COMPLIANT@ , up to a maximum number.
--
-- * 'cComplianceType' - Indicates whether an AWS resource or AWS Config rule is compliant. A resource is compliant if it complies with all of the AWS Config rules that evaluate it. A resource is noncompliant if it does not comply with one or more of these rules. A rule is compliant if all of the resources that the rule evaluates comply with it. A rule is noncompliant if any of these resources do not comply. AWS Config returns the @INSUFFICIENT_DATA@ value when no evaluation results are available for the AWS resource or AWS Config rule. For the @Compliance@ data type, AWS Config supports only @COMPLIANT@ , @NON_COMPLIANT@ , and @INSUFFICIENT_DATA@ values. AWS Config does not support the @NOT_APPLICABLE@ value for the @Compliance@ data type.
compliance ::
  Compliance
compliance =
  Compliance'
    { _cComplianceContributorCount = Nothing,
      _cComplianceType = Nothing
    }

-- | The number of AWS resources or AWS Config rules that cause a result of @NON_COMPLIANT@ , up to a maximum number.
cComplianceContributorCount :: Lens' Compliance (Maybe ComplianceContributorCount)
cComplianceContributorCount = lens _cComplianceContributorCount (\s a -> s {_cComplianceContributorCount = a})

-- | Indicates whether an AWS resource or AWS Config rule is compliant. A resource is compliant if it complies with all of the AWS Config rules that evaluate it. A resource is noncompliant if it does not comply with one or more of these rules. A rule is compliant if all of the resources that the rule evaluates comply with it. A rule is noncompliant if any of these resources do not comply. AWS Config returns the @INSUFFICIENT_DATA@ value when no evaluation results are available for the AWS resource or AWS Config rule. For the @Compliance@ data type, AWS Config supports only @COMPLIANT@ , @NON_COMPLIANT@ , and @INSUFFICIENT_DATA@ values. AWS Config does not support the @NOT_APPLICABLE@ value for the @Compliance@ data type.
cComplianceType :: Lens' Compliance (Maybe ComplianceType)
cComplianceType = lens _cComplianceType (\s a -> s {_cComplianceType = a})

instance FromJSON Compliance where
  parseJSON =
    withObject
      "Compliance"
      ( \x ->
          Compliance'
            <$> (x .:? "ComplianceContributorCount") <*> (x .:? "ComplianceType")
      )

instance Hashable Compliance

instance NFData Compliance
