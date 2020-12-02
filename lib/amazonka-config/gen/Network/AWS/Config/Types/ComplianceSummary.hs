{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ComplianceSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ComplianceSummary where

import Network.AWS.Config.Types.ComplianceContributorCount
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The number of AWS Config rules or AWS resources that are compliant and noncompliant.
--
--
--
-- /See:/ 'complianceSummary' smart constructor.
data ComplianceSummary = ComplianceSummary'
  { _csComplianceSummaryTimestamp ::
      !(Maybe POSIX),
    _csCompliantResourceCount ::
      !(Maybe ComplianceContributorCount),
    _csNonCompliantResourceCount ::
      !(Maybe ComplianceContributorCount)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ComplianceSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csComplianceSummaryTimestamp' - The time that AWS Config created the compliance summary.
--
-- * 'csCompliantResourceCount' - The number of AWS Config rules or AWS resources that are compliant, up to a maximum of 25 for rules and 100 for resources.
--
-- * 'csNonCompliantResourceCount' - The number of AWS Config rules or AWS resources that are noncompliant, up to a maximum of 25 for rules and 100 for resources.
complianceSummary ::
  ComplianceSummary
complianceSummary =
  ComplianceSummary'
    { _csComplianceSummaryTimestamp = Nothing,
      _csCompliantResourceCount = Nothing,
      _csNonCompliantResourceCount = Nothing
    }

-- | The time that AWS Config created the compliance summary.
csComplianceSummaryTimestamp :: Lens' ComplianceSummary (Maybe UTCTime)
csComplianceSummaryTimestamp = lens _csComplianceSummaryTimestamp (\s a -> s {_csComplianceSummaryTimestamp = a}) . mapping _Time

-- | The number of AWS Config rules or AWS resources that are compliant, up to a maximum of 25 for rules and 100 for resources.
csCompliantResourceCount :: Lens' ComplianceSummary (Maybe ComplianceContributorCount)
csCompliantResourceCount = lens _csCompliantResourceCount (\s a -> s {_csCompliantResourceCount = a})

-- | The number of AWS Config rules or AWS resources that are noncompliant, up to a maximum of 25 for rules and 100 for resources.
csNonCompliantResourceCount :: Lens' ComplianceSummary (Maybe ComplianceContributorCount)
csNonCompliantResourceCount = lens _csNonCompliantResourceCount (\s a -> s {_csNonCompliantResourceCount = a})

instance FromJSON ComplianceSummary where
  parseJSON =
    withObject
      "ComplianceSummary"
      ( \x ->
          ComplianceSummary'
            <$> (x .:? "ComplianceSummaryTimestamp")
            <*> (x .:? "CompliantResourceCount")
            <*> (x .:? "NonCompliantResourceCount")
      )

instance Hashable ComplianceSummary

instance NFData ComplianceSummary
