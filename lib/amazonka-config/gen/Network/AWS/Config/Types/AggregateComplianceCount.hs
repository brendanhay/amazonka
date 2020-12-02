{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.AggregateComplianceCount
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.AggregateComplianceCount where

import Network.AWS.Config.Types.ComplianceSummary
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Returns the number of compliant and noncompliant rules for one or more accounts and regions in an aggregator.
--
--
--
-- /See:/ 'aggregateComplianceCount' smart constructor.
data AggregateComplianceCount = AggregateComplianceCount'
  { _accGroupName ::
      !(Maybe Text),
    _accComplianceSummary ::
      !(Maybe ComplianceSummary)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AggregateComplianceCount' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'accGroupName' - The 12-digit account ID or region based on the GroupByKey value.
--
-- * 'accComplianceSummary' - The number of compliant and noncompliant AWS Config rules.
aggregateComplianceCount ::
  AggregateComplianceCount
aggregateComplianceCount =
  AggregateComplianceCount'
    { _accGroupName = Nothing,
      _accComplianceSummary = Nothing
    }

-- | The 12-digit account ID or region based on the GroupByKey value.
accGroupName :: Lens' AggregateComplianceCount (Maybe Text)
accGroupName = lens _accGroupName (\s a -> s {_accGroupName = a})

-- | The number of compliant and noncompliant AWS Config rules.
accComplianceSummary :: Lens' AggregateComplianceCount (Maybe ComplianceSummary)
accComplianceSummary = lens _accComplianceSummary (\s a -> s {_accComplianceSummary = a})

instance FromJSON AggregateComplianceCount where
  parseJSON =
    withObject
      "AggregateComplianceCount"
      ( \x ->
          AggregateComplianceCount'
            <$> (x .:? "GroupName") <*> (x .:? "ComplianceSummary")
      )

instance Hashable AggregateComplianceCount

instance NFData AggregateComplianceCount
