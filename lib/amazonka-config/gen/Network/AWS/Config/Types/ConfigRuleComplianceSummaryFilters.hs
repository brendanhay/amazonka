{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ConfigRuleComplianceSummaryFilters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ConfigRuleComplianceSummaryFilters where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Filters the results based on the account IDs and regions.
--
--
--
-- /See:/ 'configRuleComplianceSummaryFilters' smart constructor.
data ConfigRuleComplianceSummaryFilters = ConfigRuleComplianceSummaryFilters'
  { _crcsfAccountId ::
      !(Maybe Text),
    _crcsfAWSRegion ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ConfigRuleComplianceSummaryFilters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crcsfAccountId' - The 12-digit account ID of the source account.
--
-- * 'crcsfAWSRegion' - The source region where the data is aggregated.
configRuleComplianceSummaryFilters ::
  ConfigRuleComplianceSummaryFilters
configRuleComplianceSummaryFilters =
  ConfigRuleComplianceSummaryFilters'
    { _crcsfAccountId = Nothing,
      _crcsfAWSRegion = Nothing
    }

-- | The 12-digit account ID of the source account.
crcsfAccountId :: Lens' ConfigRuleComplianceSummaryFilters (Maybe Text)
crcsfAccountId = lens _crcsfAccountId (\s a -> s {_crcsfAccountId = a})

-- | The source region where the data is aggregated.
crcsfAWSRegion :: Lens' ConfigRuleComplianceSummaryFilters (Maybe Text)
crcsfAWSRegion = lens _crcsfAWSRegion (\s a -> s {_crcsfAWSRegion = a})

instance Hashable ConfigRuleComplianceSummaryFilters

instance NFData ConfigRuleComplianceSummaryFilters

instance ToJSON ConfigRuleComplianceSummaryFilters where
  toJSON ConfigRuleComplianceSummaryFilters' {..} =
    object
      ( catMaybes
          [ ("AccountId" .=) <$> _crcsfAccountId,
            ("AwsRegion" .=) <$> _crcsfAWSRegion
          ]
      )
