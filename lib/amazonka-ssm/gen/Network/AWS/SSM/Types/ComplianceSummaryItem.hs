{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.ComplianceSummaryItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.ComplianceSummaryItem where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SSM.Types.CompliantSummary
import Network.AWS.SSM.Types.NonCompliantSummary

-- | A summary of compliance information by compliance type.
--
--
--
-- /See:/ 'complianceSummaryItem' smart constructor.
data ComplianceSummaryItem = ComplianceSummaryItem'
  { _csiNonCompliantSummary ::
      !(Maybe NonCompliantSummary),
    _csiCompliantSummary ::
      !(Maybe CompliantSummary),
    _csiComplianceType :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ComplianceSummaryItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csiNonCompliantSummary' - A list of NON_COMPLIANT items for the specified compliance type.
--
-- * 'csiCompliantSummary' - A list of COMPLIANT items for the specified compliance type.
--
-- * 'csiComplianceType' - The type of compliance item. For example, the compliance type can be Association, Patch, or Custom:string.
complianceSummaryItem ::
  ComplianceSummaryItem
complianceSummaryItem =
  ComplianceSummaryItem'
    { _csiNonCompliantSummary = Nothing,
      _csiCompliantSummary = Nothing,
      _csiComplianceType = Nothing
    }

-- | A list of NON_COMPLIANT items for the specified compliance type.
csiNonCompliantSummary :: Lens' ComplianceSummaryItem (Maybe NonCompliantSummary)
csiNonCompliantSummary = lens _csiNonCompliantSummary (\s a -> s {_csiNonCompliantSummary = a})

-- | A list of COMPLIANT items for the specified compliance type.
csiCompliantSummary :: Lens' ComplianceSummaryItem (Maybe CompliantSummary)
csiCompliantSummary = lens _csiCompliantSummary (\s a -> s {_csiCompliantSummary = a})

-- | The type of compliance item. For example, the compliance type can be Association, Patch, or Custom:string.
csiComplianceType :: Lens' ComplianceSummaryItem (Maybe Text)
csiComplianceType = lens _csiComplianceType (\s a -> s {_csiComplianceType = a})

instance FromJSON ComplianceSummaryItem where
  parseJSON =
    withObject
      "ComplianceSummaryItem"
      ( \x ->
          ComplianceSummaryItem'
            <$> (x .:? "NonCompliantSummary")
            <*> (x .:? "CompliantSummary")
            <*> (x .:? "ComplianceType")
      )

instance Hashable ComplianceSummaryItem

instance NFData ComplianceSummaryItem
