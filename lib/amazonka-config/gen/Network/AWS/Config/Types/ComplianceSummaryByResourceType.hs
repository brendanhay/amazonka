{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ComplianceSummaryByResourceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ComplianceSummaryByResourceType where

import Network.AWS.Config.Types.ComplianceSummary
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The number of AWS resources of a specific type that are compliant or noncompliant, up to a maximum of 100 for each.
--
--
--
-- /See:/ 'complianceSummaryByResourceType' smart constructor.
data ComplianceSummaryByResourceType = ComplianceSummaryByResourceType'
  { _csbrtResourceType ::
      !(Maybe Text),
    _csbrtComplianceSummary ::
      !(Maybe ComplianceSummary)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ComplianceSummaryByResourceType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csbrtResourceType' - The type of AWS resource.
--
-- * 'csbrtComplianceSummary' - The number of AWS resources that are compliant or noncompliant, up to a maximum of 100 for each.
complianceSummaryByResourceType ::
  ComplianceSummaryByResourceType
complianceSummaryByResourceType =
  ComplianceSummaryByResourceType'
    { _csbrtResourceType = Nothing,
      _csbrtComplianceSummary = Nothing
    }

-- | The type of AWS resource.
csbrtResourceType :: Lens' ComplianceSummaryByResourceType (Maybe Text)
csbrtResourceType = lens _csbrtResourceType (\s a -> s {_csbrtResourceType = a})

-- | The number of AWS resources that are compliant or noncompliant, up to a maximum of 100 for each.
csbrtComplianceSummary :: Lens' ComplianceSummaryByResourceType (Maybe ComplianceSummary)
csbrtComplianceSummary = lens _csbrtComplianceSummary (\s a -> s {_csbrtComplianceSummary = a})

instance FromJSON ComplianceSummaryByResourceType where
  parseJSON =
    withObject
      "ComplianceSummaryByResourceType"
      ( \x ->
          ComplianceSummaryByResourceType'
            <$> (x .:? "ResourceType") <*> (x .:? "ComplianceSummary")
      )

instance Hashable ComplianceSummaryByResourceType

instance NFData ComplianceSummaryByResourceType
