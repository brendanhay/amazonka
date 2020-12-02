{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.ResourceComplianceSummaryItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.ResourceComplianceSummaryItem where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SSM.Types.ComplianceExecutionSummary
import Network.AWS.SSM.Types.ComplianceSeverity
import Network.AWS.SSM.Types.ComplianceStatus
import Network.AWS.SSM.Types.CompliantSummary
import Network.AWS.SSM.Types.NonCompliantSummary

-- | Compliance summary information for a specific resource.
--
--
--
-- /See:/ 'resourceComplianceSummaryItem' smart constructor.
data ResourceComplianceSummaryItem = ResourceComplianceSummaryItem'
  { _rcsiNonCompliantSummary ::
      !(Maybe NonCompliantSummary),
    _rcsiStatus ::
      !(Maybe ComplianceStatus),
    _rcsiResourceId ::
      !(Maybe Text),
    _rcsiResourceType ::
      !(Maybe Text),
    _rcsiCompliantSummary ::
      !(Maybe CompliantSummary),
    _rcsiExecutionSummary ::
      !( Maybe
           ComplianceExecutionSummary
       ),
    _rcsiOverallSeverity ::
      !(Maybe ComplianceSeverity),
    _rcsiComplianceType ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ResourceComplianceSummaryItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcsiNonCompliantSummary' - A list of items that aren't compliant for the resource.
--
-- * 'rcsiStatus' - The compliance status for the resource.
--
-- * 'rcsiResourceId' - The resource ID.
--
-- * 'rcsiResourceType' - The resource type.
--
-- * 'rcsiCompliantSummary' - A list of items that are compliant for the resource.
--
-- * 'rcsiExecutionSummary' - Information about the execution.
--
-- * 'rcsiOverallSeverity' - The highest severity item found for the resource. The resource is compliant for this item.
--
-- * 'rcsiComplianceType' - The compliance type.
resourceComplianceSummaryItem ::
  ResourceComplianceSummaryItem
resourceComplianceSummaryItem =
  ResourceComplianceSummaryItem'
    { _rcsiNonCompliantSummary =
        Nothing,
      _rcsiStatus = Nothing,
      _rcsiResourceId = Nothing,
      _rcsiResourceType = Nothing,
      _rcsiCompliantSummary = Nothing,
      _rcsiExecutionSummary = Nothing,
      _rcsiOverallSeverity = Nothing,
      _rcsiComplianceType = Nothing
    }

-- | A list of items that aren't compliant for the resource.
rcsiNonCompliantSummary :: Lens' ResourceComplianceSummaryItem (Maybe NonCompliantSummary)
rcsiNonCompliantSummary = lens _rcsiNonCompliantSummary (\s a -> s {_rcsiNonCompliantSummary = a})

-- | The compliance status for the resource.
rcsiStatus :: Lens' ResourceComplianceSummaryItem (Maybe ComplianceStatus)
rcsiStatus = lens _rcsiStatus (\s a -> s {_rcsiStatus = a})

-- | The resource ID.
rcsiResourceId :: Lens' ResourceComplianceSummaryItem (Maybe Text)
rcsiResourceId = lens _rcsiResourceId (\s a -> s {_rcsiResourceId = a})

-- | The resource type.
rcsiResourceType :: Lens' ResourceComplianceSummaryItem (Maybe Text)
rcsiResourceType = lens _rcsiResourceType (\s a -> s {_rcsiResourceType = a})

-- | A list of items that are compliant for the resource.
rcsiCompliantSummary :: Lens' ResourceComplianceSummaryItem (Maybe CompliantSummary)
rcsiCompliantSummary = lens _rcsiCompliantSummary (\s a -> s {_rcsiCompliantSummary = a})

-- | Information about the execution.
rcsiExecutionSummary :: Lens' ResourceComplianceSummaryItem (Maybe ComplianceExecutionSummary)
rcsiExecutionSummary = lens _rcsiExecutionSummary (\s a -> s {_rcsiExecutionSummary = a})

-- | The highest severity item found for the resource. The resource is compliant for this item.
rcsiOverallSeverity :: Lens' ResourceComplianceSummaryItem (Maybe ComplianceSeverity)
rcsiOverallSeverity = lens _rcsiOverallSeverity (\s a -> s {_rcsiOverallSeverity = a})

-- | The compliance type.
rcsiComplianceType :: Lens' ResourceComplianceSummaryItem (Maybe Text)
rcsiComplianceType = lens _rcsiComplianceType (\s a -> s {_rcsiComplianceType = a})

instance FromJSON ResourceComplianceSummaryItem where
  parseJSON =
    withObject
      "ResourceComplianceSummaryItem"
      ( \x ->
          ResourceComplianceSummaryItem'
            <$> (x .:? "NonCompliantSummary")
            <*> (x .:? "Status")
            <*> (x .:? "ResourceId")
            <*> (x .:? "ResourceType")
            <*> (x .:? "CompliantSummary")
            <*> (x .:? "ExecutionSummary")
            <*> (x .:? "OverallSeverity")
            <*> (x .:? "ComplianceType")
      )

instance Hashable ResourceComplianceSummaryItem

instance NFData ResourceComplianceSummaryItem
