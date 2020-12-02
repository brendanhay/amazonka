{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.ComplianceItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.ComplianceItem where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SSM.Types.ComplianceExecutionSummary
import Network.AWS.SSM.Types.ComplianceSeverity
import Network.AWS.SSM.Types.ComplianceStatus

-- | Information about the compliance as defined by the resource type. For example, for a patch resource type, @Items@ includes information about the PatchSeverity, Classification, and so on.
--
--
--
-- /See:/ 'complianceItem' smart constructor.
data ComplianceItem = ComplianceItem'
  { _ciStatus ::
      !(Maybe ComplianceStatus),
    _ciResourceId :: !(Maybe Text),
    _ciResourceType :: !(Maybe Text),
    _ciSeverity :: !(Maybe ComplianceSeverity),
    _ciExecutionSummary :: !(Maybe ComplianceExecutionSummary),
    _ciDetails :: !(Maybe (Map Text (Text))),
    _ciId :: !(Maybe Text),
    _ciComplianceType :: !(Maybe Text),
    _ciTitle :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ComplianceItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ciStatus' - The status of the compliance item. An item is either COMPLIANT, NON_COMPLIANT, or an empty string (for Windows patches that aren't applicable).
--
-- * 'ciResourceId' - An ID for the resource. For a managed instance, this is the instance ID.
--
-- * 'ciResourceType' - The type of resource. @ManagedInstance@ is currently the only supported resource type.
--
-- * 'ciSeverity' - The severity of the compliance status. Severity can be one of the following: Critical, High, Medium, Low, Informational, Unspecified.
--
-- * 'ciExecutionSummary' - A summary for the compliance item. The summary includes an execution ID, the execution type (for example, command), and the execution time.
--
-- * 'ciDetails' - A "Key": "Value" tag combination for the compliance item.
--
-- * 'ciId' - An ID for the compliance item. For example, if the compliance item is a Windows patch, the ID could be the number of the KB article; for example: KB4010320.
--
-- * 'ciComplianceType' - The compliance type. For example, Association (for a State Manager association), Patch, or Custom:@string@ are all valid compliance types.
--
-- * 'ciTitle' - A title for the compliance item. For example, if the compliance item is a Windows patch, the title could be the title of the KB article for the patch; for example: Security Update for Active Directory Federation Services.
complianceItem ::
  ComplianceItem
complianceItem =
  ComplianceItem'
    { _ciStatus = Nothing,
      _ciResourceId = Nothing,
      _ciResourceType = Nothing,
      _ciSeverity = Nothing,
      _ciExecutionSummary = Nothing,
      _ciDetails = Nothing,
      _ciId = Nothing,
      _ciComplianceType = Nothing,
      _ciTitle = Nothing
    }

-- | The status of the compliance item. An item is either COMPLIANT, NON_COMPLIANT, or an empty string (for Windows patches that aren't applicable).
ciStatus :: Lens' ComplianceItem (Maybe ComplianceStatus)
ciStatus = lens _ciStatus (\s a -> s {_ciStatus = a})

-- | An ID for the resource. For a managed instance, this is the instance ID.
ciResourceId :: Lens' ComplianceItem (Maybe Text)
ciResourceId = lens _ciResourceId (\s a -> s {_ciResourceId = a})

-- | The type of resource. @ManagedInstance@ is currently the only supported resource type.
ciResourceType :: Lens' ComplianceItem (Maybe Text)
ciResourceType = lens _ciResourceType (\s a -> s {_ciResourceType = a})

-- | The severity of the compliance status. Severity can be one of the following: Critical, High, Medium, Low, Informational, Unspecified.
ciSeverity :: Lens' ComplianceItem (Maybe ComplianceSeverity)
ciSeverity = lens _ciSeverity (\s a -> s {_ciSeverity = a})

-- | A summary for the compliance item. The summary includes an execution ID, the execution type (for example, command), and the execution time.
ciExecutionSummary :: Lens' ComplianceItem (Maybe ComplianceExecutionSummary)
ciExecutionSummary = lens _ciExecutionSummary (\s a -> s {_ciExecutionSummary = a})

-- | A "Key": "Value" tag combination for the compliance item.
ciDetails :: Lens' ComplianceItem (HashMap Text (Text))
ciDetails = lens _ciDetails (\s a -> s {_ciDetails = a}) . _Default . _Map

-- | An ID for the compliance item. For example, if the compliance item is a Windows patch, the ID could be the number of the KB article; for example: KB4010320.
ciId :: Lens' ComplianceItem (Maybe Text)
ciId = lens _ciId (\s a -> s {_ciId = a})

-- | The compliance type. For example, Association (for a State Manager association), Patch, or Custom:@string@ are all valid compliance types.
ciComplianceType :: Lens' ComplianceItem (Maybe Text)
ciComplianceType = lens _ciComplianceType (\s a -> s {_ciComplianceType = a})

-- | A title for the compliance item. For example, if the compliance item is a Windows patch, the title could be the title of the KB article for the patch; for example: Security Update for Active Directory Federation Services.
ciTitle :: Lens' ComplianceItem (Maybe Text)
ciTitle = lens _ciTitle (\s a -> s {_ciTitle = a})

instance FromJSON ComplianceItem where
  parseJSON =
    withObject
      "ComplianceItem"
      ( \x ->
          ComplianceItem'
            <$> (x .:? "Status")
            <*> (x .:? "ResourceId")
            <*> (x .:? "ResourceType")
            <*> (x .:? "Severity")
            <*> (x .:? "ExecutionSummary")
            <*> (x .:? "Details" .!= mempty)
            <*> (x .:? "Id")
            <*> (x .:? "ComplianceType")
            <*> (x .:? "Title")
      )

instance Hashable ComplianceItem

instance NFData ComplianceItem
