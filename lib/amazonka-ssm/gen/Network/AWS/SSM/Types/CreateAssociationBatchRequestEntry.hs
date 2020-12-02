{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.CreateAssociationBatchRequestEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.CreateAssociationBatchRequestEntry where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SSM.Types.AssociationComplianceSeverity
import Network.AWS.SSM.Types.AssociationSyncCompliance
import Network.AWS.SSM.Types.InstanceAssociationOutputLocation
import Network.AWS.SSM.Types.Target

-- | Describes the association of a Systems Manager SSM document and an instance.
--
--
--
-- /See:/ 'createAssociationBatchRequestEntry' smart constructor.
data CreateAssociationBatchRequestEntry = CreateAssociationBatchRequestEntry'
  { _cabreInstanceId ::
      !(Maybe Text),
    _cabreApplyOnlyAtCronInterval ::
      !(Maybe Bool),
    _cabreMaxErrors ::
      !(Maybe Text),
    _cabreScheduleExpression ::
      !(Maybe Text),
    _cabreOutputLocation ::
      !( Maybe
           InstanceAssociationOutputLocation
       ),
    _cabreSyncCompliance ::
      !( Maybe
           AssociationSyncCompliance
       ),
    _cabreTargets ::
      !(Maybe [Target]),
    _cabreParameters ::
      !( Maybe
           ( Map
               Text
               ([Text])
           )
       ),
    _cabreDocumentVersion ::
      !(Maybe Text),
    _cabreAutomationTargetParameterName ::
      !(Maybe Text),
    _cabreAssociationName ::
      !(Maybe Text),
    _cabreComplianceSeverity ::
      !( Maybe
           AssociationComplianceSeverity
       ),
    _cabreMaxConcurrency ::
      !(Maybe Text),
    _cabreName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateAssociationBatchRequestEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cabreInstanceId' - The ID of the instance.
--
-- * 'cabreApplyOnlyAtCronInterval' - By default, when you create a new associations, the system runs it immediately after it is created and then according to the schedule you specified. Specify this option if you don't want an association to run immediately after you create it.
--
-- * 'cabreMaxErrors' - The number of errors that are allowed before the system stops sending requests to run the association on additional targets. You can specify either an absolute number of errors, for example 10, or a percentage of the target set, for example 10%. If you specify 3, for example, the system stops sending requests when the fourth error is received. If you specify 0, then the system stops sending requests after the first error is returned. If you run an association on 50 instances and set MaxError to 10%, then the system stops sending the request when the sixth error is received. Executions that are already running an association when MaxErrors is reached are allowed to complete, but some of these executions may fail as well. If you need to ensure that there won't be more than max-errors failed executions, set MaxConcurrency to 1 so that executions proceed one at a time.
--
-- * 'cabreScheduleExpression' - A cron expression that specifies a schedule when the association runs.
--
-- * 'cabreOutputLocation' - An S3 bucket where you want to store the results of this request.
--
-- * 'cabreSyncCompliance' - The mode for generating association compliance. You can specify @AUTO@ or @MANUAL@ . In @AUTO@ mode, the system uses the status of the association execution to determine the compliance status. If the association execution runs successfully, then the association is @COMPLIANT@ . If the association execution doesn't run successfully, the association is @NON-COMPLIANT@ .  In @MANUAL@ mode, you must specify the @AssociationId@ as a parameter for the 'PutComplianceItems' API action. In this case, compliance data is not managed by State Manager. It is managed by your direct call to the 'PutComplianceItems' API action. By default, all associations use @AUTO@ mode.
--
-- * 'cabreTargets' - The instances targeted by the request.
--
-- * 'cabreParameters' - A description of the parameters for a document.
--
-- * 'cabreDocumentVersion' - The document version.
--
-- * 'cabreAutomationTargetParameterName' - Specify the target for the association. This target is required for associations that use an Automation document and target resources by using rate controls.
--
-- * 'cabreAssociationName' - Specify a descriptive name for the association.
--
-- * 'cabreComplianceSeverity' - The severity level to assign to the association.
--
-- * 'cabreMaxConcurrency' - The maximum number of targets allowed to run the association at the same time. You can specify a number, for example 10, or a percentage of the target set, for example 10%. The default value is 100%, which means all targets run the association at the same time. If a new instance starts and attempts to run an association while Systems Manager is running MaxConcurrency associations, the association is allowed to run. During the next association interval, the new instance will process its association within the limit specified for MaxConcurrency.
--
-- * 'cabreName' - The name of the SSM document that contains the configuration information for the instance. You can specify Command or Automation documents. You can specify AWS-predefined documents, documents you created, or a document that is shared with you from another account. For SSM documents that are shared with you from other AWS accounts, you must specify the complete SSM document ARN, in the following format: @arn:aws:ssm:/region/ :/account-id/ :document//document-name/ @  For example: @arn:aws:ssm:us-east-2:12345678912:document/My-Shared-Document@  For AWS-predefined documents and SSM documents you created in your account, you only need to specify the document name. For example, @AWS-ApplyPatchBaseline@ or @My-Document@ .
createAssociationBatchRequestEntry ::
  -- | 'cabreName'
  Text ->
  CreateAssociationBatchRequestEntry
createAssociationBatchRequestEntry pName_ =
  CreateAssociationBatchRequestEntry'
    { _cabreInstanceId = Nothing,
      _cabreApplyOnlyAtCronInterval = Nothing,
      _cabreMaxErrors = Nothing,
      _cabreScheduleExpression = Nothing,
      _cabreOutputLocation = Nothing,
      _cabreSyncCompliance = Nothing,
      _cabreTargets = Nothing,
      _cabreParameters = Nothing,
      _cabreDocumentVersion = Nothing,
      _cabreAutomationTargetParameterName = Nothing,
      _cabreAssociationName = Nothing,
      _cabreComplianceSeverity = Nothing,
      _cabreMaxConcurrency = Nothing,
      _cabreName = pName_
    }

-- | The ID of the instance.
cabreInstanceId :: Lens' CreateAssociationBatchRequestEntry (Maybe Text)
cabreInstanceId = lens _cabreInstanceId (\s a -> s {_cabreInstanceId = a})

-- | By default, when you create a new associations, the system runs it immediately after it is created and then according to the schedule you specified. Specify this option if you don't want an association to run immediately after you create it.
cabreApplyOnlyAtCronInterval :: Lens' CreateAssociationBatchRequestEntry (Maybe Bool)
cabreApplyOnlyAtCronInterval = lens _cabreApplyOnlyAtCronInterval (\s a -> s {_cabreApplyOnlyAtCronInterval = a})

-- | The number of errors that are allowed before the system stops sending requests to run the association on additional targets. You can specify either an absolute number of errors, for example 10, or a percentage of the target set, for example 10%. If you specify 3, for example, the system stops sending requests when the fourth error is received. If you specify 0, then the system stops sending requests after the first error is returned. If you run an association on 50 instances and set MaxError to 10%, then the system stops sending the request when the sixth error is received. Executions that are already running an association when MaxErrors is reached are allowed to complete, but some of these executions may fail as well. If you need to ensure that there won't be more than max-errors failed executions, set MaxConcurrency to 1 so that executions proceed one at a time.
cabreMaxErrors :: Lens' CreateAssociationBatchRequestEntry (Maybe Text)
cabreMaxErrors = lens _cabreMaxErrors (\s a -> s {_cabreMaxErrors = a})

-- | A cron expression that specifies a schedule when the association runs.
cabreScheduleExpression :: Lens' CreateAssociationBatchRequestEntry (Maybe Text)
cabreScheduleExpression = lens _cabreScheduleExpression (\s a -> s {_cabreScheduleExpression = a})

-- | An S3 bucket where you want to store the results of this request.
cabreOutputLocation :: Lens' CreateAssociationBatchRequestEntry (Maybe InstanceAssociationOutputLocation)
cabreOutputLocation = lens _cabreOutputLocation (\s a -> s {_cabreOutputLocation = a})

-- | The mode for generating association compliance. You can specify @AUTO@ or @MANUAL@ . In @AUTO@ mode, the system uses the status of the association execution to determine the compliance status. If the association execution runs successfully, then the association is @COMPLIANT@ . If the association execution doesn't run successfully, the association is @NON-COMPLIANT@ .  In @MANUAL@ mode, you must specify the @AssociationId@ as a parameter for the 'PutComplianceItems' API action. In this case, compliance data is not managed by State Manager. It is managed by your direct call to the 'PutComplianceItems' API action. By default, all associations use @AUTO@ mode.
cabreSyncCompliance :: Lens' CreateAssociationBatchRequestEntry (Maybe AssociationSyncCompliance)
cabreSyncCompliance = lens _cabreSyncCompliance (\s a -> s {_cabreSyncCompliance = a})

-- | The instances targeted by the request.
cabreTargets :: Lens' CreateAssociationBatchRequestEntry [Target]
cabreTargets = lens _cabreTargets (\s a -> s {_cabreTargets = a}) . _Default . _Coerce

-- | A description of the parameters for a document.
cabreParameters :: Lens' CreateAssociationBatchRequestEntry (HashMap Text ([Text]))
cabreParameters = lens _cabreParameters (\s a -> s {_cabreParameters = a}) . _Default . _Map

-- | The document version.
cabreDocumentVersion :: Lens' CreateAssociationBatchRequestEntry (Maybe Text)
cabreDocumentVersion = lens _cabreDocumentVersion (\s a -> s {_cabreDocumentVersion = a})

-- | Specify the target for the association. This target is required for associations that use an Automation document and target resources by using rate controls.
cabreAutomationTargetParameterName :: Lens' CreateAssociationBatchRequestEntry (Maybe Text)
cabreAutomationTargetParameterName = lens _cabreAutomationTargetParameterName (\s a -> s {_cabreAutomationTargetParameterName = a})

-- | Specify a descriptive name for the association.
cabreAssociationName :: Lens' CreateAssociationBatchRequestEntry (Maybe Text)
cabreAssociationName = lens _cabreAssociationName (\s a -> s {_cabreAssociationName = a})

-- | The severity level to assign to the association.
cabreComplianceSeverity :: Lens' CreateAssociationBatchRequestEntry (Maybe AssociationComplianceSeverity)
cabreComplianceSeverity = lens _cabreComplianceSeverity (\s a -> s {_cabreComplianceSeverity = a})

-- | The maximum number of targets allowed to run the association at the same time. You can specify a number, for example 10, or a percentage of the target set, for example 10%. The default value is 100%, which means all targets run the association at the same time. If a new instance starts and attempts to run an association while Systems Manager is running MaxConcurrency associations, the association is allowed to run. During the next association interval, the new instance will process its association within the limit specified for MaxConcurrency.
cabreMaxConcurrency :: Lens' CreateAssociationBatchRequestEntry (Maybe Text)
cabreMaxConcurrency = lens _cabreMaxConcurrency (\s a -> s {_cabreMaxConcurrency = a})

-- | The name of the SSM document that contains the configuration information for the instance. You can specify Command or Automation documents. You can specify AWS-predefined documents, documents you created, or a document that is shared with you from another account. For SSM documents that are shared with you from other AWS accounts, you must specify the complete SSM document ARN, in the following format: @arn:aws:ssm:/region/ :/account-id/ :document//document-name/ @  For example: @arn:aws:ssm:us-east-2:12345678912:document/My-Shared-Document@  For AWS-predefined documents and SSM documents you created in your account, you only need to specify the document name. For example, @AWS-ApplyPatchBaseline@ or @My-Document@ .
cabreName :: Lens' CreateAssociationBatchRequestEntry Text
cabreName = lens _cabreName (\s a -> s {_cabreName = a})

instance FromJSON CreateAssociationBatchRequestEntry where
  parseJSON =
    withObject
      "CreateAssociationBatchRequestEntry"
      ( \x ->
          CreateAssociationBatchRequestEntry'
            <$> (x .:? "InstanceId")
            <*> (x .:? "ApplyOnlyAtCronInterval")
            <*> (x .:? "MaxErrors")
            <*> (x .:? "ScheduleExpression")
            <*> (x .:? "OutputLocation")
            <*> (x .:? "SyncCompliance")
            <*> (x .:? "Targets" .!= mempty)
            <*> (x .:? "Parameters" .!= mempty)
            <*> (x .:? "DocumentVersion")
            <*> (x .:? "AutomationTargetParameterName")
            <*> (x .:? "AssociationName")
            <*> (x .:? "ComplianceSeverity")
            <*> (x .:? "MaxConcurrency")
            <*> (x .: "Name")
      )

instance Hashable CreateAssociationBatchRequestEntry

instance NFData CreateAssociationBatchRequestEntry

instance ToJSON CreateAssociationBatchRequestEntry where
  toJSON CreateAssociationBatchRequestEntry' {..} =
    object
      ( catMaybes
          [ ("InstanceId" .=) <$> _cabreInstanceId,
            ("ApplyOnlyAtCronInterval" .=) <$> _cabreApplyOnlyAtCronInterval,
            ("MaxErrors" .=) <$> _cabreMaxErrors,
            ("ScheduleExpression" .=) <$> _cabreScheduleExpression,
            ("OutputLocation" .=) <$> _cabreOutputLocation,
            ("SyncCompliance" .=) <$> _cabreSyncCompliance,
            ("Targets" .=) <$> _cabreTargets,
            ("Parameters" .=) <$> _cabreParameters,
            ("DocumentVersion" .=) <$> _cabreDocumentVersion,
            ("AutomationTargetParameterName" .=)
              <$> _cabreAutomationTargetParameterName,
            ("AssociationName" .=) <$> _cabreAssociationName,
            ("ComplianceSeverity" .=) <$> _cabreComplianceSeverity,
            ("MaxConcurrency" .=) <$> _cabreMaxConcurrency,
            Just ("Name" .= _cabreName)
          ]
      )
