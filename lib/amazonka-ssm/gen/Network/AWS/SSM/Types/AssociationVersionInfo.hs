{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.AssociationVersionInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.AssociationVersionInfo where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SSM.Types.AssociationComplianceSeverity
import Network.AWS.SSM.Types.AssociationSyncCompliance
import Network.AWS.SSM.Types.InstanceAssociationOutputLocation
import Network.AWS.SSM.Types.Target

-- | Information about the association version.
--
--
--
-- /See:/ 'associationVersionInfo' smart constructor.
data AssociationVersionInfo = AssociationVersionInfo'
  { _aviAssociationId ::
      !(Maybe Text),
    _aviApplyOnlyAtCronInterval :: !(Maybe Bool),
    _aviCreatedDate :: !(Maybe POSIX),
    _aviMaxErrors :: !(Maybe Text),
    _aviScheduleExpression :: !(Maybe Text),
    _aviName :: !(Maybe Text),
    _aviOutputLocation ::
      !(Maybe InstanceAssociationOutputLocation),
    _aviSyncCompliance ::
      !(Maybe AssociationSyncCompliance),
    _aviTargets :: !(Maybe [Target]),
    _aviParameters ::
      !(Maybe (Map Text ([Text]))),
    _aviDocumentVersion :: !(Maybe Text),
    _aviAssociationVersion :: !(Maybe Text),
    _aviAssociationName :: !(Maybe Text),
    _aviComplianceSeverity ::
      !(Maybe AssociationComplianceSeverity),
    _aviMaxConcurrency :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AssociationVersionInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aviAssociationId' - The ID created by the system when the association was created.
--
-- * 'aviApplyOnlyAtCronInterval' - By default, when you create a new associations, the system runs it immediately after it is created and then according to the schedule you specified. Specify this option if you don't want an association to run immediately after you create it.
--
-- * 'aviCreatedDate' - The date the association version was created.
--
-- * 'aviMaxErrors' - The number of errors that are allowed before the system stops sending requests to run the association on additional targets. You can specify either an absolute number of errors, for example 10, or a percentage of the target set, for example 10%. If you specify 3, for example, the system stops sending requests when the fourth error is received. If you specify 0, then the system stops sending requests after the first error is returned. If you run an association on 50 instances and set MaxError to 10%, then the system stops sending the request when the sixth error is received. Executions that are already running an association when MaxErrors is reached are allowed to complete, but some of these executions may fail as well. If you need to ensure that there won't be more than max-errors failed executions, set MaxConcurrency to 1 so that executions proceed one at a time.
--
-- * 'aviScheduleExpression' - The cron or rate schedule specified for the association when the association version was created.
--
-- * 'aviName' - The name specified when the association was created.
--
-- * 'aviOutputLocation' - The location in Amazon S3 specified for the association when the association version was created.
--
-- * 'aviSyncCompliance' - The mode for generating association compliance. You can specify @AUTO@ or @MANUAL@ . In @AUTO@ mode, the system uses the status of the association execution to determine the compliance status. If the association execution runs successfully, then the association is @COMPLIANT@ . If the association execution doesn't run successfully, the association is @NON-COMPLIANT@ . In @MANUAL@ mode, you must specify the @AssociationId@ as a parameter for the 'PutComplianceItems' API action. In this case, compliance data is not managed by State Manager. It is managed by your direct call to the 'PutComplianceItems' API action. By default, all associations use @AUTO@ mode.
--
-- * 'aviTargets' - The targets specified for the association when the association version was created.
--
-- * 'aviParameters' - Parameters specified when the association version was created.
--
-- * 'aviDocumentVersion' - The version of a Systems Manager document used when the association version was created.
--
-- * 'aviAssociationVersion' - The association version.
--
-- * 'aviAssociationName' - The name specified for the association version when the association version was created.
--
-- * 'aviComplianceSeverity' - The severity level that is assigned to the association.
--
-- * 'aviMaxConcurrency' - The maximum number of targets allowed to run the association at the same time. You can specify a number, for example 10, or a percentage of the target set, for example 10%. The default value is 100%, which means all targets run the association at the same time. If a new instance starts and attempts to run an association while Systems Manager is running MaxConcurrency associations, the association is allowed to run. During the next association interval, the new instance will process its association within the limit specified for MaxConcurrency.
associationVersionInfo ::
  AssociationVersionInfo
associationVersionInfo =
  AssociationVersionInfo'
    { _aviAssociationId = Nothing,
      _aviApplyOnlyAtCronInterval = Nothing,
      _aviCreatedDate = Nothing,
      _aviMaxErrors = Nothing,
      _aviScheduleExpression = Nothing,
      _aviName = Nothing,
      _aviOutputLocation = Nothing,
      _aviSyncCompliance = Nothing,
      _aviTargets = Nothing,
      _aviParameters = Nothing,
      _aviDocumentVersion = Nothing,
      _aviAssociationVersion = Nothing,
      _aviAssociationName = Nothing,
      _aviComplianceSeverity = Nothing,
      _aviMaxConcurrency = Nothing
    }

-- | The ID created by the system when the association was created.
aviAssociationId :: Lens' AssociationVersionInfo (Maybe Text)
aviAssociationId = lens _aviAssociationId (\s a -> s {_aviAssociationId = a})

-- | By default, when you create a new associations, the system runs it immediately after it is created and then according to the schedule you specified. Specify this option if you don't want an association to run immediately after you create it.
aviApplyOnlyAtCronInterval :: Lens' AssociationVersionInfo (Maybe Bool)
aviApplyOnlyAtCronInterval = lens _aviApplyOnlyAtCronInterval (\s a -> s {_aviApplyOnlyAtCronInterval = a})

-- | The date the association version was created.
aviCreatedDate :: Lens' AssociationVersionInfo (Maybe UTCTime)
aviCreatedDate = lens _aviCreatedDate (\s a -> s {_aviCreatedDate = a}) . mapping _Time

-- | The number of errors that are allowed before the system stops sending requests to run the association on additional targets. You can specify either an absolute number of errors, for example 10, or a percentage of the target set, for example 10%. If you specify 3, for example, the system stops sending requests when the fourth error is received. If you specify 0, then the system stops sending requests after the first error is returned. If you run an association on 50 instances and set MaxError to 10%, then the system stops sending the request when the sixth error is received. Executions that are already running an association when MaxErrors is reached are allowed to complete, but some of these executions may fail as well. If you need to ensure that there won't be more than max-errors failed executions, set MaxConcurrency to 1 so that executions proceed one at a time.
aviMaxErrors :: Lens' AssociationVersionInfo (Maybe Text)
aviMaxErrors = lens _aviMaxErrors (\s a -> s {_aviMaxErrors = a})

-- | The cron or rate schedule specified for the association when the association version was created.
aviScheduleExpression :: Lens' AssociationVersionInfo (Maybe Text)
aviScheduleExpression = lens _aviScheduleExpression (\s a -> s {_aviScheduleExpression = a})

-- | The name specified when the association was created.
aviName :: Lens' AssociationVersionInfo (Maybe Text)
aviName = lens _aviName (\s a -> s {_aviName = a})

-- | The location in Amazon S3 specified for the association when the association version was created.
aviOutputLocation :: Lens' AssociationVersionInfo (Maybe InstanceAssociationOutputLocation)
aviOutputLocation = lens _aviOutputLocation (\s a -> s {_aviOutputLocation = a})

-- | The mode for generating association compliance. You can specify @AUTO@ or @MANUAL@ . In @AUTO@ mode, the system uses the status of the association execution to determine the compliance status. If the association execution runs successfully, then the association is @COMPLIANT@ . If the association execution doesn't run successfully, the association is @NON-COMPLIANT@ . In @MANUAL@ mode, you must specify the @AssociationId@ as a parameter for the 'PutComplianceItems' API action. In this case, compliance data is not managed by State Manager. It is managed by your direct call to the 'PutComplianceItems' API action. By default, all associations use @AUTO@ mode.
aviSyncCompliance :: Lens' AssociationVersionInfo (Maybe AssociationSyncCompliance)
aviSyncCompliance = lens _aviSyncCompliance (\s a -> s {_aviSyncCompliance = a})

-- | The targets specified for the association when the association version was created.
aviTargets :: Lens' AssociationVersionInfo [Target]
aviTargets = lens _aviTargets (\s a -> s {_aviTargets = a}) . _Default . _Coerce

-- | Parameters specified when the association version was created.
aviParameters :: Lens' AssociationVersionInfo (HashMap Text ([Text]))
aviParameters = lens _aviParameters (\s a -> s {_aviParameters = a}) . _Default . _Map

-- | The version of a Systems Manager document used when the association version was created.
aviDocumentVersion :: Lens' AssociationVersionInfo (Maybe Text)
aviDocumentVersion = lens _aviDocumentVersion (\s a -> s {_aviDocumentVersion = a})

-- | The association version.
aviAssociationVersion :: Lens' AssociationVersionInfo (Maybe Text)
aviAssociationVersion = lens _aviAssociationVersion (\s a -> s {_aviAssociationVersion = a})

-- | The name specified for the association version when the association version was created.
aviAssociationName :: Lens' AssociationVersionInfo (Maybe Text)
aviAssociationName = lens _aviAssociationName (\s a -> s {_aviAssociationName = a})

-- | The severity level that is assigned to the association.
aviComplianceSeverity :: Lens' AssociationVersionInfo (Maybe AssociationComplianceSeverity)
aviComplianceSeverity = lens _aviComplianceSeverity (\s a -> s {_aviComplianceSeverity = a})

-- | The maximum number of targets allowed to run the association at the same time. You can specify a number, for example 10, or a percentage of the target set, for example 10%. The default value is 100%, which means all targets run the association at the same time. If a new instance starts and attempts to run an association while Systems Manager is running MaxConcurrency associations, the association is allowed to run. During the next association interval, the new instance will process its association within the limit specified for MaxConcurrency.
aviMaxConcurrency :: Lens' AssociationVersionInfo (Maybe Text)
aviMaxConcurrency = lens _aviMaxConcurrency (\s a -> s {_aviMaxConcurrency = a})

instance FromJSON AssociationVersionInfo where
  parseJSON =
    withObject
      "AssociationVersionInfo"
      ( \x ->
          AssociationVersionInfo'
            <$> (x .:? "AssociationId")
            <*> (x .:? "ApplyOnlyAtCronInterval")
            <*> (x .:? "CreatedDate")
            <*> (x .:? "MaxErrors")
            <*> (x .:? "ScheduleExpression")
            <*> (x .:? "Name")
            <*> (x .:? "OutputLocation")
            <*> (x .:? "SyncCompliance")
            <*> (x .:? "Targets" .!= mempty)
            <*> (x .:? "Parameters" .!= mempty)
            <*> (x .:? "DocumentVersion")
            <*> (x .:? "AssociationVersion")
            <*> (x .:? "AssociationName")
            <*> (x .:? "ComplianceSeverity")
            <*> (x .:? "MaxConcurrency")
      )

instance Hashable AssociationVersionInfo

instance NFData AssociationVersionInfo
