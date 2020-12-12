{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.AssociationVersionInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.AssociationVersionInfo
  ( AssociationVersionInfo (..),

    -- * Smart constructor
    mkAssociationVersionInfo,

    -- * Lenses
    aviAssociationId,
    aviApplyOnlyAtCronInterval,
    aviCreatedDate,
    aviMaxErrors,
    aviScheduleExpression,
    aviName,
    aviOutputLocation,
    aviSyncCompliance,
    aviTargets,
    aviParameters,
    aviDocumentVersion,
    aviAssociationVersion,
    aviAssociationName,
    aviComplianceSeverity,
    aviMaxConcurrency,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SSM.Types.AssociationComplianceSeverity
import Network.AWS.SSM.Types.AssociationSyncCompliance
import Network.AWS.SSM.Types.InstanceAssociationOutputLocation
import Network.AWS.SSM.Types.Target

-- | Information about the association version.
--
-- /See:/ 'mkAssociationVersionInfo' smart constructor.
data AssociationVersionInfo = AssociationVersionInfo'
  { associationId ::
      Lude.Maybe Lude.Text,
    applyOnlyAtCronInterval ::
      Lude.Maybe Lude.Bool,
    createdDate :: Lude.Maybe Lude.Timestamp,
    maxErrors :: Lude.Maybe Lude.Text,
    scheduleExpression :: Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text,
    outputLocation ::
      Lude.Maybe InstanceAssociationOutputLocation,
    syncCompliance ::
      Lude.Maybe AssociationSyncCompliance,
    targets :: Lude.Maybe [Target],
    parameters ::
      Lude.Maybe
        (Lude.HashMap Lude.Text ([Lude.Text])),
    documentVersion :: Lude.Maybe Lude.Text,
    associationVersion :: Lude.Maybe Lude.Text,
    associationName :: Lude.Maybe Lude.Text,
    complianceSeverity ::
      Lude.Maybe AssociationComplianceSeverity,
    maxConcurrency :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociationVersionInfo' with the minimum fields required to make a request.
--
-- * 'applyOnlyAtCronInterval' - By default, when you create a new associations, the system runs it immediately after it is created and then according to the schedule you specified. Specify this option if you don't want an association to run immediately after you create it.
-- * 'associationId' - The ID created by the system when the association was created.
-- * 'associationName' - The name specified for the association version when the association version was created.
-- * 'associationVersion' - The association version.
-- * 'complianceSeverity' - The severity level that is assigned to the association.
-- * 'createdDate' - The date the association version was created.
-- * 'documentVersion' - The version of a Systems Manager document used when the association version was created.
-- * 'maxConcurrency' - The maximum number of targets allowed to run the association at the same time. You can specify a number, for example 10, or a percentage of the target set, for example 10%. The default value is 100%, which means all targets run the association at the same time.
--
-- If a new instance starts and attempts to run an association while Systems Manager is running MaxConcurrency associations, the association is allowed to run. During the next association interval, the new instance will process its association within the limit specified for MaxConcurrency.
-- * 'maxErrors' - The number of errors that are allowed before the system stops sending requests to run the association on additional targets. You can specify either an absolute number of errors, for example 10, or a percentage of the target set, for example 10%. If you specify 3, for example, the system stops sending requests when the fourth error is received. If you specify 0, then the system stops sending requests after the first error is returned. If you run an association on 50 instances and set MaxError to 10%, then the system stops sending the request when the sixth error is received.
--
-- Executions that are already running an association when MaxErrors is reached are allowed to complete, but some of these executions may fail as well. If you need to ensure that there won't be more than max-errors failed executions, set MaxConcurrency to 1 so that executions proceed one at a time.
-- * 'name' - The name specified when the association was created.
-- * 'outputLocation' - The location in Amazon S3 specified for the association when the association version was created.
-- * 'parameters' - Parameters specified when the association version was created.
-- * 'scheduleExpression' - The cron or rate schedule specified for the association when the association version was created.
-- * 'syncCompliance' - The mode for generating association compliance. You can specify @AUTO@ or @MANUAL@ . In @AUTO@ mode, the system uses the status of the association execution to determine the compliance status. If the association execution runs successfully, then the association is @COMPLIANT@ . If the association execution doesn't run successfully, the association is @NON-COMPLIANT@ .
--
-- In @MANUAL@ mode, you must specify the @AssociationId@ as a parameter for the 'PutComplianceItems' API action. In this case, compliance data is not managed by State Manager. It is managed by your direct call to the 'PutComplianceItems' API action.
-- By default, all associations use @AUTO@ mode.
-- * 'targets' - The targets specified for the association when the association version was created.
mkAssociationVersionInfo ::
  AssociationVersionInfo
mkAssociationVersionInfo =
  AssociationVersionInfo'
    { associationId = Lude.Nothing,
      applyOnlyAtCronInterval = Lude.Nothing,
      createdDate = Lude.Nothing,
      maxErrors = Lude.Nothing,
      scheduleExpression = Lude.Nothing,
      name = Lude.Nothing,
      outputLocation = Lude.Nothing,
      syncCompliance = Lude.Nothing,
      targets = Lude.Nothing,
      parameters = Lude.Nothing,
      documentVersion = Lude.Nothing,
      associationVersion = Lude.Nothing,
      associationName = Lude.Nothing,
      complianceSeverity = Lude.Nothing,
      maxConcurrency = Lude.Nothing
    }

-- | The ID created by the system when the association was created.
--
-- /Note:/ Consider using 'associationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aviAssociationId :: Lens.Lens' AssociationVersionInfo (Lude.Maybe Lude.Text)
aviAssociationId = Lens.lens (associationId :: AssociationVersionInfo -> Lude.Maybe Lude.Text) (\s a -> s {associationId = a} :: AssociationVersionInfo)
{-# DEPRECATED aviAssociationId "Use generic-lens or generic-optics with 'associationId' instead." #-}

-- | By default, when you create a new associations, the system runs it immediately after it is created and then according to the schedule you specified. Specify this option if you don't want an association to run immediately after you create it.
--
-- /Note:/ Consider using 'applyOnlyAtCronInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aviApplyOnlyAtCronInterval :: Lens.Lens' AssociationVersionInfo (Lude.Maybe Lude.Bool)
aviApplyOnlyAtCronInterval = Lens.lens (applyOnlyAtCronInterval :: AssociationVersionInfo -> Lude.Maybe Lude.Bool) (\s a -> s {applyOnlyAtCronInterval = a} :: AssociationVersionInfo)
{-# DEPRECATED aviApplyOnlyAtCronInterval "Use generic-lens or generic-optics with 'applyOnlyAtCronInterval' instead." #-}

-- | The date the association version was created.
--
-- /Note:/ Consider using 'createdDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aviCreatedDate :: Lens.Lens' AssociationVersionInfo (Lude.Maybe Lude.Timestamp)
aviCreatedDate = Lens.lens (createdDate :: AssociationVersionInfo -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdDate = a} :: AssociationVersionInfo)
{-# DEPRECATED aviCreatedDate "Use generic-lens or generic-optics with 'createdDate' instead." #-}

-- | The number of errors that are allowed before the system stops sending requests to run the association on additional targets. You can specify either an absolute number of errors, for example 10, or a percentage of the target set, for example 10%. If you specify 3, for example, the system stops sending requests when the fourth error is received. If you specify 0, then the system stops sending requests after the first error is returned. If you run an association on 50 instances and set MaxError to 10%, then the system stops sending the request when the sixth error is received.
--
-- Executions that are already running an association when MaxErrors is reached are allowed to complete, but some of these executions may fail as well. If you need to ensure that there won't be more than max-errors failed executions, set MaxConcurrency to 1 so that executions proceed one at a time.
--
-- /Note:/ Consider using 'maxErrors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aviMaxErrors :: Lens.Lens' AssociationVersionInfo (Lude.Maybe Lude.Text)
aviMaxErrors = Lens.lens (maxErrors :: AssociationVersionInfo -> Lude.Maybe Lude.Text) (\s a -> s {maxErrors = a} :: AssociationVersionInfo)
{-# DEPRECATED aviMaxErrors "Use generic-lens or generic-optics with 'maxErrors' instead." #-}

-- | The cron or rate schedule specified for the association when the association version was created.
--
-- /Note:/ Consider using 'scheduleExpression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aviScheduleExpression :: Lens.Lens' AssociationVersionInfo (Lude.Maybe Lude.Text)
aviScheduleExpression = Lens.lens (scheduleExpression :: AssociationVersionInfo -> Lude.Maybe Lude.Text) (\s a -> s {scheduleExpression = a} :: AssociationVersionInfo)
{-# DEPRECATED aviScheduleExpression "Use generic-lens or generic-optics with 'scheduleExpression' instead." #-}

-- | The name specified when the association was created.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aviName :: Lens.Lens' AssociationVersionInfo (Lude.Maybe Lude.Text)
aviName = Lens.lens (name :: AssociationVersionInfo -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: AssociationVersionInfo)
{-# DEPRECATED aviName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The location in Amazon S3 specified for the association when the association version was created.
--
-- /Note:/ Consider using 'outputLocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aviOutputLocation :: Lens.Lens' AssociationVersionInfo (Lude.Maybe InstanceAssociationOutputLocation)
aviOutputLocation = Lens.lens (outputLocation :: AssociationVersionInfo -> Lude.Maybe InstanceAssociationOutputLocation) (\s a -> s {outputLocation = a} :: AssociationVersionInfo)
{-# DEPRECATED aviOutputLocation "Use generic-lens or generic-optics with 'outputLocation' instead." #-}

-- | The mode for generating association compliance. You can specify @AUTO@ or @MANUAL@ . In @AUTO@ mode, the system uses the status of the association execution to determine the compliance status. If the association execution runs successfully, then the association is @COMPLIANT@ . If the association execution doesn't run successfully, the association is @NON-COMPLIANT@ .
--
-- In @MANUAL@ mode, you must specify the @AssociationId@ as a parameter for the 'PutComplianceItems' API action. In this case, compliance data is not managed by State Manager. It is managed by your direct call to the 'PutComplianceItems' API action.
-- By default, all associations use @AUTO@ mode.
--
-- /Note:/ Consider using 'syncCompliance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aviSyncCompliance :: Lens.Lens' AssociationVersionInfo (Lude.Maybe AssociationSyncCompliance)
aviSyncCompliance = Lens.lens (syncCompliance :: AssociationVersionInfo -> Lude.Maybe AssociationSyncCompliance) (\s a -> s {syncCompliance = a} :: AssociationVersionInfo)
{-# DEPRECATED aviSyncCompliance "Use generic-lens or generic-optics with 'syncCompliance' instead." #-}

-- | The targets specified for the association when the association version was created.
--
-- /Note:/ Consider using 'targets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aviTargets :: Lens.Lens' AssociationVersionInfo (Lude.Maybe [Target])
aviTargets = Lens.lens (targets :: AssociationVersionInfo -> Lude.Maybe [Target]) (\s a -> s {targets = a} :: AssociationVersionInfo)
{-# DEPRECATED aviTargets "Use generic-lens or generic-optics with 'targets' instead." #-}

-- | Parameters specified when the association version was created.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aviParameters :: Lens.Lens' AssociationVersionInfo (Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text])))
aviParameters = Lens.lens (parameters :: AssociationVersionInfo -> Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text]))) (\s a -> s {parameters = a} :: AssociationVersionInfo)
{-# DEPRECATED aviParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | The version of a Systems Manager document used when the association version was created.
--
-- /Note:/ Consider using 'documentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aviDocumentVersion :: Lens.Lens' AssociationVersionInfo (Lude.Maybe Lude.Text)
aviDocumentVersion = Lens.lens (documentVersion :: AssociationVersionInfo -> Lude.Maybe Lude.Text) (\s a -> s {documentVersion = a} :: AssociationVersionInfo)
{-# DEPRECATED aviDocumentVersion "Use generic-lens or generic-optics with 'documentVersion' instead." #-}

-- | The association version.
--
-- /Note:/ Consider using 'associationVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aviAssociationVersion :: Lens.Lens' AssociationVersionInfo (Lude.Maybe Lude.Text)
aviAssociationVersion = Lens.lens (associationVersion :: AssociationVersionInfo -> Lude.Maybe Lude.Text) (\s a -> s {associationVersion = a} :: AssociationVersionInfo)
{-# DEPRECATED aviAssociationVersion "Use generic-lens or generic-optics with 'associationVersion' instead." #-}

-- | The name specified for the association version when the association version was created.
--
-- /Note:/ Consider using 'associationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aviAssociationName :: Lens.Lens' AssociationVersionInfo (Lude.Maybe Lude.Text)
aviAssociationName = Lens.lens (associationName :: AssociationVersionInfo -> Lude.Maybe Lude.Text) (\s a -> s {associationName = a} :: AssociationVersionInfo)
{-# DEPRECATED aviAssociationName "Use generic-lens or generic-optics with 'associationName' instead." #-}

-- | The severity level that is assigned to the association.
--
-- /Note:/ Consider using 'complianceSeverity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aviComplianceSeverity :: Lens.Lens' AssociationVersionInfo (Lude.Maybe AssociationComplianceSeverity)
aviComplianceSeverity = Lens.lens (complianceSeverity :: AssociationVersionInfo -> Lude.Maybe AssociationComplianceSeverity) (\s a -> s {complianceSeverity = a} :: AssociationVersionInfo)
{-# DEPRECATED aviComplianceSeverity "Use generic-lens or generic-optics with 'complianceSeverity' instead." #-}

-- | The maximum number of targets allowed to run the association at the same time. You can specify a number, for example 10, or a percentage of the target set, for example 10%. The default value is 100%, which means all targets run the association at the same time.
--
-- If a new instance starts and attempts to run an association while Systems Manager is running MaxConcurrency associations, the association is allowed to run. During the next association interval, the new instance will process its association within the limit specified for MaxConcurrency.
--
-- /Note:/ Consider using 'maxConcurrency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aviMaxConcurrency :: Lens.Lens' AssociationVersionInfo (Lude.Maybe Lude.Text)
aviMaxConcurrency = Lens.lens (maxConcurrency :: AssociationVersionInfo -> Lude.Maybe Lude.Text) (\s a -> s {maxConcurrency = a} :: AssociationVersionInfo)
{-# DEPRECATED aviMaxConcurrency "Use generic-lens or generic-optics with 'maxConcurrency' instead." #-}

instance Lude.FromJSON AssociationVersionInfo where
  parseJSON =
    Lude.withObject
      "AssociationVersionInfo"
      ( \x ->
          AssociationVersionInfo'
            Lude.<$> (x Lude..:? "AssociationId")
            Lude.<*> (x Lude..:? "ApplyOnlyAtCronInterval")
            Lude.<*> (x Lude..:? "CreatedDate")
            Lude.<*> (x Lude..:? "MaxErrors")
            Lude.<*> (x Lude..:? "ScheduleExpression")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "OutputLocation")
            Lude.<*> (x Lude..:? "SyncCompliance")
            Lude.<*> (x Lude..:? "Targets" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Parameters" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "DocumentVersion")
            Lude.<*> (x Lude..:? "AssociationVersion")
            Lude.<*> (x Lude..:? "AssociationName")
            Lude.<*> (x Lude..:? "ComplianceSeverity")
            Lude.<*> (x Lude..:? "MaxConcurrency")
      )
