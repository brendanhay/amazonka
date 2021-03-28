{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.AssociationVersionInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.AssociationVersionInfo
  ( AssociationVersionInfo (..)
  -- * Smart constructor
  , mkAssociationVersionInfo
  -- * Lenses
  , aviApplyOnlyAtCronInterval
  , aviAssociationId
  , aviAssociationName
  , aviAssociationVersion
  , aviComplianceSeverity
  , aviCreatedDate
  , aviDocumentVersion
  , aviMaxConcurrency
  , aviMaxErrors
  , aviName
  , aviOutputLocation
  , aviParameters
  , aviScheduleExpression
  , aviSyncCompliance
  , aviTargets
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.AssociationComplianceSeverity as Types
import qualified Network.AWS.SSM.Types.AssociationId as Types
import qualified Network.AWS.SSM.Types.AssociationName as Types
import qualified Network.AWS.SSM.Types.AssociationSyncCompliance as Types
import qualified Network.AWS.SSM.Types.AssociationVersion as Types
import qualified Network.AWS.SSM.Types.DocumentARN as Types
import qualified Network.AWS.SSM.Types.DocumentVersion as Types
import qualified Network.AWS.SSM.Types.InstanceAssociationOutputLocation as Types
import qualified Network.AWS.SSM.Types.MaxConcurrency as Types
import qualified Network.AWS.SSM.Types.MaxErrors as Types
import qualified Network.AWS.SSM.Types.ParameterName as Types
import qualified Network.AWS.SSM.Types.ParameterValue as Types
import qualified Network.AWS.SSM.Types.ScheduleExpression as Types
import qualified Network.AWS.SSM.Types.Target as Types

-- | Information about the association version.
--
-- /See:/ 'mkAssociationVersionInfo' smart constructor.
data AssociationVersionInfo = AssociationVersionInfo'
  { applyOnlyAtCronInterval :: Core.Maybe Core.Bool
    -- ^ By default, when you create a new associations, the system runs it immediately after it is created and then according to the schedule you specified. Specify this option if you don't want an association to run immediately after you create it.
  , associationId :: Core.Maybe Types.AssociationId
    -- ^ The ID created by the system when the association was created.
  , associationName :: Core.Maybe Types.AssociationName
    -- ^ The name specified for the association version when the association version was created.
  , associationVersion :: Core.Maybe Types.AssociationVersion
    -- ^ The association version.
  , complianceSeverity :: Core.Maybe Types.AssociationComplianceSeverity
    -- ^ The severity level that is assigned to the association.
  , createdDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date the association version was created.
  , documentVersion :: Core.Maybe Types.DocumentVersion
    -- ^ The version of a Systems Manager document used when the association version was created.
  , maxConcurrency :: Core.Maybe Types.MaxConcurrency
    -- ^ The maximum number of targets allowed to run the association at the same time. You can specify a number, for example 10, or a percentage of the target set, for example 10%. The default value is 100%, which means all targets run the association at the same time.
--
-- If a new instance starts and attempts to run an association while Systems Manager is running MaxConcurrency associations, the association is allowed to run. During the next association interval, the new instance will process its association within the limit specified for MaxConcurrency.
  , maxErrors :: Core.Maybe Types.MaxErrors
    -- ^ The number of errors that are allowed before the system stops sending requests to run the association on additional targets. You can specify either an absolute number of errors, for example 10, or a percentage of the target set, for example 10%. If you specify 3, for example, the system stops sending requests when the fourth error is received. If you specify 0, then the system stops sending requests after the first error is returned. If you run an association on 50 instances and set MaxError to 10%, then the system stops sending the request when the sixth error is received.
--
-- Executions that are already running an association when MaxErrors is reached are allowed to complete, but some of these executions may fail as well. If you need to ensure that there won't be more than max-errors failed executions, set MaxConcurrency to 1 so that executions proceed one at a time.
  , name :: Core.Maybe Types.DocumentARN
    -- ^ The name specified when the association was created.
  , outputLocation :: Core.Maybe Types.InstanceAssociationOutputLocation
    -- ^ The location in Amazon S3 specified for the association when the association version was created.
  , parameters :: Core.Maybe (Core.HashMap Types.ParameterName [Types.ParameterValue])
    -- ^ Parameters specified when the association version was created.
  , scheduleExpression :: Core.Maybe Types.ScheduleExpression
    -- ^ The cron or rate schedule specified for the association when the association version was created.
  , syncCompliance :: Core.Maybe Types.AssociationSyncCompliance
    -- ^ The mode for generating association compliance. You can specify @AUTO@ or @MANUAL@ . In @AUTO@ mode, the system uses the status of the association execution to determine the compliance status. If the association execution runs successfully, then the association is @COMPLIANT@ . If the association execution doesn't run successfully, the association is @NON-COMPLIANT@ .
--
-- In @MANUAL@ mode, you must specify the @AssociationId@ as a parameter for the 'PutComplianceItems' API action. In this case, compliance data is not managed by State Manager. It is managed by your direct call to the 'PutComplianceItems' API action.
-- By default, all associations use @AUTO@ mode.
  , targets :: Core.Maybe [Types.Target]
    -- ^ The targets specified for the association when the association version was created. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'AssociationVersionInfo' value with any optional fields omitted.
mkAssociationVersionInfo
    :: AssociationVersionInfo
mkAssociationVersionInfo
  = AssociationVersionInfo'{applyOnlyAtCronInterval = Core.Nothing,
                            associationId = Core.Nothing, associationName = Core.Nothing,
                            associationVersion = Core.Nothing,
                            complianceSeverity = Core.Nothing, createdDate = Core.Nothing,
                            documentVersion = Core.Nothing, maxConcurrency = Core.Nothing,
                            maxErrors = Core.Nothing, name = Core.Nothing,
                            outputLocation = Core.Nothing, parameters = Core.Nothing,
                            scheduleExpression = Core.Nothing, syncCompliance = Core.Nothing,
                            targets = Core.Nothing}

-- | By default, when you create a new associations, the system runs it immediately after it is created and then according to the schedule you specified. Specify this option if you don't want an association to run immediately after you create it.
--
-- /Note:/ Consider using 'applyOnlyAtCronInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aviApplyOnlyAtCronInterval :: Lens.Lens' AssociationVersionInfo (Core.Maybe Core.Bool)
aviApplyOnlyAtCronInterval = Lens.field @"applyOnlyAtCronInterval"
{-# INLINEABLE aviApplyOnlyAtCronInterval #-}
{-# DEPRECATED applyOnlyAtCronInterval "Use generic-lens or generic-optics with 'applyOnlyAtCronInterval' instead"  #-}

-- | The ID created by the system when the association was created.
--
-- /Note:/ Consider using 'associationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aviAssociationId :: Lens.Lens' AssociationVersionInfo (Core.Maybe Types.AssociationId)
aviAssociationId = Lens.field @"associationId"
{-# INLINEABLE aviAssociationId #-}
{-# DEPRECATED associationId "Use generic-lens or generic-optics with 'associationId' instead"  #-}

-- | The name specified for the association version when the association version was created.
--
-- /Note:/ Consider using 'associationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aviAssociationName :: Lens.Lens' AssociationVersionInfo (Core.Maybe Types.AssociationName)
aviAssociationName = Lens.field @"associationName"
{-# INLINEABLE aviAssociationName #-}
{-# DEPRECATED associationName "Use generic-lens or generic-optics with 'associationName' instead"  #-}

-- | The association version.
--
-- /Note:/ Consider using 'associationVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aviAssociationVersion :: Lens.Lens' AssociationVersionInfo (Core.Maybe Types.AssociationVersion)
aviAssociationVersion = Lens.field @"associationVersion"
{-# INLINEABLE aviAssociationVersion #-}
{-# DEPRECATED associationVersion "Use generic-lens or generic-optics with 'associationVersion' instead"  #-}

-- | The severity level that is assigned to the association.
--
-- /Note:/ Consider using 'complianceSeverity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aviComplianceSeverity :: Lens.Lens' AssociationVersionInfo (Core.Maybe Types.AssociationComplianceSeverity)
aviComplianceSeverity = Lens.field @"complianceSeverity"
{-# INLINEABLE aviComplianceSeverity #-}
{-# DEPRECATED complianceSeverity "Use generic-lens or generic-optics with 'complianceSeverity' instead"  #-}

-- | The date the association version was created.
--
-- /Note:/ Consider using 'createdDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aviCreatedDate :: Lens.Lens' AssociationVersionInfo (Core.Maybe Core.NominalDiffTime)
aviCreatedDate = Lens.field @"createdDate"
{-# INLINEABLE aviCreatedDate #-}
{-# DEPRECATED createdDate "Use generic-lens or generic-optics with 'createdDate' instead"  #-}

-- | The version of a Systems Manager document used when the association version was created.
--
-- /Note:/ Consider using 'documentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aviDocumentVersion :: Lens.Lens' AssociationVersionInfo (Core.Maybe Types.DocumentVersion)
aviDocumentVersion = Lens.field @"documentVersion"
{-# INLINEABLE aviDocumentVersion #-}
{-# DEPRECATED documentVersion "Use generic-lens or generic-optics with 'documentVersion' instead"  #-}

-- | The maximum number of targets allowed to run the association at the same time. You can specify a number, for example 10, or a percentage of the target set, for example 10%. The default value is 100%, which means all targets run the association at the same time.
--
-- If a new instance starts and attempts to run an association while Systems Manager is running MaxConcurrency associations, the association is allowed to run. During the next association interval, the new instance will process its association within the limit specified for MaxConcurrency.
--
-- /Note:/ Consider using 'maxConcurrency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aviMaxConcurrency :: Lens.Lens' AssociationVersionInfo (Core.Maybe Types.MaxConcurrency)
aviMaxConcurrency = Lens.field @"maxConcurrency"
{-# INLINEABLE aviMaxConcurrency #-}
{-# DEPRECATED maxConcurrency "Use generic-lens or generic-optics with 'maxConcurrency' instead"  #-}

-- | The number of errors that are allowed before the system stops sending requests to run the association on additional targets. You can specify either an absolute number of errors, for example 10, or a percentage of the target set, for example 10%. If you specify 3, for example, the system stops sending requests when the fourth error is received. If you specify 0, then the system stops sending requests after the first error is returned. If you run an association on 50 instances and set MaxError to 10%, then the system stops sending the request when the sixth error is received.
--
-- Executions that are already running an association when MaxErrors is reached are allowed to complete, but some of these executions may fail as well. If you need to ensure that there won't be more than max-errors failed executions, set MaxConcurrency to 1 so that executions proceed one at a time.
--
-- /Note:/ Consider using 'maxErrors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aviMaxErrors :: Lens.Lens' AssociationVersionInfo (Core.Maybe Types.MaxErrors)
aviMaxErrors = Lens.field @"maxErrors"
{-# INLINEABLE aviMaxErrors #-}
{-# DEPRECATED maxErrors "Use generic-lens or generic-optics with 'maxErrors' instead"  #-}

-- | The name specified when the association was created.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aviName :: Lens.Lens' AssociationVersionInfo (Core.Maybe Types.DocumentARN)
aviName = Lens.field @"name"
{-# INLINEABLE aviName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The location in Amazon S3 specified for the association when the association version was created.
--
-- /Note:/ Consider using 'outputLocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aviOutputLocation :: Lens.Lens' AssociationVersionInfo (Core.Maybe Types.InstanceAssociationOutputLocation)
aviOutputLocation = Lens.field @"outputLocation"
{-# INLINEABLE aviOutputLocation #-}
{-# DEPRECATED outputLocation "Use generic-lens or generic-optics with 'outputLocation' instead"  #-}

-- | Parameters specified when the association version was created.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aviParameters :: Lens.Lens' AssociationVersionInfo (Core.Maybe (Core.HashMap Types.ParameterName [Types.ParameterValue]))
aviParameters = Lens.field @"parameters"
{-# INLINEABLE aviParameters #-}
{-# DEPRECATED parameters "Use generic-lens or generic-optics with 'parameters' instead"  #-}

-- | The cron or rate schedule specified for the association when the association version was created.
--
-- /Note:/ Consider using 'scheduleExpression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aviScheduleExpression :: Lens.Lens' AssociationVersionInfo (Core.Maybe Types.ScheduleExpression)
aviScheduleExpression = Lens.field @"scheduleExpression"
{-# INLINEABLE aviScheduleExpression #-}
{-# DEPRECATED scheduleExpression "Use generic-lens or generic-optics with 'scheduleExpression' instead"  #-}

-- | The mode for generating association compliance. You can specify @AUTO@ or @MANUAL@ . In @AUTO@ mode, the system uses the status of the association execution to determine the compliance status. If the association execution runs successfully, then the association is @COMPLIANT@ . If the association execution doesn't run successfully, the association is @NON-COMPLIANT@ .
--
-- In @MANUAL@ mode, you must specify the @AssociationId@ as a parameter for the 'PutComplianceItems' API action. In this case, compliance data is not managed by State Manager. It is managed by your direct call to the 'PutComplianceItems' API action.
-- By default, all associations use @AUTO@ mode.
--
-- /Note:/ Consider using 'syncCompliance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aviSyncCompliance :: Lens.Lens' AssociationVersionInfo (Core.Maybe Types.AssociationSyncCompliance)
aviSyncCompliance = Lens.field @"syncCompliance"
{-# INLINEABLE aviSyncCompliance #-}
{-# DEPRECATED syncCompliance "Use generic-lens or generic-optics with 'syncCompliance' instead"  #-}

-- | The targets specified for the association when the association version was created. 
--
-- /Note:/ Consider using 'targets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aviTargets :: Lens.Lens' AssociationVersionInfo (Core.Maybe [Types.Target])
aviTargets = Lens.field @"targets"
{-# INLINEABLE aviTargets #-}
{-# DEPRECATED targets "Use generic-lens or generic-optics with 'targets' instead"  #-}

instance Core.FromJSON AssociationVersionInfo where
        parseJSON
          = Core.withObject "AssociationVersionInfo" Core.$
              \ x ->
                AssociationVersionInfo' Core.<$>
                  (x Core..:? "ApplyOnlyAtCronInterval") Core.<*>
                    x Core..:? "AssociationId"
                    Core.<*> x Core..:? "AssociationName"
                    Core.<*> x Core..:? "AssociationVersion"
                    Core.<*> x Core..:? "ComplianceSeverity"
                    Core.<*> x Core..:? "CreatedDate"
                    Core.<*> x Core..:? "DocumentVersion"
                    Core.<*> x Core..:? "MaxConcurrency"
                    Core.<*> x Core..:? "MaxErrors"
                    Core.<*> x Core..:? "Name"
                    Core.<*> x Core..:? "OutputLocation"
                    Core.<*> x Core..:? "Parameters"
                    Core.<*> x Core..:? "ScheduleExpression"
                    Core.<*> x Core..:? "SyncCompliance"
                    Core.<*> x Core..:? "Targets"
