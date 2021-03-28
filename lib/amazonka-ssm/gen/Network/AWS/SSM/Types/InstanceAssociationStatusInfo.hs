{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.InstanceAssociationStatusInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.InstanceAssociationStatusInfo
  ( InstanceAssociationStatusInfo (..)
  -- * Smart constructor
  , mkInstanceAssociationStatusInfo
  -- * Lenses
  , iasiAssociationId
  , iasiAssociationName
  , iasiAssociationVersion
  , iasiDetailedStatus
  , iasiDocumentVersion
  , iasiErrorCode
  , iasiExecutionDate
  , iasiExecutionSummary
  , iasiInstanceId
  , iasiName
  , iasiOutputUrl
  , iasiStatus
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.AgentErrorCode as Types
import qualified Network.AWS.SSM.Types.AssociationId as Types
import qualified Network.AWS.SSM.Types.AssociationName as Types
import qualified Network.AWS.SSM.Types.AssociationVersion as Types
import qualified Network.AWS.SSM.Types.DetailedStatus as Types
import qualified Network.AWS.SSM.Types.DocumentARN as Types
import qualified Network.AWS.SSM.Types.DocumentVersion as Types
import qualified Network.AWS.SSM.Types.ExecutionSummary as Types
import qualified Network.AWS.SSM.Types.InstanceAssociationOutputUrl as Types
import qualified Network.AWS.SSM.Types.InstanceId as Types
import qualified Network.AWS.SSM.Types.Status as Types

-- | Status information about the instance association.
--
-- /See:/ 'mkInstanceAssociationStatusInfo' smart constructor.
data InstanceAssociationStatusInfo = InstanceAssociationStatusInfo'
  { associationId :: Core.Maybe Types.AssociationId
    -- ^ The association ID.
  , associationName :: Core.Maybe Types.AssociationName
    -- ^ The name of the association applied to the instance.
  , associationVersion :: Core.Maybe Types.AssociationVersion
    -- ^ The version of the association applied to the instance.
  , detailedStatus :: Core.Maybe Types.DetailedStatus
    -- ^ Detailed status information about the instance association.
  , documentVersion :: Core.Maybe Types.DocumentVersion
    -- ^ The association document versions.
  , errorCode :: Core.Maybe Types.AgentErrorCode
    -- ^ An error code returned by the request to create the association.
  , executionDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date the instance association ran. 
  , executionSummary :: Core.Maybe Types.ExecutionSummary
    -- ^ Summary information about association execution.
  , instanceId :: Core.Maybe Types.InstanceId
    -- ^ The instance ID where the association was created.
  , name :: Core.Maybe Types.DocumentARN
    -- ^ The name of the association.
  , outputUrl :: Core.Maybe Types.InstanceAssociationOutputUrl
    -- ^ A URL for an S3 bucket where you want to store the results of this request.
  , status :: Core.Maybe Types.Status
    -- ^ Status information about the instance association.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'InstanceAssociationStatusInfo' value with any optional fields omitted.
mkInstanceAssociationStatusInfo
    :: InstanceAssociationStatusInfo
mkInstanceAssociationStatusInfo
  = InstanceAssociationStatusInfo'{associationId = Core.Nothing,
                                   associationName = Core.Nothing,
                                   associationVersion = Core.Nothing, detailedStatus = Core.Nothing,
                                   documentVersion = Core.Nothing, errorCode = Core.Nothing,
                                   executionDate = Core.Nothing, executionSummary = Core.Nothing,
                                   instanceId = Core.Nothing, name = Core.Nothing,
                                   outputUrl = Core.Nothing, status = Core.Nothing}

-- | The association ID.
--
-- /Note:/ Consider using 'associationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iasiAssociationId :: Lens.Lens' InstanceAssociationStatusInfo (Core.Maybe Types.AssociationId)
iasiAssociationId = Lens.field @"associationId"
{-# INLINEABLE iasiAssociationId #-}
{-# DEPRECATED associationId "Use generic-lens or generic-optics with 'associationId' instead"  #-}

-- | The name of the association applied to the instance.
--
-- /Note:/ Consider using 'associationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iasiAssociationName :: Lens.Lens' InstanceAssociationStatusInfo (Core.Maybe Types.AssociationName)
iasiAssociationName = Lens.field @"associationName"
{-# INLINEABLE iasiAssociationName #-}
{-# DEPRECATED associationName "Use generic-lens or generic-optics with 'associationName' instead"  #-}

-- | The version of the association applied to the instance.
--
-- /Note:/ Consider using 'associationVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iasiAssociationVersion :: Lens.Lens' InstanceAssociationStatusInfo (Core.Maybe Types.AssociationVersion)
iasiAssociationVersion = Lens.field @"associationVersion"
{-# INLINEABLE iasiAssociationVersion #-}
{-# DEPRECATED associationVersion "Use generic-lens or generic-optics with 'associationVersion' instead"  #-}

-- | Detailed status information about the instance association.
--
-- /Note:/ Consider using 'detailedStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iasiDetailedStatus :: Lens.Lens' InstanceAssociationStatusInfo (Core.Maybe Types.DetailedStatus)
iasiDetailedStatus = Lens.field @"detailedStatus"
{-# INLINEABLE iasiDetailedStatus #-}
{-# DEPRECATED detailedStatus "Use generic-lens or generic-optics with 'detailedStatus' instead"  #-}

-- | The association document versions.
--
-- /Note:/ Consider using 'documentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iasiDocumentVersion :: Lens.Lens' InstanceAssociationStatusInfo (Core.Maybe Types.DocumentVersion)
iasiDocumentVersion = Lens.field @"documentVersion"
{-# INLINEABLE iasiDocumentVersion #-}
{-# DEPRECATED documentVersion "Use generic-lens or generic-optics with 'documentVersion' instead"  #-}

-- | An error code returned by the request to create the association.
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iasiErrorCode :: Lens.Lens' InstanceAssociationStatusInfo (Core.Maybe Types.AgentErrorCode)
iasiErrorCode = Lens.field @"errorCode"
{-# INLINEABLE iasiErrorCode #-}
{-# DEPRECATED errorCode "Use generic-lens or generic-optics with 'errorCode' instead"  #-}

-- | The date the instance association ran. 
--
-- /Note:/ Consider using 'executionDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iasiExecutionDate :: Lens.Lens' InstanceAssociationStatusInfo (Core.Maybe Core.NominalDiffTime)
iasiExecutionDate = Lens.field @"executionDate"
{-# INLINEABLE iasiExecutionDate #-}
{-# DEPRECATED executionDate "Use generic-lens or generic-optics with 'executionDate' instead"  #-}

-- | Summary information about association execution.
--
-- /Note:/ Consider using 'executionSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iasiExecutionSummary :: Lens.Lens' InstanceAssociationStatusInfo (Core.Maybe Types.ExecutionSummary)
iasiExecutionSummary = Lens.field @"executionSummary"
{-# INLINEABLE iasiExecutionSummary #-}
{-# DEPRECATED executionSummary "Use generic-lens or generic-optics with 'executionSummary' instead"  #-}

-- | The instance ID where the association was created.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iasiInstanceId :: Lens.Lens' InstanceAssociationStatusInfo (Core.Maybe Types.InstanceId)
iasiInstanceId = Lens.field @"instanceId"
{-# INLINEABLE iasiInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

-- | The name of the association.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iasiName :: Lens.Lens' InstanceAssociationStatusInfo (Core.Maybe Types.DocumentARN)
iasiName = Lens.field @"name"
{-# INLINEABLE iasiName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | A URL for an S3 bucket where you want to store the results of this request.
--
-- /Note:/ Consider using 'outputUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iasiOutputUrl :: Lens.Lens' InstanceAssociationStatusInfo (Core.Maybe Types.InstanceAssociationOutputUrl)
iasiOutputUrl = Lens.field @"outputUrl"
{-# INLINEABLE iasiOutputUrl #-}
{-# DEPRECATED outputUrl "Use generic-lens or generic-optics with 'outputUrl' instead"  #-}

-- | Status information about the instance association.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iasiStatus :: Lens.Lens' InstanceAssociationStatusInfo (Core.Maybe Types.Status)
iasiStatus = Lens.field @"status"
{-# INLINEABLE iasiStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

instance Core.FromJSON InstanceAssociationStatusInfo where
        parseJSON
          = Core.withObject "InstanceAssociationStatusInfo" Core.$
              \ x ->
                InstanceAssociationStatusInfo' Core.<$>
                  (x Core..:? "AssociationId") Core.<*> x Core..:? "AssociationName"
                    Core.<*> x Core..:? "AssociationVersion"
                    Core.<*> x Core..:? "DetailedStatus"
                    Core.<*> x Core..:? "DocumentVersion"
                    Core.<*> x Core..:? "ErrorCode"
                    Core.<*> x Core..:? "ExecutionDate"
                    Core.<*> x Core..:? "ExecutionSummary"
                    Core.<*> x Core..:? "InstanceId"
                    Core.<*> x Core..:? "Name"
                    Core.<*> x Core..:? "OutputUrl"
                    Core.<*> x Core..:? "Status"
