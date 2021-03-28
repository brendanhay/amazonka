{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.Finding
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GuardDuty.Types.Finding
  ( Finding (..)
  -- * Smart constructor
  , mkFinding
  -- * Lenses
  , fAccountId
  , fArn
  , fCreatedAt
  , fId
  , fRegion
  , fResource
  , fSchemaVersion
  , fSeverity
  , fType
  , fUpdatedAt
  , fConfidence
  , fDescription
  , fPartition
  , fService
  , fTitle
  ) where

import qualified Network.AWS.GuardDuty.Types.Resource as Types
import qualified Network.AWS.GuardDuty.Types.ServiceInfo as Types
import qualified Network.AWS.GuardDuty.Types.Type as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about the finding, which is generated when abnormal or suspicious activity is detected.
--
-- /See:/ 'mkFinding' smart constructor.
data Finding = Finding'
  { accountId :: Core.Text
    -- ^ The ID of the account in which the finding was generated.
  , arn :: Core.Text
    -- ^ The ARN of the finding.
  , createdAt :: Core.Text
    -- ^ The time and date when the finding was created.
  , id :: Core.Text
    -- ^ The ID of the finding.
  , region :: Core.Text
    -- ^ The Region where the finding was generated.
  , resource :: Types.Resource
  , schemaVersion :: Core.Text
    -- ^ The version of the schema used for the finding.
  , severity :: Core.Double
    -- ^ The severity of the finding.
  , type' :: Types.Type
    -- ^ The type of finding.
  , updatedAt :: Core.Text
    -- ^ The time and date when the finding was last updated.
  , confidence :: Core.Maybe Core.Double
    -- ^ The confidence score for the finding.
  , description :: Core.Maybe Core.Text
    -- ^ The description of the finding.
  , partition :: Core.Maybe Core.Text
    -- ^ The partition associated with the finding.
  , service :: Core.Maybe Types.ServiceInfo
  , title :: Core.Maybe Core.Text
    -- ^ The title of the finding.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'Finding' value with any optional fields omitted.
mkFinding
    :: Core.Text -- ^ 'accountId'
    -> Core.Text -- ^ 'arn'
    -> Core.Text -- ^ 'createdAt'
    -> Core.Text -- ^ 'id'
    -> Core.Text -- ^ 'region'
    -> Types.Resource -- ^ 'resource'
    -> Core.Text -- ^ 'schemaVersion'
    -> Core.Double -- ^ 'severity'
    -> Types.Type -- ^ 'type\''
    -> Core.Text -- ^ 'updatedAt'
    -> Finding
mkFinding accountId arn createdAt id region resource schemaVersion
  severity type' updatedAt
  = Finding'{accountId, arn, createdAt, id, region, resource,
             schemaVersion, severity, type', updatedAt,
             confidence = Core.Nothing, description = Core.Nothing,
             partition = Core.Nothing, service = Core.Nothing,
             title = Core.Nothing}

-- | The ID of the account in which the finding was generated.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fAccountId :: Lens.Lens' Finding Core.Text
fAccountId = Lens.field @"accountId"
{-# INLINEABLE fAccountId #-}
{-# DEPRECATED accountId "Use generic-lens or generic-optics with 'accountId' instead"  #-}

-- | The ARN of the finding.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fArn :: Lens.Lens' Finding Core.Text
fArn = Lens.field @"arn"
{-# INLINEABLE fArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The time and date when the finding was created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fCreatedAt :: Lens.Lens' Finding Core.Text
fCreatedAt = Lens.field @"createdAt"
{-# INLINEABLE fCreatedAt #-}
{-# DEPRECATED createdAt "Use generic-lens or generic-optics with 'createdAt' instead"  #-}

-- | The ID of the finding.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fId :: Lens.Lens' Finding Core.Text
fId = Lens.field @"id"
{-# INLINEABLE fId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The Region where the finding was generated.
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fRegion :: Lens.Lens' Finding Core.Text
fRegion = Lens.field @"region"
{-# INLINEABLE fRegion #-}
{-# DEPRECATED region "Use generic-lens or generic-optics with 'region' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'resource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fResource :: Lens.Lens' Finding Types.Resource
fResource = Lens.field @"resource"
{-# INLINEABLE fResource #-}
{-# DEPRECATED resource "Use generic-lens or generic-optics with 'resource' instead"  #-}

-- | The version of the schema used for the finding.
--
-- /Note:/ Consider using 'schemaVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fSchemaVersion :: Lens.Lens' Finding Core.Text
fSchemaVersion = Lens.field @"schemaVersion"
{-# INLINEABLE fSchemaVersion #-}
{-# DEPRECATED schemaVersion "Use generic-lens or generic-optics with 'schemaVersion' instead"  #-}

-- | The severity of the finding.
--
-- /Note:/ Consider using 'severity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fSeverity :: Lens.Lens' Finding Core.Double
fSeverity = Lens.field @"severity"
{-# INLINEABLE fSeverity #-}
{-# DEPRECATED severity "Use generic-lens or generic-optics with 'severity' instead"  #-}

-- | The type of finding.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fType :: Lens.Lens' Finding Types.Type
fType = Lens.field @"type'"
{-# INLINEABLE fType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

-- | The time and date when the finding was last updated.
--
-- /Note:/ Consider using 'updatedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fUpdatedAt :: Lens.Lens' Finding Core.Text
fUpdatedAt = Lens.field @"updatedAt"
{-# INLINEABLE fUpdatedAt #-}
{-# DEPRECATED updatedAt "Use generic-lens or generic-optics with 'updatedAt' instead"  #-}

-- | The confidence score for the finding.
--
-- /Note:/ Consider using 'confidence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fConfidence :: Lens.Lens' Finding (Core.Maybe Core.Double)
fConfidence = Lens.field @"confidence"
{-# INLINEABLE fConfidence #-}
{-# DEPRECATED confidence "Use generic-lens or generic-optics with 'confidence' instead"  #-}

-- | The description of the finding.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fDescription :: Lens.Lens' Finding (Core.Maybe Core.Text)
fDescription = Lens.field @"description"
{-# INLINEABLE fDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The partition associated with the finding.
--
-- /Note:/ Consider using 'partition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fPartition :: Lens.Lens' Finding (Core.Maybe Core.Text)
fPartition = Lens.field @"partition"
{-# INLINEABLE fPartition #-}
{-# DEPRECATED partition "Use generic-lens or generic-optics with 'partition' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'service' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fService :: Lens.Lens' Finding (Core.Maybe Types.ServiceInfo)
fService = Lens.field @"service"
{-# INLINEABLE fService #-}
{-# DEPRECATED service "Use generic-lens or generic-optics with 'service' instead"  #-}

-- | The title of the finding.
--
-- /Note:/ Consider using 'title' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fTitle :: Lens.Lens' Finding (Core.Maybe Core.Text)
fTitle = Lens.field @"title"
{-# INLINEABLE fTitle #-}
{-# DEPRECATED title "Use generic-lens or generic-optics with 'title' instead"  #-}

instance Core.FromJSON Finding where
        parseJSON
          = Core.withObject "Finding" Core.$
              \ x ->
                Finding' Core.<$>
                  (x Core..: "accountId") Core.<*> x Core..: "arn" Core.<*>
                    x Core..: "createdAt"
                    Core.<*> x Core..: "id"
                    Core.<*> x Core..: "region"
                    Core.<*> x Core..: "resource"
                    Core.<*> x Core..: "schemaVersion"
                    Core.<*> x Core..: "severity"
                    Core.<*> x Core..: "type"
                    Core.<*> x Core..: "updatedAt"
                    Core.<*> x Core..:? "confidence"
                    Core.<*> x Core..:? "description"
                    Core.<*> x Core..:? "partition"
                    Core.<*> x Core..:? "service"
                    Core.<*> x Core..:? "title"
