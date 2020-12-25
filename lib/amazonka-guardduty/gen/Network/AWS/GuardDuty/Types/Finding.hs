{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.Finding
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.Finding
  ( Finding (..),

    -- * Smart constructor
    mkFinding,

    -- * Lenses
    fAccountId,
    fArn,
    fCreatedAt,
    fId,
    fRegion,
    fResource,
    fSchemaVersion,
    fSeverity,
    fType,
    fUpdatedAt,
    fConfidence,
    fDescription,
    fPartition,
    fService,
    fTitle,
  )
where

import qualified Network.AWS.GuardDuty.Types.Resource as Types
import qualified Network.AWS.GuardDuty.Types.ServiceInfo as Types
import qualified Network.AWS.GuardDuty.Types.String as Types
import qualified Network.AWS.GuardDuty.Types.Type as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about the finding, which is generated when abnormal or suspicious activity is detected.
--
-- /See:/ 'mkFinding' smart constructor.
data Finding = Finding'
  { -- | The ID of the account in which the finding was generated.
    accountId :: Types.String,
    -- | The ARN of the finding.
    arn :: Types.String,
    -- | The time and date when the finding was created.
    createdAt :: Types.String,
    -- | The ID of the finding.
    id :: Types.String,
    -- | The Region where the finding was generated.
    region :: Types.String,
    resource :: Types.Resource,
    -- | The version of the schema used for the finding.
    schemaVersion :: Types.String,
    -- | The severity of the finding.
    severity :: Core.Double,
    -- | The type of finding.
    type' :: Types.Type,
    -- | The time and date when the finding was last updated.
    updatedAt :: Types.String,
    -- | The confidence score for the finding.
    confidence :: Core.Maybe Core.Double,
    -- | The description of the finding.
    description :: Core.Maybe Types.String,
    -- | The partition associated with the finding.
    partition :: Core.Maybe Types.String,
    service :: Core.Maybe Types.ServiceInfo,
    -- | The title of the finding.
    title :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'Finding' value with any optional fields omitted.
mkFinding ::
  -- | 'accountId'
  Types.String ->
  -- | 'arn'
  Types.String ->
  -- | 'createdAt'
  Types.String ->
  -- | 'id'
  Types.String ->
  -- | 'region'
  Types.String ->
  -- | 'resource'
  Types.Resource ->
  -- | 'schemaVersion'
  Types.String ->
  -- | 'severity'
  Core.Double ->
  -- | 'type\''
  Types.Type ->
  -- | 'updatedAt'
  Types.String ->
  Finding
mkFinding
  accountId
  arn
  createdAt
  id
  region
  resource
  schemaVersion
  severity
  type'
  updatedAt =
    Finding'
      { accountId,
        arn,
        createdAt,
        id,
        region,
        resource,
        schemaVersion,
        severity,
        type',
        updatedAt,
        confidence = Core.Nothing,
        description = Core.Nothing,
        partition = Core.Nothing,
        service = Core.Nothing,
        title = Core.Nothing
      }

-- | The ID of the account in which the finding was generated.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fAccountId :: Lens.Lens' Finding Types.String
fAccountId = Lens.field @"accountId"
{-# DEPRECATED fAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | The ARN of the finding.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fArn :: Lens.Lens' Finding Types.String
fArn = Lens.field @"arn"
{-# DEPRECATED fArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The time and date when the finding was created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fCreatedAt :: Lens.Lens' Finding Types.String
fCreatedAt = Lens.field @"createdAt"
{-# DEPRECATED fCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | The ID of the finding.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fId :: Lens.Lens' Finding Types.String
fId = Lens.field @"id"
{-# DEPRECATED fId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The Region where the finding was generated.
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fRegion :: Lens.Lens' Finding Types.String
fRegion = Lens.field @"region"
{-# DEPRECATED fRegion "Use generic-lens or generic-optics with 'region' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'resource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fResource :: Lens.Lens' Finding Types.Resource
fResource = Lens.field @"resource"
{-# DEPRECATED fResource "Use generic-lens or generic-optics with 'resource' instead." #-}

-- | The version of the schema used for the finding.
--
-- /Note:/ Consider using 'schemaVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fSchemaVersion :: Lens.Lens' Finding Types.String
fSchemaVersion = Lens.field @"schemaVersion"
{-# DEPRECATED fSchemaVersion "Use generic-lens or generic-optics with 'schemaVersion' instead." #-}

-- | The severity of the finding.
--
-- /Note:/ Consider using 'severity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fSeverity :: Lens.Lens' Finding Core.Double
fSeverity = Lens.field @"severity"
{-# DEPRECATED fSeverity "Use generic-lens or generic-optics with 'severity' instead." #-}

-- | The type of finding.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fType :: Lens.Lens' Finding Types.Type
fType = Lens.field @"type'"
{-# DEPRECATED fType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The time and date when the finding was last updated.
--
-- /Note:/ Consider using 'updatedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fUpdatedAt :: Lens.Lens' Finding Types.String
fUpdatedAt = Lens.field @"updatedAt"
{-# DEPRECATED fUpdatedAt "Use generic-lens or generic-optics with 'updatedAt' instead." #-}

-- | The confidence score for the finding.
--
-- /Note:/ Consider using 'confidence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fConfidence :: Lens.Lens' Finding (Core.Maybe Core.Double)
fConfidence = Lens.field @"confidence"
{-# DEPRECATED fConfidence "Use generic-lens or generic-optics with 'confidence' instead." #-}

-- | The description of the finding.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fDescription :: Lens.Lens' Finding (Core.Maybe Types.String)
fDescription = Lens.field @"description"
{-# DEPRECATED fDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The partition associated with the finding.
--
-- /Note:/ Consider using 'partition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fPartition :: Lens.Lens' Finding (Core.Maybe Types.String)
fPartition = Lens.field @"partition"
{-# DEPRECATED fPartition "Use generic-lens or generic-optics with 'partition' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'service' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fService :: Lens.Lens' Finding (Core.Maybe Types.ServiceInfo)
fService = Lens.field @"service"
{-# DEPRECATED fService "Use generic-lens or generic-optics with 'service' instead." #-}

-- | The title of the finding.
--
-- /Note:/ Consider using 'title' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fTitle :: Lens.Lens' Finding (Core.Maybe Types.String)
fTitle = Lens.field @"title"
{-# DEPRECATED fTitle "Use generic-lens or generic-optics with 'title' instead." #-}

instance Core.FromJSON Finding where
  parseJSON =
    Core.withObject "Finding" Core.$
      \x ->
        Finding'
          Core.<$> (x Core..: "accountId")
          Core.<*> (x Core..: "arn")
          Core.<*> (x Core..: "createdAt")
          Core.<*> (x Core..: "id")
          Core.<*> (x Core..: "region")
          Core.<*> (x Core..: "resource")
          Core.<*> (x Core..: "schemaVersion")
          Core.<*> (x Core..: "severity")
          Core.<*> (x Core..: "type")
          Core.<*> (x Core..: "updatedAt")
          Core.<*> (x Core..:? "confidence")
          Core.<*> (x Core..:? "description")
          Core.<*> (x Core..:? "partition")
          Core.<*> (x Core..:? "service")
          Core.<*> (x Core..:? "title")
