{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.OpsItemSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.OpsItemSummary
  ( OpsItemSummary (..),

    -- * Smart constructor
    mkOpsItemSummary,

    -- * Lenses
    oisCategory,
    oisCreatedBy,
    oisCreatedTime,
    oisLastModifiedBy,
    oisLastModifiedTime,
    oisOperationalData,
    oisOpsItemId,
    oisPriority,
    oisSeverity,
    oisSource,
    oisStatus,
    oisTitle,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.Category as Types
import qualified Network.AWS.SSM.Types.OpsItemDataKey as Types
import qualified Network.AWS.SSM.Types.OpsItemDataValue as Types
import qualified Network.AWS.SSM.Types.OpsItemId as Types
import qualified Network.AWS.SSM.Types.OpsItemSource as Types
import qualified Network.AWS.SSM.Types.OpsItemStatus as Types
import qualified Network.AWS.SSM.Types.Severity as Types
import qualified Network.AWS.SSM.Types.String as Types
import qualified Network.AWS.SSM.Types.Title as Types

-- | A count of OpsItems.
--
-- /See:/ 'mkOpsItemSummary' smart constructor.
data OpsItemSummary = OpsItemSummary'
  { -- | A list of OpsItems by category.
    category :: Core.Maybe Types.Category,
    -- | The Amazon Resource Name (ARN) of the IAM entity that created the OpsItem.
    createdBy :: Core.Maybe Types.String,
    -- | The date and time the OpsItem was created.
    createdTime :: Core.Maybe Core.NominalDiffTime,
    -- | The Amazon Resource Name (ARN) of the IAM entity that created the OpsItem.
    lastModifiedBy :: Core.Maybe Types.String,
    -- | The date and time the OpsItem was last updated.
    lastModifiedTime :: Core.Maybe Core.NominalDiffTime,
    -- | Operational data is custom data that provides useful reference details about the OpsItem.
    operationalData :: Core.Maybe (Core.HashMap Types.OpsItemDataKey Types.OpsItemDataValue),
    -- | The ID of the OpsItem.
    opsItemId :: Core.Maybe Types.OpsItemId,
    -- | The importance of this OpsItem in relation to other OpsItems in the system.
    priority :: Core.Maybe Core.Natural,
    -- | A list of OpsItems by severity.
    severity :: Core.Maybe Types.Severity,
    -- | The impacted AWS resource.
    source :: Core.Maybe Types.OpsItemSource,
    -- | The OpsItem status. Status can be @Open@ , @In Progress@ , or @Resolved@ .
    status :: Core.Maybe Types.OpsItemStatus,
    -- | A short heading that describes the nature of the OpsItem and the impacted resource.
    title :: Core.Maybe Types.Title
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'OpsItemSummary' value with any optional fields omitted.
mkOpsItemSummary ::
  OpsItemSummary
mkOpsItemSummary =
  OpsItemSummary'
    { category = Core.Nothing,
      createdBy = Core.Nothing,
      createdTime = Core.Nothing,
      lastModifiedBy = Core.Nothing,
      lastModifiedTime = Core.Nothing,
      operationalData = Core.Nothing,
      opsItemId = Core.Nothing,
      priority = Core.Nothing,
      severity = Core.Nothing,
      source = Core.Nothing,
      status = Core.Nothing,
      title = Core.Nothing
    }

-- | A list of OpsItems by category.
--
-- /Note:/ Consider using 'category' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oisCategory :: Lens.Lens' OpsItemSummary (Core.Maybe Types.Category)
oisCategory = Lens.field @"category"
{-# DEPRECATED oisCategory "Use generic-lens or generic-optics with 'category' instead." #-}

-- | The Amazon Resource Name (ARN) of the IAM entity that created the OpsItem.
--
-- /Note:/ Consider using 'createdBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oisCreatedBy :: Lens.Lens' OpsItemSummary (Core.Maybe Types.String)
oisCreatedBy = Lens.field @"createdBy"
{-# DEPRECATED oisCreatedBy "Use generic-lens or generic-optics with 'createdBy' instead." #-}

-- | The date and time the OpsItem was created.
--
-- /Note:/ Consider using 'createdTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oisCreatedTime :: Lens.Lens' OpsItemSummary (Core.Maybe Core.NominalDiffTime)
oisCreatedTime = Lens.field @"createdTime"
{-# DEPRECATED oisCreatedTime "Use generic-lens or generic-optics with 'createdTime' instead." #-}

-- | The Amazon Resource Name (ARN) of the IAM entity that created the OpsItem.
--
-- /Note:/ Consider using 'lastModifiedBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oisLastModifiedBy :: Lens.Lens' OpsItemSummary (Core.Maybe Types.String)
oisLastModifiedBy = Lens.field @"lastModifiedBy"
{-# DEPRECATED oisLastModifiedBy "Use generic-lens or generic-optics with 'lastModifiedBy' instead." #-}

-- | The date and time the OpsItem was last updated.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oisLastModifiedTime :: Lens.Lens' OpsItemSummary (Core.Maybe Core.NominalDiffTime)
oisLastModifiedTime = Lens.field @"lastModifiedTime"
{-# DEPRECATED oisLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | Operational data is custom data that provides useful reference details about the OpsItem.
--
-- /Note:/ Consider using 'operationalData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oisOperationalData :: Lens.Lens' OpsItemSummary (Core.Maybe (Core.HashMap Types.OpsItemDataKey Types.OpsItemDataValue))
oisOperationalData = Lens.field @"operationalData"
{-# DEPRECATED oisOperationalData "Use generic-lens or generic-optics with 'operationalData' instead." #-}

-- | The ID of the OpsItem.
--
-- /Note:/ Consider using 'opsItemId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oisOpsItemId :: Lens.Lens' OpsItemSummary (Core.Maybe Types.OpsItemId)
oisOpsItemId = Lens.field @"opsItemId"
{-# DEPRECATED oisOpsItemId "Use generic-lens or generic-optics with 'opsItemId' instead." #-}

-- | The importance of this OpsItem in relation to other OpsItems in the system.
--
-- /Note:/ Consider using 'priority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oisPriority :: Lens.Lens' OpsItemSummary (Core.Maybe Core.Natural)
oisPriority = Lens.field @"priority"
{-# DEPRECATED oisPriority "Use generic-lens or generic-optics with 'priority' instead." #-}

-- | A list of OpsItems by severity.
--
-- /Note:/ Consider using 'severity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oisSeverity :: Lens.Lens' OpsItemSummary (Core.Maybe Types.Severity)
oisSeverity = Lens.field @"severity"
{-# DEPRECATED oisSeverity "Use generic-lens or generic-optics with 'severity' instead." #-}

-- | The impacted AWS resource.
--
-- /Note:/ Consider using 'source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oisSource :: Lens.Lens' OpsItemSummary (Core.Maybe Types.OpsItemSource)
oisSource = Lens.field @"source"
{-# DEPRECATED oisSource "Use generic-lens or generic-optics with 'source' instead." #-}

-- | The OpsItem status. Status can be @Open@ , @In Progress@ , or @Resolved@ .
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oisStatus :: Lens.Lens' OpsItemSummary (Core.Maybe Types.OpsItemStatus)
oisStatus = Lens.field @"status"
{-# DEPRECATED oisStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | A short heading that describes the nature of the OpsItem and the impacted resource.
--
-- /Note:/ Consider using 'title' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oisTitle :: Lens.Lens' OpsItemSummary (Core.Maybe Types.Title)
oisTitle = Lens.field @"title"
{-# DEPRECATED oisTitle "Use generic-lens or generic-optics with 'title' instead." #-}

instance Core.FromJSON OpsItemSummary where
  parseJSON =
    Core.withObject "OpsItemSummary" Core.$
      \x ->
        OpsItemSummary'
          Core.<$> (x Core..:? "Category")
          Core.<*> (x Core..:? "CreatedBy")
          Core.<*> (x Core..:? "CreatedTime")
          Core.<*> (x Core..:? "LastModifiedBy")
          Core.<*> (x Core..:? "LastModifiedTime")
          Core.<*> (x Core..:? "OperationalData")
          Core.<*> (x Core..:? "OpsItemId")
          Core.<*> (x Core..:? "Priority")
          Core.<*> (x Core..:? "Severity")
          Core.<*> (x Core..:? "Source")
          Core.<*> (x Core..:? "Status")
          Core.<*> (x Core..:? "Title")
