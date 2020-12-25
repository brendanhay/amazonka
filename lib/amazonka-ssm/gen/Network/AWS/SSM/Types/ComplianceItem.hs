{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.ComplianceItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.ComplianceItem
  ( ComplianceItem (..),

    -- * Smart constructor
    mkComplianceItem,

    -- * Lenses
    cifComplianceType,
    cifDetails,
    cifExecutionSummary,
    cifId,
    cifResourceId,
    cifResourceType,
    cifSeverity,
    cifStatus,
    cifTitle,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.AttributeName as Types
import qualified Network.AWS.SSM.Types.AttributeValue as Types
import qualified Network.AWS.SSM.Types.ComplianceExecutionSummary as Types
import qualified Network.AWS.SSM.Types.ComplianceItemId as Types
import qualified Network.AWS.SSM.Types.ComplianceItemTitle as Types
import qualified Network.AWS.SSM.Types.ComplianceResourceId as Types
import qualified Network.AWS.SSM.Types.ComplianceResourceType as Types
import qualified Network.AWS.SSM.Types.ComplianceSeverity as Types
import qualified Network.AWS.SSM.Types.ComplianceStatus as Types
import qualified Network.AWS.SSM.Types.ComplianceTypeName as Types

-- | Information about the compliance as defined by the resource type. For example, for a patch resource type, @Items@ includes information about the PatchSeverity, Classification, and so on.
--
-- /See:/ 'mkComplianceItem' smart constructor.
data ComplianceItem = ComplianceItem'
  { -- | The compliance type. For example, Association (for a State Manager association), Patch, or Custom:@string@ are all valid compliance types.
    complianceType :: Core.Maybe Types.ComplianceTypeName,
    -- | A "Key": "Value" tag combination for the compliance item.
    details :: Core.Maybe (Core.HashMap Types.AttributeName Types.AttributeValue),
    -- | A summary for the compliance item. The summary includes an execution ID, the execution type (for example, command), and the execution time.
    executionSummary :: Core.Maybe Types.ComplianceExecutionSummary,
    -- | An ID for the compliance item. For example, if the compliance item is a Windows patch, the ID could be the number of the KB article; for example: KB4010320.
    id :: Core.Maybe Types.ComplianceItemId,
    -- | An ID for the resource. For a managed instance, this is the instance ID.
    resourceId :: Core.Maybe Types.ComplianceResourceId,
    -- | The type of resource. @ManagedInstance@ is currently the only supported resource type.
    resourceType :: Core.Maybe Types.ComplianceResourceType,
    -- | The severity of the compliance status. Severity can be one of the following: Critical, High, Medium, Low, Informational, Unspecified.
    severity :: Core.Maybe Types.ComplianceSeverity,
    -- | The status of the compliance item. An item is either COMPLIANT, NON_COMPLIANT, or an empty string (for Windows patches that aren't applicable).
    status :: Core.Maybe Types.ComplianceStatus,
    -- | A title for the compliance item. For example, if the compliance item is a Windows patch, the title could be the title of the KB article for the patch; for example: Security Update for Active Directory Federation Services.
    title :: Core.Maybe Types.ComplianceItemTitle
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ComplianceItem' value with any optional fields omitted.
mkComplianceItem ::
  ComplianceItem
mkComplianceItem =
  ComplianceItem'
    { complianceType = Core.Nothing,
      details = Core.Nothing,
      executionSummary = Core.Nothing,
      id = Core.Nothing,
      resourceId = Core.Nothing,
      resourceType = Core.Nothing,
      severity = Core.Nothing,
      status = Core.Nothing,
      title = Core.Nothing
    }

-- | The compliance type. For example, Association (for a State Manager association), Patch, or Custom:@string@ are all valid compliance types.
--
-- /Note:/ Consider using 'complianceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cifComplianceType :: Lens.Lens' ComplianceItem (Core.Maybe Types.ComplianceTypeName)
cifComplianceType = Lens.field @"complianceType"
{-# DEPRECATED cifComplianceType "Use generic-lens or generic-optics with 'complianceType' instead." #-}

-- | A "Key": "Value" tag combination for the compliance item.
--
-- /Note:/ Consider using 'details' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cifDetails :: Lens.Lens' ComplianceItem (Core.Maybe (Core.HashMap Types.AttributeName Types.AttributeValue))
cifDetails = Lens.field @"details"
{-# DEPRECATED cifDetails "Use generic-lens or generic-optics with 'details' instead." #-}

-- | A summary for the compliance item. The summary includes an execution ID, the execution type (for example, command), and the execution time.
--
-- /Note:/ Consider using 'executionSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cifExecutionSummary :: Lens.Lens' ComplianceItem (Core.Maybe Types.ComplianceExecutionSummary)
cifExecutionSummary = Lens.field @"executionSummary"
{-# DEPRECATED cifExecutionSummary "Use generic-lens or generic-optics with 'executionSummary' instead." #-}

-- | An ID for the compliance item. For example, if the compliance item is a Windows patch, the ID could be the number of the KB article; for example: KB4010320.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cifId :: Lens.Lens' ComplianceItem (Core.Maybe Types.ComplianceItemId)
cifId = Lens.field @"id"
{-# DEPRECATED cifId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | An ID for the resource. For a managed instance, this is the instance ID.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cifResourceId :: Lens.Lens' ComplianceItem (Core.Maybe Types.ComplianceResourceId)
cifResourceId = Lens.field @"resourceId"
{-# DEPRECATED cifResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The type of resource. @ManagedInstance@ is currently the only supported resource type.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cifResourceType :: Lens.Lens' ComplianceItem (Core.Maybe Types.ComplianceResourceType)
cifResourceType = Lens.field @"resourceType"
{-# DEPRECATED cifResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The severity of the compliance status. Severity can be one of the following: Critical, High, Medium, Low, Informational, Unspecified.
--
-- /Note:/ Consider using 'severity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cifSeverity :: Lens.Lens' ComplianceItem (Core.Maybe Types.ComplianceSeverity)
cifSeverity = Lens.field @"severity"
{-# DEPRECATED cifSeverity "Use generic-lens or generic-optics with 'severity' instead." #-}

-- | The status of the compliance item. An item is either COMPLIANT, NON_COMPLIANT, or an empty string (for Windows patches that aren't applicable).
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cifStatus :: Lens.Lens' ComplianceItem (Core.Maybe Types.ComplianceStatus)
cifStatus = Lens.field @"status"
{-# DEPRECATED cifStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | A title for the compliance item. For example, if the compliance item is a Windows patch, the title could be the title of the KB article for the patch; for example: Security Update for Active Directory Federation Services.
--
-- /Note:/ Consider using 'title' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cifTitle :: Lens.Lens' ComplianceItem (Core.Maybe Types.ComplianceItemTitle)
cifTitle = Lens.field @"title"
{-# DEPRECATED cifTitle "Use generic-lens or generic-optics with 'title' instead." #-}

instance Core.FromJSON ComplianceItem where
  parseJSON =
    Core.withObject "ComplianceItem" Core.$
      \x ->
        ComplianceItem'
          Core.<$> (x Core..:? "ComplianceType")
          Core.<*> (x Core..:? "Details")
          Core.<*> (x Core..:? "ExecutionSummary")
          Core.<*> (x Core..:? "Id")
          Core.<*> (x Core..:? "ResourceId")
          Core.<*> (x Core..:? "ResourceType")
          Core.<*> (x Core..:? "Severity")
          Core.<*> (x Core..:? "Status")
          Core.<*> (x Core..:? "Title")
