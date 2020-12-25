{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.ComplianceItemEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.ComplianceItemEntry
  ( ComplianceItemEntry (..),

    -- * Smart constructor
    mkComplianceItemEntry,

    -- * Lenses
    cieSeverity,
    cieStatus,
    cieDetails,
    cieId,
    cieTitle,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.AttributeName as Types
import qualified Network.AWS.SSM.Types.AttributeValue as Types
import qualified Network.AWS.SSM.Types.ComplianceSeverity as Types
import qualified Network.AWS.SSM.Types.ComplianceStatus as Types
import qualified Network.AWS.SSM.Types.Id as Types
import qualified Network.AWS.SSM.Types.Title as Types

-- | Information about a compliance item.
--
-- /See:/ 'mkComplianceItemEntry' smart constructor.
data ComplianceItemEntry = ComplianceItemEntry'
  { -- | The severity of the compliance status. Severity can be one of the following: Critical, High, Medium, Low, Informational, Unspecified.
    severity :: Types.ComplianceSeverity,
    -- | The status of the compliance item. An item is either COMPLIANT or NON_COMPLIANT.
    status :: Types.ComplianceStatus,
    -- | A "Key": "Value" tag combination for the compliance item.
    details :: Core.Maybe (Core.HashMap Types.AttributeName Types.AttributeValue),
    -- | The compliance item ID. For example, if the compliance item is a Windows patch, the ID could be the number of the KB article.
    id :: Core.Maybe Types.Id,
    -- | The title of the compliance item. For example, if the compliance item is a Windows patch, the title could be the title of the KB article for the patch; for example: Security Update for Active Directory Federation Services.
    title :: Core.Maybe Types.Title
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ComplianceItemEntry' value with any optional fields omitted.
mkComplianceItemEntry ::
  -- | 'severity'
  Types.ComplianceSeverity ->
  -- | 'status'
  Types.ComplianceStatus ->
  ComplianceItemEntry
mkComplianceItemEntry severity status =
  ComplianceItemEntry'
    { severity,
      status,
      details = Core.Nothing,
      id = Core.Nothing,
      title = Core.Nothing
    }

-- | The severity of the compliance status. Severity can be one of the following: Critical, High, Medium, Low, Informational, Unspecified.
--
-- /Note:/ Consider using 'severity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cieSeverity :: Lens.Lens' ComplianceItemEntry Types.ComplianceSeverity
cieSeverity = Lens.field @"severity"
{-# DEPRECATED cieSeverity "Use generic-lens or generic-optics with 'severity' instead." #-}

-- | The status of the compliance item. An item is either COMPLIANT or NON_COMPLIANT.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cieStatus :: Lens.Lens' ComplianceItemEntry Types.ComplianceStatus
cieStatus = Lens.field @"status"
{-# DEPRECATED cieStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | A "Key": "Value" tag combination for the compliance item.
--
-- /Note:/ Consider using 'details' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cieDetails :: Lens.Lens' ComplianceItemEntry (Core.Maybe (Core.HashMap Types.AttributeName Types.AttributeValue))
cieDetails = Lens.field @"details"
{-# DEPRECATED cieDetails "Use generic-lens or generic-optics with 'details' instead." #-}

-- | The compliance item ID. For example, if the compliance item is a Windows patch, the ID could be the number of the KB article.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cieId :: Lens.Lens' ComplianceItemEntry (Core.Maybe Types.Id)
cieId = Lens.field @"id"
{-# DEPRECATED cieId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The title of the compliance item. For example, if the compliance item is a Windows patch, the title could be the title of the KB article for the patch; for example: Security Update for Active Directory Federation Services.
--
-- /Note:/ Consider using 'title' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cieTitle :: Lens.Lens' ComplianceItemEntry (Core.Maybe Types.Title)
cieTitle = Lens.field @"title"
{-# DEPRECATED cieTitle "Use generic-lens or generic-optics with 'title' instead." #-}

instance Core.FromJSON ComplianceItemEntry where
  toJSON ComplianceItemEntry {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Severity" Core..= severity),
            Core.Just ("Status" Core..= status),
            ("Details" Core..=) Core.<$> details,
            ("Id" Core..=) Core.<$> id,
            ("Title" Core..=) Core.<$> title
          ]
      )
