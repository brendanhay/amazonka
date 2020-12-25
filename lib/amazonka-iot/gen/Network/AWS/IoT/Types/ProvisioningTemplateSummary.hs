{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.ProvisioningTemplateSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ProvisioningTemplateSummary
  ( ProvisioningTemplateSummary (..),

    -- * Smart constructor
    mkProvisioningTemplateSummary,

    -- * Lenses
    ptsCreationDate,
    ptsDescription,
    ptsEnabled,
    ptsLastModifiedDate,
    ptsTemplateArn,
    ptsTemplateName,
  )
where

import qualified Network.AWS.IoT.Types.Description as Types
import qualified Network.AWS.IoT.Types.TemplateArn as Types
import qualified Network.AWS.IoT.Types.TemplateName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A summary of information about a fleet provisioning template.
--
-- /See:/ 'mkProvisioningTemplateSummary' smart constructor.
data ProvisioningTemplateSummary = ProvisioningTemplateSummary'
  { -- | The date when the fleet provisioning template summary was created.
    creationDate :: Core.Maybe Core.NominalDiffTime,
    -- | The description of the fleet provisioning template.
    description :: Core.Maybe Types.Description,
    -- | True if the fleet provision template is enabled, otherwise false.
    enabled :: Core.Maybe Core.Bool,
    -- | The date when the fleet provisioning template summary was last modified.
    lastModifiedDate :: Core.Maybe Core.NominalDiffTime,
    -- | The ARN of the fleet provisioning template.
    templateArn :: Core.Maybe Types.TemplateArn,
    -- | The name of the fleet provisioning template.
    templateName :: Core.Maybe Types.TemplateName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ProvisioningTemplateSummary' value with any optional fields omitted.
mkProvisioningTemplateSummary ::
  ProvisioningTemplateSummary
mkProvisioningTemplateSummary =
  ProvisioningTemplateSummary'
    { creationDate = Core.Nothing,
      description = Core.Nothing,
      enabled = Core.Nothing,
      lastModifiedDate = Core.Nothing,
      templateArn = Core.Nothing,
      templateName = Core.Nothing
    }

-- | The date when the fleet provisioning template summary was created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptsCreationDate :: Lens.Lens' ProvisioningTemplateSummary (Core.Maybe Core.NominalDiffTime)
ptsCreationDate = Lens.field @"creationDate"
{-# DEPRECATED ptsCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The description of the fleet provisioning template.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptsDescription :: Lens.Lens' ProvisioningTemplateSummary (Core.Maybe Types.Description)
ptsDescription = Lens.field @"description"
{-# DEPRECATED ptsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | True if the fleet provision template is enabled, otherwise false.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptsEnabled :: Lens.Lens' ProvisioningTemplateSummary (Core.Maybe Core.Bool)
ptsEnabled = Lens.field @"enabled"
{-# DEPRECATED ptsEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | The date when the fleet provisioning template summary was last modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptsLastModifiedDate :: Lens.Lens' ProvisioningTemplateSummary (Core.Maybe Core.NominalDiffTime)
ptsLastModifiedDate = Lens.field @"lastModifiedDate"
{-# DEPRECATED ptsLastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead." #-}

-- | The ARN of the fleet provisioning template.
--
-- /Note:/ Consider using 'templateArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptsTemplateArn :: Lens.Lens' ProvisioningTemplateSummary (Core.Maybe Types.TemplateArn)
ptsTemplateArn = Lens.field @"templateArn"
{-# DEPRECATED ptsTemplateArn "Use generic-lens or generic-optics with 'templateArn' instead." #-}

-- | The name of the fleet provisioning template.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptsTemplateName :: Lens.Lens' ProvisioningTemplateSummary (Core.Maybe Types.TemplateName)
ptsTemplateName = Lens.field @"templateName"
{-# DEPRECATED ptsTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

instance Core.FromJSON ProvisioningTemplateSummary where
  parseJSON =
    Core.withObject "ProvisioningTemplateSummary" Core.$
      \x ->
        ProvisioningTemplateSummary'
          Core.<$> (x Core..:? "creationDate")
          Core.<*> (x Core..:? "description")
          Core.<*> (x Core..:? "enabled")
          Core.<*> (x Core..:? "lastModifiedDate")
          Core.<*> (x Core..:? "templateArn")
          Core.<*> (x Core..:? "templateName")
