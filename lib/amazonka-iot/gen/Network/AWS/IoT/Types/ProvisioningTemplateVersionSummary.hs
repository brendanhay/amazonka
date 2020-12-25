{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.ProvisioningTemplateVersionSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ProvisioningTemplateVersionSummary
  ( ProvisioningTemplateVersionSummary (..),

    -- * Smart constructor
    mkProvisioningTemplateVersionSummary,

    -- * Lenses
    ptvsCreationDate,
    ptvsIsDefaultVersion,
    ptvsVersionId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A summary of information about a fleet provision template version.
--
-- /See:/ 'mkProvisioningTemplateVersionSummary' smart constructor.
data ProvisioningTemplateVersionSummary = ProvisioningTemplateVersionSummary'
  { -- | The date when the fleet provisioning template version was created
    creationDate :: Core.Maybe Core.NominalDiffTime,
    -- | True if the fleet provisioning template version is the default version, otherwise false.
    isDefaultVersion :: Core.Maybe Core.Bool,
    -- | The ID of the fleet privisioning template version.
    versionId :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ProvisioningTemplateVersionSummary' value with any optional fields omitted.
mkProvisioningTemplateVersionSummary ::
  ProvisioningTemplateVersionSummary
mkProvisioningTemplateVersionSummary =
  ProvisioningTemplateVersionSummary'
    { creationDate = Core.Nothing,
      isDefaultVersion = Core.Nothing,
      versionId = Core.Nothing
    }

-- | The date when the fleet provisioning template version was created
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptvsCreationDate :: Lens.Lens' ProvisioningTemplateVersionSummary (Core.Maybe Core.NominalDiffTime)
ptvsCreationDate = Lens.field @"creationDate"
{-# DEPRECATED ptvsCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | True if the fleet provisioning template version is the default version, otherwise false.
--
-- /Note:/ Consider using 'isDefaultVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptvsIsDefaultVersion :: Lens.Lens' ProvisioningTemplateVersionSummary (Core.Maybe Core.Bool)
ptvsIsDefaultVersion = Lens.field @"isDefaultVersion"
{-# DEPRECATED ptvsIsDefaultVersion "Use generic-lens or generic-optics with 'isDefaultVersion' instead." #-}

-- | The ID of the fleet privisioning template version.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptvsVersionId :: Lens.Lens' ProvisioningTemplateVersionSummary (Core.Maybe Core.Int)
ptvsVersionId = Lens.field @"versionId"
{-# DEPRECATED ptvsVersionId "Use generic-lens or generic-optics with 'versionId' instead." #-}

instance Core.FromJSON ProvisioningTemplateVersionSummary where
  parseJSON =
    Core.withObject "ProvisioningTemplateVersionSummary" Core.$
      \x ->
        ProvisioningTemplateVersionSummary'
          Core.<$> (x Core..:? "creationDate")
          Core.<*> (x Core..:? "isDefaultVersion")
          Core.<*> (x Core..:? "versionId")
