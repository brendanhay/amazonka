{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.Types.ImageScanFinding
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECR.Types.ImageScanFinding
  ( ImageScanFinding (..),

    -- * Smart constructor
    mkImageScanFinding,

    -- * Lenses
    isfAttributes,
    isfDescription,
    isfName,
    isfSeverity,
    isfUri,
  )
where

import qualified Network.AWS.ECR.Types.Attribute as Types
import qualified Network.AWS.ECR.Types.Description as Types
import qualified Network.AWS.ECR.Types.FindingName as Types
import qualified Network.AWS.ECR.Types.FindingSeverity as Types
import qualified Network.AWS.ECR.Types.Url as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about an image scan finding.
--
-- /See:/ 'mkImageScanFinding' smart constructor.
data ImageScanFinding = ImageScanFinding'
  { -- | A collection of attributes of the host from which the finding is generated.
    attributes :: Core.Maybe [Types.Attribute],
    -- | The description of the finding.
    description :: Core.Maybe Types.Description,
    -- | The name associated with the finding, usually a CVE number.
    name :: Core.Maybe Types.FindingName,
    -- | The finding severity.
    severity :: Core.Maybe Types.FindingSeverity,
    -- | A link containing additional details about the security vulnerability.
    uri :: Core.Maybe Types.Url
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ImageScanFinding' value with any optional fields omitted.
mkImageScanFinding ::
  ImageScanFinding
mkImageScanFinding =
  ImageScanFinding'
    { attributes = Core.Nothing,
      description = Core.Nothing,
      name = Core.Nothing,
      severity = Core.Nothing,
      uri = Core.Nothing
    }

-- | A collection of attributes of the host from which the finding is generated.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isfAttributes :: Lens.Lens' ImageScanFinding (Core.Maybe [Types.Attribute])
isfAttributes = Lens.field @"attributes"
{-# DEPRECATED isfAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | The description of the finding.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isfDescription :: Lens.Lens' ImageScanFinding (Core.Maybe Types.Description)
isfDescription = Lens.field @"description"
{-# DEPRECATED isfDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The name associated with the finding, usually a CVE number.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isfName :: Lens.Lens' ImageScanFinding (Core.Maybe Types.FindingName)
isfName = Lens.field @"name"
{-# DEPRECATED isfName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The finding severity.
--
-- /Note:/ Consider using 'severity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isfSeverity :: Lens.Lens' ImageScanFinding (Core.Maybe Types.FindingSeverity)
isfSeverity = Lens.field @"severity"
{-# DEPRECATED isfSeverity "Use generic-lens or generic-optics with 'severity' instead." #-}

-- | A link containing additional details about the security vulnerability.
--
-- /Note:/ Consider using 'uri' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isfUri :: Lens.Lens' ImageScanFinding (Core.Maybe Types.Url)
isfUri = Lens.field @"uri"
{-# DEPRECATED isfUri "Use generic-lens or generic-optics with 'uri' instead." #-}

instance Core.FromJSON ImageScanFinding where
  parseJSON =
    Core.withObject "ImageScanFinding" Core.$
      \x ->
        ImageScanFinding'
          Core.<$> (x Core..:? "attributes")
          Core.<*> (x Core..:? "description")
          Core.<*> (x Core..:? "name")
          Core.<*> (x Core..:? "severity")
          Core.<*> (x Core..:? "uri")
