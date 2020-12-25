{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.PatchBaselineIdentity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.PatchBaselineIdentity
  ( PatchBaselineIdentity (..),

    -- * Smart constructor
    mkPatchBaselineIdentity,

    -- * Lenses
    pbiBaselineDescription,
    pbiBaselineId,
    pbiBaselineName,
    pbiDefaultBaseline,
    pbiOperatingSystem,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.BaselineDescription as Types
import qualified Network.AWS.SSM.Types.BaselineId as Types
import qualified Network.AWS.SSM.Types.BaselineName as Types
import qualified Network.AWS.SSM.Types.OperatingSystem as Types

-- | Defines the basic information about a patch baseline.
--
-- /See:/ 'mkPatchBaselineIdentity' smart constructor.
data PatchBaselineIdentity = PatchBaselineIdentity'
  { -- | The description of the patch baseline.
    baselineDescription :: Core.Maybe Types.BaselineDescription,
    -- | The ID of the patch baseline.
    baselineId :: Core.Maybe Types.BaselineId,
    -- | The name of the patch baseline.
    baselineName :: Core.Maybe Types.BaselineName,
    -- | Whether this is the default baseline. Note that Systems Manager supports creating multiple default patch baselines. For example, you can create a default patch baseline for each operating system.
    defaultBaseline :: Core.Maybe Core.Bool,
    -- | Defines the operating system the patch baseline applies to. The Default value is WINDOWS.
    operatingSystem :: Core.Maybe Types.OperatingSystem
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PatchBaselineIdentity' value with any optional fields omitted.
mkPatchBaselineIdentity ::
  PatchBaselineIdentity
mkPatchBaselineIdentity =
  PatchBaselineIdentity'
    { baselineDescription = Core.Nothing,
      baselineId = Core.Nothing,
      baselineName = Core.Nothing,
      defaultBaseline = Core.Nothing,
      operatingSystem = Core.Nothing
    }

-- | The description of the patch baseline.
--
-- /Note:/ Consider using 'baselineDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbiBaselineDescription :: Lens.Lens' PatchBaselineIdentity (Core.Maybe Types.BaselineDescription)
pbiBaselineDescription = Lens.field @"baselineDescription"
{-# DEPRECATED pbiBaselineDescription "Use generic-lens or generic-optics with 'baselineDescription' instead." #-}

-- | The ID of the patch baseline.
--
-- /Note:/ Consider using 'baselineId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbiBaselineId :: Lens.Lens' PatchBaselineIdentity (Core.Maybe Types.BaselineId)
pbiBaselineId = Lens.field @"baselineId"
{-# DEPRECATED pbiBaselineId "Use generic-lens or generic-optics with 'baselineId' instead." #-}

-- | The name of the patch baseline.
--
-- /Note:/ Consider using 'baselineName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbiBaselineName :: Lens.Lens' PatchBaselineIdentity (Core.Maybe Types.BaselineName)
pbiBaselineName = Lens.field @"baselineName"
{-# DEPRECATED pbiBaselineName "Use generic-lens or generic-optics with 'baselineName' instead." #-}

-- | Whether this is the default baseline. Note that Systems Manager supports creating multiple default patch baselines. For example, you can create a default patch baseline for each operating system.
--
-- /Note:/ Consider using 'defaultBaseline' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbiDefaultBaseline :: Lens.Lens' PatchBaselineIdentity (Core.Maybe Core.Bool)
pbiDefaultBaseline = Lens.field @"defaultBaseline"
{-# DEPRECATED pbiDefaultBaseline "Use generic-lens or generic-optics with 'defaultBaseline' instead." #-}

-- | Defines the operating system the patch baseline applies to. The Default value is WINDOWS.
--
-- /Note:/ Consider using 'operatingSystem' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbiOperatingSystem :: Lens.Lens' PatchBaselineIdentity (Core.Maybe Types.OperatingSystem)
pbiOperatingSystem = Lens.field @"operatingSystem"
{-# DEPRECATED pbiOperatingSystem "Use generic-lens or generic-optics with 'operatingSystem' instead." #-}

instance Core.FromJSON PatchBaselineIdentity where
  parseJSON =
    Core.withObject "PatchBaselineIdentity" Core.$
      \x ->
        PatchBaselineIdentity'
          Core.<$> (x Core..:? "BaselineDescription")
          Core.<*> (x Core..:? "BaselineId")
          Core.<*> (x Core..:? "BaselineName")
          Core.<*> (x Core..:? "DefaultBaseline")
          Core.<*> (x Core..:? "OperatingSystem")
