{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.KernelSpec
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.KernelSpec
  ( KernelSpec (..),

    -- * Smart constructor
    mkKernelSpec,

    -- * Lenses
    ksName,
    ksDisplayName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.DisplayName as Types
import qualified Network.AWS.SageMaker.Types.Name as Types

-- | The specification of a Jupyter kernel.
--
-- /See:/ 'mkKernelSpec' smart constructor.
data KernelSpec = KernelSpec'
  { -- | The name of the kernel.
    name :: Types.Name,
    -- | The display name of the kernel.
    displayName :: Core.Maybe Types.DisplayName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'KernelSpec' value with any optional fields omitted.
mkKernelSpec ::
  -- | 'name'
  Types.Name ->
  KernelSpec
mkKernelSpec name = KernelSpec' {name, displayName = Core.Nothing}

-- | The name of the kernel.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ksName :: Lens.Lens' KernelSpec Types.Name
ksName = Lens.field @"name"
{-# DEPRECATED ksName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The display name of the kernel.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ksDisplayName :: Lens.Lens' KernelSpec (Core.Maybe Types.DisplayName)
ksDisplayName = Lens.field @"displayName"
{-# DEPRECATED ksDisplayName "Use generic-lens or generic-optics with 'displayName' instead." #-}

instance Core.FromJSON KernelSpec where
  toJSON KernelSpec {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            ("DisplayName" Core..=) Core.<$> displayName
          ]
      )

instance Core.FromJSON KernelSpec where
  parseJSON =
    Core.withObject "KernelSpec" Core.$
      \x ->
        KernelSpec'
          Core.<$> (x Core..: "Name") Core.<*> (x Core..:? "DisplayName")
