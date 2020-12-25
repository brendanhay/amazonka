{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.Configuration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.Configuration
  ( Configuration (..),

    -- * Smart constructor
    mkConfiguration,

    -- * Lenses
    cEnabled,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Configuration.
--
-- /See:/ 'mkConfiguration' smart constructor.
newtype Configuration = Configuration'
  { -- | True to enable the configuration.
    enabled :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'Configuration' value with any optional fields omitted.
mkConfiguration ::
  Configuration
mkConfiguration = Configuration' {enabled = Core.Nothing}

-- | True to enable the configuration.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cEnabled :: Lens.Lens' Configuration (Core.Maybe Core.Bool)
cEnabled = Lens.field @"enabled"
{-# DEPRECATED cEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

instance Core.FromJSON Configuration where
  toJSON Configuration {..} =
    Core.object
      (Core.catMaybes [("Enabled" Core..=) Core.<$> enabled])

instance Core.FromJSON Configuration where
  parseJSON =
    Core.withObject "Configuration" Core.$
      \x -> Configuration' Core.<$> (x Core..:? "Enabled")
