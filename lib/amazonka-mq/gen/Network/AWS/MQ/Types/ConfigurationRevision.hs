{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.Types.ConfigurationRevision
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MQ.Types.ConfigurationRevision
  ( ConfigurationRevision (..),

    -- * Smart constructor
    mkConfigurationRevision,

    -- * Lenses
    crCreated,
    crDescription,
    crRevision,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Returns information about the specified configuration revision.
--
-- /See:/ 'mkConfigurationRevision' smart constructor.
data ConfigurationRevision = ConfigurationRevision'
  { -- | Required. The date and time of the configuration revision.
    created :: Core.Maybe Core.UTCTime,
    -- | The description of the configuration revision.
    description :: Core.Maybe Core.Text,
    -- | Required. The revision number of the configuration.
    revision :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ConfigurationRevision' value with any optional fields omitted.
mkConfigurationRevision ::
  ConfigurationRevision
mkConfigurationRevision =
  ConfigurationRevision'
    { created = Core.Nothing,
      description = Core.Nothing,
      revision = Core.Nothing
    }

-- | Required. The date and time of the configuration revision.
--
-- /Note:/ Consider using 'created' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crCreated :: Lens.Lens' ConfigurationRevision (Core.Maybe Core.UTCTime)
crCreated = Lens.field @"created"
{-# DEPRECATED crCreated "Use generic-lens or generic-optics with 'created' instead." #-}

-- | The description of the configuration revision.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crDescription :: Lens.Lens' ConfigurationRevision (Core.Maybe Core.Text)
crDescription = Lens.field @"description"
{-# DEPRECATED crDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Required. The revision number of the configuration.
--
-- /Note:/ Consider using 'revision' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crRevision :: Lens.Lens' ConfigurationRevision (Core.Maybe Core.Int)
crRevision = Lens.field @"revision"
{-# DEPRECATED crRevision "Use generic-lens or generic-optics with 'revision' instead." #-}

instance Core.FromJSON ConfigurationRevision where
  parseJSON =
    Core.withObject "ConfigurationRevision" Core.$
      \x ->
        ConfigurationRevision'
          Core.<$> (x Core..:? "created")
          Core.<*> (x Core..:? "description")
          Core.<*> (x Core..:? "revision")
