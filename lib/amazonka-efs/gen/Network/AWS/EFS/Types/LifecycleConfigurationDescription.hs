{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.Types.LifecycleConfigurationDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EFS.Types.LifecycleConfigurationDescription
  ( LifecycleConfigurationDescription (..),

    -- * Smart constructor
    mkLifecycleConfigurationDescription,

    -- * Lenses
    lcdLifecyclePolicies,
  )
where

import qualified Network.AWS.EFS.Types.LifecyclePolicy as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | /See:/ 'mkLifecycleConfigurationDescription' smart constructor.
newtype LifecycleConfigurationDescription = LifecycleConfigurationDescription'
  { -- | An array of lifecycle management policies. Currently, EFS supports a maximum of one policy per file system.
    lifecyclePolicies :: Core.Maybe [Types.LifecyclePolicy]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'LifecycleConfigurationDescription' value with any optional fields omitted.
mkLifecycleConfigurationDescription ::
  LifecycleConfigurationDescription
mkLifecycleConfigurationDescription =
  LifecycleConfigurationDescription'
    { lifecyclePolicies =
        Core.Nothing
    }

-- | An array of lifecycle management policies. Currently, EFS supports a maximum of one policy per file system.
--
-- /Note:/ Consider using 'lifecyclePolicies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcdLifecyclePolicies :: Lens.Lens' LifecycleConfigurationDescription (Core.Maybe [Types.LifecyclePolicy])
lcdLifecyclePolicies = Lens.field @"lifecyclePolicies"
{-# DEPRECATED lcdLifecyclePolicies "Use generic-lens or generic-optics with 'lifecyclePolicies' instead." #-}

instance Core.FromJSON LifecycleConfigurationDescription where
  parseJSON =
    Core.withObject "LifecycleConfigurationDescription" Core.$
      \x ->
        LifecycleConfigurationDescription'
          Core.<$> (x Core..:? "LifecyclePolicies")
