{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.OperatingSystemConfigurationManager
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.OpsWorks.Types.OperatingSystemConfigurationManager
  ( OperatingSystemConfigurationManager (..)
  -- * Smart constructor
  , mkOperatingSystemConfigurationManager
  -- * Lenses
  , oscmName
  , oscmVersion
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A block that contains information about the configuration manager (Chef) and the versions of the configuration manager that are supported for an operating system.
--
-- /See:/ 'mkOperatingSystemConfigurationManager' smart constructor.
data OperatingSystemConfigurationManager = OperatingSystemConfigurationManager'
  { name :: Core.Maybe Core.Text
    -- ^ The name of the configuration manager, which is Chef.
  , version :: Core.Maybe Core.Text
    -- ^ The versions of the configuration manager that are supported by an operating system.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'OperatingSystemConfigurationManager' value with any optional fields omitted.
mkOperatingSystemConfigurationManager
    :: OperatingSystemConfigurationManager
mkOperatingSystemConfigurationManager
  = OperatingSystemConfigurationManager'{name = Core.Nothing,
                                         version = Core.Nothing}

-- | The name of the configuration manager, which is Chef.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oscmName :: Lens.Lens' OperatingSystemConfigurationManager (Core.Maybe Core.Text)
oscmName = Lens.field @"name"
{-# INLINEABLE oscmName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The versions of the configuration manager that are supported by an operating system.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oscmVersion :: Lens.Lens' OperatingSystemConfigurationManager (Core.Maybe Core.Text)
oscmVersion = Lens.field @"version"
{-# INLINEABLE oscmVersion #-}
{-# DEPRECATED version "Use generic-lens or generic-optics with 'version' instead"  #-}

instance Core.FromJSON OperatingSystemConfigurationManager where
        parseJSON
          = Core.withObject "OperatingSystemConfigurationManager" Core.$
              \ x ->
                OperatingSystemConfigurationManager' Core.<$>
                  (x Core..:? "Name") Core.<*> x Core..:? "Version"
