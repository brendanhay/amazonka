{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.StackConfigurationManager
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.OpsWorks.Types.StackConfigurationManager
  ( StackConfigurationManager (..)
  -- * Smart constructor
  , mkStackConfigurationManager
  -- * Lenses
  , scmName
  , scmVersion
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the configuration manager.
--
-- /See:/ 'mkStackConfigurationManager' smart constructor.
data StackConfigurationManager = StackConfigurationManager'
  { name :: Core.Maybe Core.Text
    -- ^ The name. This parameter must be set to "Chef".
  , version :: Core.Maybe Core.Text
    -- ^ The Chef version. This parameter must be set to 12, 11.10, or 11.4 for Linux stacks, and to 12.2 for Windows stacks. The default value for Linux stacks is 11.4.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StackConfigurationManager' value with any optional fields omitted.
mkStackConfigurationManager
    :: StackConfigurationManager
mkStackConfigurationManager
  = StackConfigurationManager'{name = Core.Nothing,
                               version = Core.Nothing}

-- | The name. This parameter must be set to "Chef".
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scmName :: Lens.Lens' StackConfigurationManager (Core.Maybe Core.Text)
scmName = Lens.field @"name"
{-# INLINEABLE scmName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The Chef version. This parameter must be set to 12, 11.10, or 11.4 for Linux stacks, and to 12.2 for Windows stacks. The default value for Linux stacks is 11.4.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scmVersion :: Lens.Lens' StackConfigurationManager (Core.Maybe Core.Text)
scmVersion = Lens.field @"version"
{-# INLINEABLE scmVersion #-}
{-# DEPRECATED version "Use generic-lens or generic-optics with 'version' instead"  #-}

instance Core.FromJSON StackConfigurationManager where
        toJSON StackConfigurationManager{..}
          = Core.object
              (Core.catMaybes
                 [("Name" Core..=) Core.<$> name,
                  ("Version" Core..=) Core.<$> version])

instance Core.FromJSON StackConfigurationManager where
        parseJSON
          = Core.withObject "StackConfigurationManager" Core.$
              \ x ->
                StackConfigurationManager' Core.<$>
                  (x Core..:? "Name") Core.<*> x Core..:? "Version"
