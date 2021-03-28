{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.BootstrapActionConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EMR.Types.BootstrapActionConfig
  ( BootstrapActionConfig (..)
  -- * Smart constructor
  , mkBootstrapActionConfig
  -- * Lenses
  , bacName
  , bacScriptBootstrapAction
  ) where

import qualified Network.AWS.EMR.Types.ScriptBootstrapActionConfig as Types
import qualified Network.AWS.EMR.Types.XmlStringMaxLen256 as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Configuration of a bootstrap action.
--
-- /See:/ 'mkBootstrapActionConfig' smart constructor.
data BootstrapActionConfig = BootstrapActionConfig'
  { name :: Types.XmlStringMaxLen256
    -- ^ The name of the bootstrap action.
  , scriptBootstrapAction :: Types.ScriptBootstrapActionConfig
    -- ^ The script run by the bootstrap action.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BootstrapActionConfig' value with any optional fields omitted.
mkBootstrapActionConfig
    :: Types.XmlStringMaxLen256 -- ^ 'name'
    -> Types.ScriptBootstrapActionConfig -- ^ 'scriptBootstrapAction'
    -> BootstrapActionConfig
mkBootstrapActionConfig name scriptBootstrapAction
  = BootstrapActionConfig'{name, scriptBootstrapAction}

-- | The name of the bootstrap action.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bacName :: Lens.Lens' BootstrapActionConfig Types.XmlStringMaxLen256
bacName = Lens.field @"name"
{-# INLINEABLE bacName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The script run by the bootstrap action.
--
-- /Note:/ Consider using 'scriptBootstrapAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bacScriptBootstrapAction :: Lens.Lens' BootstrapActionConfig Types.ScriptBootstrapActionConfig
bacScriptBootstrapAction = Lens.field @"scriptBootstrapAction"
{-# INLINEABLE bacScriptBootstrapAction #-}
{-# DEPRECATED scriptBootstrapAction "Use generic-lens or generic-optics with 'scriptBootstrapAction' instead"  #-}

instance Core.FromJSON BootstrapActionConfig where
        toJSON BootstrapActionConfig{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Name" Core..= name),
                  Core.Just ("ScriptBootstrapAction" Core..= scriptBootstrapAction)])
