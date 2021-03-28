{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.Command
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EMR.Types.Command
  ( Command (..)
  -- * Smart constructor
  , mkCommand
  -- * Lenses
  , cArgs
  , cName
  , cScriptPath
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An entity describing an executable that runs on a cluster.
--
-- /See:/ 'mkCommand' smart constructor.
data Command = Command'
  { args :: Core.Maybe [Core.Text]
    -- ^ Arguments for Amazon EMR to pass to the command for execution.
  , name :: Core.Maybe Core.Text
    -- ^ The name of the command.
  , scriptPath :: Core.Maybe Core.Text
    -- ^ The Amazon S3 location of the command script.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Command' value with any optional fields omitted.
mkCommand
    :: Command
mkCommand
  = Command'{args = Core.Nothing, name = Core.Nothing,
             scriptPath = Core.Nothing}

-- | Arguments for Amazon EMR to pass to the command for execution.
--
-- /Note:/ Consider using 'args' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cArgs :: Lens.Lens' Command (Core.Maybe [Core.Text])
cArgs = Lens.field @"args"
{-# INLINEABLE cArgs #-}
{-# DEPRECATED args "Use generic-lens or generic-optics with 'args' instead"  #-}

-- | The name of the command.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cName :: Lens.Lens' Command (Core.Maybe Core.Text)
cName = Lens.field @"name"
{-# INLINEABLE cName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The Amazon S3 location of the command script.
--
-- /Note:/ Consider using 'scriptPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cScriptPath :: Lens.Lens' Command (Core.Maybe Core.Text)
cScriptPath = Lens.field @"scriptPath"
{-# INLINEABLE cScriptPath #-}
{-# DEPRECATED scriptPath "Use generic-lens or generic-optics with 'scriptPath' instead"  #-}

instance Core.FromJSON Command where
        parseJSON
          = Core.withObject "Command" Core.$
              \ x ->
                Command' Core.<$>
                  (x Core..:? "Args") Core.<*> x Core..:? "Name" Core.<*>
                    x Core..:? "ScriptPath"
