{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.Command
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.Command
  ( Command (..),

    -- * Smart constructor
    mkCommand,

    -- * Lenses
    cArgs,
    cName,
    cScriptPath,
  )
where

import qualified Network.AWS.EMR.Types.Name as Types
import qualified Network.AWS.EMR.Types.ScriptPath as Types
import qualified Network.AWS.EMR.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An entity describing an executable that runs on a cluster.
--
-- /See:/ 'mkCommand' smart constructor.
data Command = Command'
  { -- | Arguments for Amazon EMR to pass to the command for execution.
    args :: Core.Maybe [Types.String],
    -- | The name of the command.
    name :: Core.Maybe Types.Name,
    -- | The Amazon S3 location of the command script.
    scriptPath :: Core.Maybe Types.ScriptPath
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Command' value with any optional fields omitted.
mkCommand ::
  Command
mkCommand =
  Command'
    { args = Core.Nothing,
      name = Core.Nothing,
      scriptPath = Core.Nothing
    }

-- | Arguments for Amazon EMR to pass to the command for execution.
--
-- /Note:/ Consider using 'args' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cArgs :: Lens.Lens' Command (Core.Maybe [Types.String])
cArgs = Lens.field @"args"
{-# DEPRECATED cArgs "Use generic-lens or generic-optics with 'args' instead." #-}

-- | The name of the command.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cName :: Lens.Lens' Command (Core.Maybe Types.Name)
cName = Lens.field @"name"
{-# DEPRECATED cName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The Amazon S3 location of the command script.
--
-- /Note:/ Consider using 'scriptPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cScriptPath :: Lens.Lens' Command (Core.Maybe Types.ScriptPath)
cScriptPath = Lens.field @"scriptPath"
{-# DEPRECATED cScriptPath "Use generic-lens or generic-optics with 'scriptPath' instead." #-}

instance Core.FromJSON Command where
  parseJSON =
    Core.withObject "Command" Core.$
      \x ->
        Command'
          Core.<$> (x Core..:? "Args")
          Core.<*> (x Core..:? "Name")
          Core.<*> (x Core..:? "ScriptPath")
