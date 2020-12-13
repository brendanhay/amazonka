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
    cScriptPath,
    cName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An entity describing an executable that runs on a cluster.
--
-- /See:/ 'mkCommand' smart constructor.
data Command = Command'
  { -- | Arguments for Amazon EMR to pass to the command for execution.
    args :: Lude.Maybe [Lude.Text],
    -- | The Amazon S3 location of the command script.
    scriptPath :: Lude.Maybe Lude.Text,
    -- | The name of the command.
    name :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Command' with the minimum fields required to make a request.
--
-- * 'args' - Arguments for Amazon EMR to pass to the command for execution.
-- * 'scriptPath' - The Amazon S3 location of the command script.
-- * 'name' - The name of the command.
mkCommand ::
  Command
mkCommand =
  Command'
    { args = Lude.Nothing,
      scriptPath = Lude.Nothing,
      name = Lude.Nothing
    }

-- | Arguments for Amazon EMR to pass to the command for execution.
--
-- /Note:/ Consider using 'args' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cArgs :: Lens.Lens' Command (Lude.Maybe [Lude.Text])
cArgs = Lens.lens (args :: Command -> Lude.Maybe [Lude.Text]) (\s a -> s {args = a} :: Command)
{-# DEPRECATED cArgs "Use generic-lens or generic-optics with 'args' instead." #-}

-- | The Amazon S3 location of the command script.
--
-- /Note:/ Consider using 'scriptPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cScriptPath :: Lens.Lens' Command (Lude.Maybe Lude.Text)
cScriptPath = Lens.lens (scriptPath :: Command -> Lude.Maybe Lude.Text) (\s a -> s {scriptPath = a} :: Command)
{-# DEPRECATED cScriptPath "Use generic-lens or generic-optics with 'scriptPath' instead." #-}

-- | The name of the command.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cName :: Lens.Lens' Command (Lude.Maybe Lude.Text)
cName = Lens.lens (name :: Command -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Command)
{-# DEPRECATED cName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON Command where
  parseJSON =
    Lude.withObject
      "Command"
      ( \x ->
          Command'
            Lude.<$> (x Lude..:? "Args" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "ScriptPath")
            Lude.<*> (x Lude..:? "Name")
      )
