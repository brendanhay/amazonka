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
    comArgs,
    comScriptPath,
    comName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An entity describing an executable that runs on a cluster.
--
-- /See:/ 'mkCommand' smart constructor.
data Command = Command'
  { args :: Lude.Maybe [Lude.Text],
    scriptPath :: Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Command' with the minimum fields required to make a request.
--
-- * 'args' - Arguments for Amazon EMR to pass to the command for execution.
-- * 'name' - The name of the command.
-- * 'scriptPath' - The Amazon S3 location of the command script.
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
comArgs :: Lens.Lens' Command (Lude.Maybe [Lude.Text])
comArgs = Lens.lens (args :: Command -> Lude.Maybe [Lude.Text]) (\s a -> s {args = a} :: Command)
{-# DEPRECATED comArgs "Use generic-lens or generic-optics with 'args' instead." #-}

-- | The Amazon S3 location of the command script.
--
-- /Note:/ Consider using 'scriptPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
comScriptPath :: Lens.Lens' Command (Lude.Maybe Lude.Text)
comScriptPath = Lens.lens (scriptPath :: Command -> Lude.Maybe Lude.Text) (\s a -> s {scriptPath = a} :: Command)
{-# DEPRECATED comScriptPath "Use generic-lens or generic-optics with 'scriptPath' instead." #-}

-- | The name of the command.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
comName :: Lens.Lens' Command (Lude.Maybe Lude.Text)
comName = Lens.lens (name :: Command -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Command)
{-# DEPRECATED comName "Use generic-lens or generic-optics with 'name' instead." #-}

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
