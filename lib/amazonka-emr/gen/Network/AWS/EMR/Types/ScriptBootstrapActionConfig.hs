{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.ScriptBootstrapActionConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.ScriptBootstrapActionConfig
  ( ScriptBootstrapActionConfig (..),

    -- * Smart constructor
    mkScriptBootstrapActionConfig,

    -- * Lenses
    sbacArgs,
    sbacPath,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Configuration of the script to run during a bootstrap action.
--
-- /See:/ 'mkScriptBootstrapActionConfig' smart constructor.
data ScriptBootstrapActionConfig = ScriptBootstrapActionConfig'
  { -- | A list of command line arguments to pass to the bootstrap action script.
    args :: Lude.Maybe [Lude.Text],
    -- | Location of the script to run during a bootstrap action. Can be either a location in Amazon S3 or on a local file system.
    path :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ScriptBootstrapActionConfig' with the minimum fields required to make a request.
--
-- * 'args' - A list of command line arguments to pass to the bootstrap action script.
-- * 'path' - Location of the script to run during a bootstrap action. Can be either a location in Amazon S3 or on a local file system.
mkScriptBootstrapActionConfig ::
  -- | 'path'
  Lude.Text ->
  ScriptBootstrapActionConfig
mkScriptBootstrapActionConfig pPath_ =
  ScriptBootstrapActionConfig' {args = Lude.Nothing, path = pPath_}

-- | A list of command line arguments to pass to the bootstrap action script.
--
-- /Note:/ Consider using 'args' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbacArgs :: Lens.Lens' ScriptBootstrapActionConfig (Lude.Maybe [Lude.Text])
sbacArgs = Lens.lens (args :: ScriptBootstrapActionConfig -> Lude.Maybe [Lude.Text]) (\s a -> s {args = a} :: ScriptBootstrapActionConfig)
{-# DEPRECATED sbacArgs "Use generic-lens or generic-optics with 'args' instead." #-}

-- | Location of the script to run during a bootstrap action. Can be either a location in Amazon S3 or on a local file system.
--
-- /Note:/ Consider using 'path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbacPath :: Lens.Lens' ScriptBootstrapActionConfig Lude.Text
sbacPath = Lens.lens (path :: ScriptBootstrapActionConfig -> Lude.Text) (\s a -> s {path = a} :: ScriptBootstrapActionConfig)
{-# DEPRECATED sbacPath "Use generic-lens or generic-optics with 'path' instead." #-}

instance Lude.ToJSON ScriptBootstrapActionConfig where
  toJSON ScriptBootstrapActionConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [("Args" Lude..=) Lude.<$> args, Lude.Just ("Path" Lude..= path)]
      )
