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
    sbacPath,
    sbacArgs,
  )
where

import qualified Network.AWS.EMR.Types.XmlString as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Configuration of the script to run during a bootstrap action.
--
-- /See:/ 'mkScriptBootstrapActionConfig' smart constructor.
data ScriptBootstrapActionConfig = ScriptBootstrapActionConfig'
  { -- | Location of the script to run during a bootstrap action. Can be either a location in Amazon S3 or on a local file system.
    path :: Types.XmlString,
    -- | A list of command line arguments to pass to the bootstrap action script.
    args :: Core.Maybe [Types.XmlString]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ScriptBootstrapActionConfig' value with any optional fields omitted.
mkScriptBootstrapActionConfig ::
  -- | 'path'
  Types.XmlString ->
  ScriptBootstrapActionConfig
mkScriptBootstrapActionConfig path =
  ScriptBootstrapActionConfig' {path, args = Core.Nothing}

-- | Location of the script to run during a bootstrap action. Can be either a location in Amazon S3 or on a local file system.
--
-- /Note:/ Consider using 'path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbacPath :: Lens.Lens' ScriptBootstrapActionConfig Types.XmlString
sbacPath = Lens.field @"path"
{-# DEPRECATED sbacPath "Use generic-lens or generic-optics with 'path' instead." #-}

-- | A list of command line arguments to pass to the bootstrap action script.
--
-- /Note:/ Consider using 'args' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbacArgs :: Lens.Lens' ScriptBootstrapActionConfig (Core.Maybe [Types.XmlString])
sbacArgs = Lens.field @"args"
{-# DEPRECATED sbacArgs "Use generic-lens or generic-optics with 'args' instead." #-}

instance Core.FromJSON ScriptBootstrapActionConfig where
  toJSON ScriptBootstrapActionConfig {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("Path" Core..= path), ("Args" Core..=) Core.<$> args]
      )
