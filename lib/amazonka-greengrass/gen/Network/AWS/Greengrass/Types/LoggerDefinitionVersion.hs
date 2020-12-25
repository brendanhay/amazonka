{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.LoggerDefinitionVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.LoggerDefinitionVersion
  ( LoggerDefinitionVersion (..),

    -- * Smart constructor
    mkLoggerDefinitionVersion,

    -- * Lenses
    ldvLoggers,
  )
where

import qualified Network.AWS.Greengrass.Types.GreengrassLogger as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a logger definition version.
--
-- /See:/ 'mkLoggerDefinitionVersion' smart constructor.
newtype LoggerDefinitionVersion = LoggerDefinitionVersion'
  { -- | A list of loggers.
    loggers :: Core.Maybe [Types.GreengrassLogger]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'LoggerDefinitionVersion' value with any optional fields omitted.
mkLoggerDefinitionVersion ::
  LoggerDefinitionVersion
mkLoggerDefinitionVersion =
  LoggerDefinitionVersion' {loggers = Core.Nothing}

-- | A list of loggers.
--
-- /Note:/ Consider using 'loggers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldvLoggers :: Lens.Lens' LoggerDefinitionVersion (Core.Maybe [Types.GreengrassLogger])
ldvLoggers = Lens.field @"loggers"
{-# DEPRECATED ldvLoggers "Use generic-lens or generic-optics with 'loggers' instead." #-}

instance Core.FromJSON LoggerDefinitionVersion where
  toJSON LoggerDefinitionVersion {..} =
    Core.object
      (Core.catMaybes [("Loggers" Core..=) Core.<$> loggers])

instance Core.FromJSON LoggerDefinitionVersion where
  parseJSON =
    Core.withObject "LoggerDefinitionVersion" Core.$
      \x -> LoggerDefinitionVersion' Core.<$> (x Core..:? "Loggers")
