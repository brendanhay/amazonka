{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.GreengrassLogger
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.GreengrassLogger
  ( GreengrassLogger (..),

    -- * Smart constructor
    mkGreengrassLogger,

    -- * Lenses
    glType,
    glLevel,
    glId,
    glComponent,
    glSpace,
  )
where

import qualified Network.AWS.Greengrass.Types.LoggerComponent as Types
import qualified Network.AWS.Greengrass.Types.LoggerLevel as Types
import qualified Network.AWS.Greengrass.Types.LoggerType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a logger
--
-- /See:/ 'mkGreengrassLogger' smart constructor.
data GreengrassLogger = GreengrassLogger'
  { -- | The type of log output which will be used.
    type' :: Types.LoggerType,
    -- | The level of the logs.
    level :: Types.LoggerLevel,
    -- | A descriptive or arbitrary ID for the logger. This value must be unique within the logger definition version. Max length is 128 characters with pattern ''[a-zA-Z0-9:_-]+''.
    id :: Core.Text,
    -- | The component that will be subject to logging.
    component :: Types.LoggerComponent,
    -- | The amount of file space, in KB, to use if the local file system is used for logging purposes.
    space :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GreengrassLogger' value with any optional fields omitted.
mkGreengrassLogger ::
  -- | 'type\''
  Types.LoggerType ->
  -- | 'level'
  Types.LoggerLevel ->
  -- | 'id'
  Core.Text ->
  -- | 'component'
  Types.LoggerComponent ->
  GreengrassLogger
mkGreengrassLogger type' level id component =
  GreengrassLogger'
    { type',
      level,
      id,
      component,
      space = Core.Nothing
    }

-- | The type of log output which will be used.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glType :: Lens.Lens' GreengrassLogger Types.LoggerType
glType = Lens.field @"type'"
{-# DEPRECATED glType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The level of the logs.
--
-- /Note:/ Consider using 'level' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glLevel :: Lens.Lens' GreengrassLogger Types.LoggerLevel
glLevel = Lens.field @"level"
{-# DEPRECATED glLevel "Use generic-lens or generic-optics with 'level' instead." #-}

-- | A descriptive or arbitrary ID for the logger. This value must be unique within the logger definition version. Max length is 128 characters with pattern ''[a-zA-Z0-9:_-]+''.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glId :: Lens.Lens' GreengrassLogger Core.Text
glId = Lens.field @"id"
{-# DEPRECATED glId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The component that will be subject to logging.
--
-- /Note:/ Consider using 'component' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glComponent :: Lens.Lens' GreengrassLogger Types.LoggerComponent
glComponent = Lens.field @"component"
{-# DEPRECATED glComponent "Use generic-lens or generic-optics with 'component' instead." #-}

-- | The amount of file space, in KB, to use if the local file system is used for logging purposes.
--
-- /Note:/ Consider using 'space' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glSpace :: Lens.Lens' GreengrassLogger (Core.Maybe Core.Int)
glSpace = Lens.field @"space"
{-# DEPRECATED glSpace "Use generic-lens or generic-optics with 'space' instead." #-}

instance Core.FromJSON GreengrassLogger where
  toJSON GreengrassLogger {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Type" Core..= type'),
            Core.Just ("Level" Core..= level),
            Core.Just ("Id" Core..= id),
            Core.Just ("Component" Core..= component),
            ("Space" Core..=) Core.<$> space
          ]
      )

instance Core.FromJSON GreengrassLogger where
  parseJSON =
    Core.withObject "GreengrassLogger" Core.$
      \x ->
        GreengrassLogger'
          Core.<$> (x Core..: "Type")
          Core.<*> (x Core..: "Level")
          Core.<*> (x Core..: "Id")
          Core.<*> (x Core..: "Component")
          Core.<*> (x Core..:? "Space")
