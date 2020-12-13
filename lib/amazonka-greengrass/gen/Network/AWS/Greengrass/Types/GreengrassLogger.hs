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
    glSpace,
    glComponent,
    glId,
    glType,
    glLevel,
  )
where

import Network.AWS.Greengrass.Types.LoggerComponent
import Network.AWS.Greengrass.Types.LoggerLevel
import Network.AWS.Greengrass.Types.LoggerType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a logger
--
-- /See:/ 'mkGreengrassLogger' smart constructor.
data GreengrassLogger = GreengrassLogger'
  { -- | The amount of file space, in KB, to use if the local file system is used for logging purposes.
    space :: Lude.Maybe Lude.Int,
    -- | The component that will be subject to logging.
    component :: LoggerComponent,
    -- | A descriptive or arbitrary ID for the logger. This value must be unique within the logger definition version. Max length is 128 characters with pattern ''[a-zA-Z0-9:_-]+''.
    id :: Lude.Text,
    -- | The type of log output which will be used.
    type' :: LoggerType,
    -- | The level of the logs.
    level :: LoggerLevel
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GreengrassLogger' with the minimum fields required to make a request.
--
-- * 'space' - The amount of file space, in KB, to use if the local file system is used for logging purposes.
-- * 'component' - The component that will be subject to logging.
-- * 'id' - A descriptive or arbitrary ID for the logger. This value must be unique within the logger definition version. Max length is 128 characters with pattern ''[a-zA-Z0-9:_-]+''.
-- * 'type'' - The type of log output which will be used.
-- * 'level' - The level of the logs.
mkGreengrassLogger ::
  -- | 'component'
  LoggerComponent ->
  -- | 'id'
  Lude.Text ->
  -- | 'type''
  LoggerType ->
  -- | 'level'
  LoggerLevel ->
  GreengrassLogger
mkGreengrassLogger pComponent_ pId_ pType_ pLevel_ =
  GreengrassLogger'
    { space = Lude.Nothing,
      component = pComponent_,
      id = pId_,
      type' = pType_,
      level = pLevel_
    }

-- | The amount of file space, in KB, to use if the local file system is used for logging purposes.
--
-- /Note:/ Consider using 'space' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glSpace :: Lens.Lens' GreengrassLogger (Lude.Maybe Lude.Int)
glSpace = Lens.lens (space :: GreengrassLogger -> Lude.Maybe Lude.Int) (\s a -> s {space = a} :: GreengrassLogger)
{-# DEPRECATED glSpace "Use generic-lens or generic-optics with 'space' instead." #-}

-- | The component that will be subject to logging.
--
-- /Note:/ Consider using 'component' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glComponent :: Lens.Lens' GreengrassLogger LoggerComponent
glComponent = Lens.lens (component :: GreengrassLogger -> LoggerComponent) (\s a -> s {component = a} :: GreengrassLogger)
{-# DEPRECATED glComponent "Use generic-lens or generic-optics with 'component' instead." #-}

-- | A descriptive or arbitrary ID for the logger. This value must be unique within the logger definition version. Max length is 128 characters with pattern ''[a-zA-Z0-9:_-]+''.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glId :: Lens.Lens' GreengrassLogger Lude.Text
glId = Lens.lens (id :: GreengrassLogger -> Lude.Text) (\s a -> s {id = a} :: GreengrassLogger)
{-# DEPRECATED glId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The type of log output which will be used.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glType :: Lens.Lens' GreengrassLogger LoggerType
glType = Lens.lens (type' :: GreengrassLogger -> LoggerType) (\s a -> s {type' = a} :: GreengrassLogger)
{-# DEPRECATED glType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The level of the logs.
--
-- /Note:/ Consider using 'level' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glLevel :: Lens.Lens' GreengrassLogger LoggerLevel
glLevel = Lens.lens (level :: GreengrassLogger -> LoggerLevel) (\s a -> s {level = a} :: GreengrassLogger)
{-# DEPRECATED glLevel "Use generic-lens or generic-optics with 'level' instead." #-}

instance Lude.FromJSON GreengrassLogger where
  parseJSON =
    Lude.withObject
      "GreengrassLogger"
      ( \x ->
          GreengrassLogger'
            Lude.<$> (x Lude..:? "Space")
            Lude.<*> (x Lude..: "Component")
            Lude.<*> (x Lude..: "Id")
            Lude.<*> (x Lude..: "Type")
            Lude.<*> (x Lude..: "Level")
      )

instance Lude.ToJSON GreengrassLogger where
  toJSON GreengrassLogger' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Space" Lude..=) Lude.<$> space,
            Lude.Just ("Component" Lude..= component),
            Lude.Just ("Id" Lude..= id),
            Lude.Just ("Type" Lude..= type'),
            Lude.Just ("Level" Lude..= level)
          ]
      )
