{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.GreengrassLogger
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.GreengrassLogger where

import qualified Network.AWS.Core as Core
import Network.AWS.Greengrass.Types.LoggerComponent
import Network.AWS.Greengrass.Types.LoggerLevel
import Network.AWS.Greengrass.Types.LoggerType
import qualified Network.AWS.Lens as Lens

-- | Information about a logger
--
-- /See:/ 'newGreengrassLogger' smart constructor.
data GreengrassLogger = GreengrassLogger'
  { -- | The amount of file space, in KB, to use if the local file system is used
    -- for logging purposes.
    space :: Core.Maybe Core.Int,
    -- | The type of log output which will be used.
    type' :: LoggerType,
    -- | The level of the logs.
    level :: LoggerLevel,
    -- | A descriptive or arbitrary ID for the logger. This value must be unique
    -- within the logger definition version. Max length is 128 characters with
    -- pattern \'\'[a-zA-Z0-9:_-]+\'\'.
    id :: Core.Text,
    -- | The component that will be subject to logging.
    component :: LoggerComponent
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GreengrassLogger' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'space', 'greengrassLogger_space' - The amount of file space, in KB, to use if the local file system is used
-- for logging purposes.
--
-- 'type'', 'greengrassLogger_type' - The type of log output which will be used.
--
-- 'level', 'greengrassLogger_level' - The level of the logs.
--
-- 'id', 'greengrassLogger_id' - A descriptive or arbitrary ID for the logger. This value must be unique
-- within the logger definition version. Max length is 128 characters with
-- pattern \'\'[a-zA-Z0-9:_-]+\'\'.
--
-- 'component', 'greengrassLogger_component' - The component that will be subject to logging.
newGreengrassLogger ::
  -- | 'type''
  LoggerType ->
  -- | 'level'
  LoggerLevel ->
  -- | 'id'
  Core.Text ->
  -- | 'component'
  LoggerComponent ->
  GreengrassLogger
newGreengrassLogger pType_ pLevel_ pId_ pComponent_ =
  GreengrassLogger'
    { space = Core.Nothing,
      type' = pType_,
      level = pLevel_,
      id = pId_,
      component = pComponent_
    }

-- | The amount of file space, in KB, to use if the local file system is used
-- for logging purposes.
greengrassLogger_space :: Lens.Lens' GreengrassLogger (Core.Maybe Core.Int)
greengrassLogger_space = Lens.lens (\GreengrassLogger' {space} -> space) (\s@GreengrassLogger' {} a -> s {space = a} :: GreengrassLogger)

-- | The type of log output which will be used.
greengrassLogger_type :: Lens.Lens' GreengrassLogger LoggerType
greengrassLogger_type = Lens.lens (\GreengrassLogger' {type'} -> type') (\s@GreengrassLogger' {} a -> s {type' = a} :: GreengrassLogger)

-- | The level of the logs.
greengrassLogger_level :: Lens.Lens' GreengrassLogger LoggerLevel
greengrassLogger_level = Lens.lens (\GreengrassLogger' {level} -> level) (\s@GreengrassLogger' {} a -> s {level = a} :: GreengrassLogger)

-- | A descriptive or arbitrary ID for the logger. This value must be unique
-- within the logger definition version. Max length is 128 characters with
-- pattern \'\'[a-zA-Z0-9:_-]+\'\'.
greengrassLogger_id :: Lens.Lens' GreengrassLogger Core.Text
greengrassLogger_id = Lens.lens (\GreengrassLogger' {id} -> id) (\s@GreengrassLogger' {} a -> s {id = a} :: GreengrassLogger)

-- | The component that will be subject to logging.
greengrassLogger_component :: Lens.Lens' GreengrassLogger LoggerComponent
greengrassLogger_component = Lens.lens (\GreengrassLogger' {component} -> component) (\s@GreengrassLogger' {} a -> s {component = a} :: GreengrassLogger)

instance Core.FromJSON GreengrassLogger where
  parseJSON =
    Core.withObject
      "GreengrassLogger"
      ( \x ->
          GreengrassLogger'
            Core.<$> (x Core..:? "Space")
            Core.<*> (x Core..: "Type")
            Core.<*> (x Core..: "Level")
            Core.<*> (x Core..: "Id")
            Core.<*> (x Core..: "Component")
      )

instance Core.Hashable GreengrassLogger

instance Core.NFData GreengrassLogger

instance Core.ToJSON GreengrassLogger where
  toJSON GreengrassLogger' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Space" Core..=) Core.<$> space,
            Core.Just ("Type" Core..= type'),
            Core.Just ("Level" Core..= level),
            Core.Just ("Id" Core..= id),
            Core.Just ("Component" Core..= component)
          ]
      )
