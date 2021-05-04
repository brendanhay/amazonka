{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.Greengrass.Types.LoggerComponent
import Network.AWS.Greengrass.Types.LoggerLevel
import Network.AWS.Greengrass.Types.LoggerType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about a logger
--
-- /See:/ 'newGreengrassLogger' smart constructor.
data GreengrassLogger = GreengrassLogger'
  { -- | The amount of file space, in KB, to use if the local file system is used
    -- for logging purposes.
    space :: Prelude.Maybe Prelude.Int,
    -- | The type of log output which will be used.
    type' :: LoggerType,
    -- | The level of the logs.
    level :: LoggerLevel,
    -- | A descriptive or arbitrary ID for the logger. This value must be unique
    -- within the logger definition version. Max length is 128 characters with
    -- pattern \'\'[a-zA-Z0-9:_-]+\'\'.
    id :: Prelude.Text,
    -- | The component that will be subject to logging.
    component :: LoggerComponent
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'component'
  LoggerComponent ->
  GreengrassLogger
newGreengrassLogger pType_ pLevel_ pId_ pComponent_ =
  GreengrassLogger'
    { space = Prelude.Nothing,
      type' = pType_,
      level = pLevel_,
      id = pId_,
      component = pComponent_
    }

-- | The amount of file space, in KB, to use if the local file system is used
-- for logging purposes.
greengrassLogger_space :: Lens.Lens' GreengrassLogger (Prelude.Maybe Prelude.Int)
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
greengrassLogger_id :: Lens.Lens' GreengrassLogger Prelude.Text
greengrassLogger_id = Lens.lens (\GreengrassLogger' {id} -> id) (\s@GreengrassLogger' {} a -> s {id = a} :: GreengrassLogger)

-- | The component that will be subject to logging.
greengrassLogger_component :: Lens.Lens' GreengrassLogger LoggerComponent
greengrassLogger_component = Lens.lens (\GreengrassLogger' {component} -> component) (\s@GreengrassLogger' {} a -> s {component = a} :: GreengrassLogger)

instance Prelude.FromJSON GreengrassLogger where
  parseJSON =
    Prelude.withObject
      "GreengrassLogger"
      ( \x ->
          GreengrassLogger'
            Prelude.<$> (x Prelude..:? "Space")
            Prelude.<*> (x Prelude..: "Type")
            Prelude.<*> (x Prelude..: "Level")
            Prelude.<*> (x Prelude..: "Id")
            Prelude.<*> (x Prelude..: "Component")
      )

instance Prelude.Hashable GreengrassLogger

instance Prelude.NFData GreengrassLogger

instance Prelude.ToJSON GreengrassLogger where
  toJSON GreengrassLogger' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Space" Prelude..=) Prelude.<$> space,
            Prelude.Just ("Type" Prelude..= type'),
            Prelude.Just ("Level" Prelude..= level),
            Prelude.Just ("Id" Prelude..= id),
            Prelude.Just ("Component" Prelude..= component)
          ]
      )
