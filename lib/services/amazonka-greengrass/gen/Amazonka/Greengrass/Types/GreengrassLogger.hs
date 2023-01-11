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
-- Module      : Amazonka.Greengrass.Types.GreengrassLogger
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Greengrass.Types.GreengrassLogger where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Greengrass.Types.LoggerComponent
import Amazonka.Greengrass.Types.LoggerLevel
import Amazonka.Greengrass.Types.LoggerType
import qualified Amazonka.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Data.FromJSON GreengrassLogger where
  parseJSON =
    Data.withObject
      "GreengrassLogger"
      ( \x ->
          GreengrassLogger'
            Prelude.<$> (x Data..:? "Space")
            Prelude.<*> (x Data..: "Type")
            Prelude.<*> (x Data..: "Level")
            Prelude.<*> (x Data..: "Id")
            Prelude.<*> (x Data..: "Component")
      )

instance Prelude.Hashable GreengrassLogger where
  hashWithSalt _salt GreengrassLogger' {..} =
    _salt `Prelude.hashWithSalt` space
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` level
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` component

instance Prelude.NFData GreengrassLogger where
  rnf GreengrassLogger' {..} =
    Prelude.rnf space
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf level
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf component

instance Data.ToJSON GreengrassLogger where
  toJSON GreengrassLogger' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Space" Data..=) Prelude.<$> space,
            Prelude.Just ("Type" Data..= type'),
            Prelude.Just ("Level" Data..= level),
            Prelude.Just ("Id" Data..= id),
            Prelude.Just ("Component" Data..= component)
          ]
      )
