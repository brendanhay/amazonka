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
-- Module      : Network.AWS.MediaConvert.Types.Preset
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Preset where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.PresetSettings
import Network.AWS.MediaConvert.Types.Type

-- | A preset is a collection of preconfigured media conversion settings that
-- you want MediaConvert to apply to the output during the conversion
-- process.
--
-- /See:/ 'newPreset' smart constructor.
data Preset = Preset'
  { -- | An optional category you create to organize your presets.
    category :: Core.Maybe Core.Text,
    -- | An identifier for this resource that is unique within all of AWS.
    arn :: Core.Maybe Core.Text,
    -- | The timestamp in epoch seconds for preset creation.
    createdAt :: Core.Maybe Core.POSIX,
    -- | The timestamp in epoch seconds when the preset was last updated.
    lastUpdated :: Core.Maybe Core.POSIX,
    -- | An optional description you create for each preset.
    description :: Core.Maybe Core.Text,
    -- | A preset can be of two types: system or custom. System or built-in
    -- preset can\'t be modified or deleted by the user.
    type' :: Core.Maybe Type,
    -- | Settings for preset
    settings :: PresetSettings,
    -- | A name you create for each preset. Each name must be unique within your
    -- account.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Preset' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'category', 'preset_category' - An optional category you create to organize your presets.
--
-- 'arn', 'preset_arn' - An identifier for this resource that is unique within all of AWS.
--
-- 'createdAt', 'preset_createdAt' - The timestamp in epoch seconds for preset creation.
--
-- 'lastUpdated', 'preset_lastUpdated' - The timestamp in epoch seconds when the preset was last updated.
--
-- 'description', 'preset_description' - An optional description you create for each preset.
--
-- 'type'', 'preset_type' - A preset can be of two types: system or custom. System or built-in
-- preset can\'t be modified or deleted by the user.
--
-- 'settings', 'preset_settings' - Settings for preset
--
-- 'name', 'preset_name' - A name you create for each preset. Each name must be unique within your
-- account.
newPreset ::
  -- | 'settings'
  PresetSettings ->
  -- | 'name'
  Core.Text ->
  Preset
newPreset pSettings_ pName_ =
  Preset'
    { category = Core.Nothing,
      arn = Core.Nothing,
      createdAt = Core.Nothing,
      lastUpdated = Core.Nothing,
      description = Core.Nothing,
      type' = Core.Nothing,
      settings = pSettings_,
      name = pName_
    }

-- | An optional category you create to organize your presets.
preset_category :: Lens.Lens' Preset (Core.Maybe Core.Text)
preset_category = Lens.lens (\Preset' {category} -> category) (\s@Preset' {} a -> s {category = a} :: Preset)

-- | An identifier for this resource that is unique within all of AWS.
preset_arn :: Lens.Lens' Preset (Core.Maybe Core.Text)
preset_arn = Lens.lens (\Preset' {arn} -> arn) (\s@Preset' {} a -> s {arn = a} :: Preset)

-- | The timestamp in epoch seconds for preset creation.
preset_createdAt :: Lens.Lens' Preset (Core.Maybe Core.UTCTime)
preset_createdAt = Lens.lens (\Preset' {createdAt} -> createdAt) (\s@Preset' {} a -> s {createdAt = a} :: Preset) Core.. Lens.mapping Core._Time

-- | The timestamp in epoch seconds when the preset was last updated.
preset_lastUpdated :: Lens.Lens' Preset (Core.Maybe Core.UTCTime)
preset_lastUpdated = Lens.lens (\Preset' {lastUpdated} -> lastUpdated) (\s@Preset' {} a -> s {lastUpdated = a} :: Preset) Core.. Lens.mapping Core._Time

-- | An optional description you create for each preset.
preset_description :: Lens.Lens' Preset (Core.Maybe Core.Text)
preset_description = Lens.lens (\Preset' {description} -> description) (\s@Preset' {} a -> s {description = a} :: Preset)

-- | A preset can be of two types: system or custom. System or built-in
-- preset can\'t be modified or deleted by the user.
preset_type :: Lens.Lens' Preset (Core.Maybe Type)
preset_type = Lens.lens (\Preset' {type'} -> type') (\s@Preset' {} a -> s {type' = a} :: Preset)

-- | Settings for preset
preset_settings :: Lens.Lens' Preset PresetSettings
preset_settings = Lens.lens (\Preset' {settings} -> settings) (\s@Preset' {} a -> s {settings = a} :: Preset)

-- | A name you create for each preset. Each name must be unique within your
-- account.
preset_name :: Lens.Lens' Preset Core.Text
preset_name = Lens.lens (\Preset' {name} -> name) (\s@Preset' {} a -> s {name = a} :: Preset)

instance Core.FromJSON Preset where
  parseJSON =
    Core.withObject
      "Preset"
      ( \x ->
          Preset'
            Core.<$> (x Core..:? "category")
            Core.<*> (x Core..:? "arn")
            Core.<*> (x Core..:? "createdAt")
            Core.<*> (x Core..:? "lastUpdated")
            Core.<*> (x Core..:? "description")
            Core.<*> (x Core..:? "type")
            Core.<*> (x Core..: "settings")
            Core.<*> (x Core..: "name")
      )

instance Core.Hashable Preset

instance Core.NFData Preset
