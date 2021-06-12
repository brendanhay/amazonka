{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.UpdatePreset
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modify one of your existing presets.
module Network.AWS.MediaConvert.UpdatePreset
  ( -- * Creating a Request
    UpdatePreset (..),
    newUpdatePreset,

    -- * Request Lenses
    updatePreset_category,
    updatePreset_description,
    updatePreset_settings,
    updatePreset_name,

    -- * Destructuring the Response
    UpdatePresetResponse (..),
    newUpdatePresetResponse,

    -- * Response Lenses
    updatePresetResponse_preset,
    updatePresetResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdatePreset' smart constructor.
data UpdatePreset = UpdatePreset'
  { -- | The new category for the preset, if you are changing it.
    category :: Core.Maybe Core.Text,
    -- | The new description for the preset, if you are changing it.
    description :: Core.Maybe Core.Text,
    -- | Settings for preset
    settings :: Core.Maybe PresetSettings,
    -- | The name of the preset you are modifying.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdatePreset' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'category', 'updatePreset_category' - The new category for the preset, if you are changing it.
--
-- 'description', 'updatePreset_description' - The new description for the preset, if you are changing it.
--
-- 'settings', 'updatePreset_settings' - Settings for preset
--
-- 'name', 'updatePreset_name' - The name of the preset you are modifying.
newUpdatePreset ::
  -- | 'name'
  Core.Text ->
  UpdatePreset
newUpdatePreset pName_ =
  UpdatePreset'
    { category = Core.Nothing,
      description = Core.Nothing,
      settings = Core.Nothing,
      name = pName_
    }

-- | The new category for the preset, if you are changing it.
updatePreset_category :: Lens.Lens' UpdatePreset (Core.Maybe Core.Text)
updatePreset_category = Lens.lens (\UpdatePreset' {category} -> category) (\s@UpdatePreset' {} a -> s {category = a} :: UpdatePreset)

-- | The new description for the preset, if you are changing it.
updatePreset_description :: Lens.Lens' UpdatePreset (Core.Maybe Core.Text)
updatePreset_description = Lens.lens (\UpdatePreset' {description} -> description) (\s@UpdatePreset' {} a -> s {description = a} :: UpdatePreset)

-- | Settings for preset
updatePreset_settings :: Lens.Lens' UpdatePreset (Core.Maybe PresetSettings)
updatePreset_settings = Lens.lens (\UpdatePreset' {settings} -> settings) (\s@UpdatePreset' {} a -> s {settings = a} :: UpdatePreset)

-- | The name of the preset you are modifying.
updatePreset_name :: Lens.Lens' UpdatePreset Core.Text
updatePreset_name = Lens.lens (\UpdatePreset' {name} -> name) (\s@UpdatePreset' {} a -> s {name = a} :: UpdatePreset)

instance Core.AWSRequest UpdatePreset where
  type AWSResponse UpdatePreset = UpdatePresetResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdatePresetResponse'
            Core.<$> (x Core..?> "preset")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdatePreset

instance Core.NFData UpdatePreset

instance Core.ToHeaders UpdatePreset where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdatePreset where
  toJSON UpdatePreset' {..} =
    Core.object
      ( Core.catMaybes
          [ ("category" Core..=) Core.<$> category,
            ("description" Core..=) Core.<$> description,
            ("settings" Core..=) Core.<$> settings
          ]
      )

instance Core.ToPath UpdatePreset where
  toPath UpdatePreset' {..} =
    Core.mconcat
      ["/2017-08-29/presets/", Core.toBS name]

instance Core.ToQuery UpdatePreset where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdatePresetResponse' smart constructor.
data UpdatePresetResponse = UpdatePresetResponse'
  { -- | A preset is a collection of preconfigured media conversion settings that
    -- you want MediaConvert to apply to the output during the conversion
    -- process.
    preset :: Core.Maybe Preset,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdatePresetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'preset', 'updatePresetResponse_preset' - A preset is a collection of preconfigured media conversion settings that
-- you want MediaConvert to apply to the output during the conversion
-- process.
--
-- 'httpStatus', 'updatePresetResponse_httpStatus' - The response's http status code.
newUpdatePresetResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdatePresetResponse
newUpdatePresetResponse pHttpStatus_ =
  UpdatePresetResponse'
    { preset = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A preset is a collection of preconfigured media conversion settings that
-- you want MediaConvert to apply to the output during the conversion
-- process.
updatePresetResponse_preset :: Lens.Lens' UpdatePresetResponse (Core.Maybe Preset)
updatePresetResponse_preset = Lens.lens (\UpdatePresetResponse' {preset} -> preset) (\s@UpdatePresetResponse' {} a -> s {preset = a} :: UpdatePresetResponse)

-- | The response's http status code.
updatePresetResponse_httpStatus :: Lens.Lens' UpdatePresetResponse Core.Int
updatePresetResponse_httpStatus = Lens.lens (\UpdatePresetResponse' {httpStatus} -> httpStatus) (\s@UpdatePresetResponse' {} a -> s {httpStatus = a} :: UpdatePresetResponse)

instance Core.NFData UpdatePresetResponse
