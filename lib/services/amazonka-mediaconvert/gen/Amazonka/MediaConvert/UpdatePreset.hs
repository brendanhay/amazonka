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
-- Module      : Amazonka.MediaConvert.UpdatePreset
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modify one of your existing presets.
module Amazonka.MediaConvert.UpdatePreset
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConvert.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdatePreset' smart constructor.
data UpdatePreset = UpdatePreset'
  { -- | The new category for the preset, if you are changing it.
    category :: Prelude.Maybe Prelude.Text,
    -- | The new description for the preset, if you are changing it.
    description :: Prelude.Maybe Prelude.Text,
    -- | Settings for preset
    settings :: Prelude.Maybe PresetSettings,
    -- | The name of the preset you are modifying.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  UpdatePreset
newUpdatePreset pName_ =
  UpdatePreset'
    { category = Prelude.Nothing,
      description = Prelude.Nothing,
      settings = Prelude.Nothing,
      name = pName_
    }

-- | The new category for the preset, if you are changing it.
updatePreset_category :: Lens.Lens' UpdatePreset (Prelude.Maybe Prelude.Text)
updatePreset_category = Lens.lens (\UpdatePreset' {category} -> category) (\s@UpdatePreset' {} a -> s {category = a} :: UpdatePreset)

-- | The new description for the preset, if you are changing it.
updatePreset_description :: Lens.Lens' UpdatePreset (Prelude.Maybe Prelude.Text)
updatePreset_description = Lens.lens (\UpdatePreset' {description} -> description) (\s@UpdatePreset' {} a -> s {description = a} :: UpdatePreset)

-- | Settings for preset
updatePreset_settings :: Lens.Lens' UpdatePreset (Prelude.Maybe PresetSettings)
updatePreset_settings = Lens.lens (\UpdatePreset' {settings} -> settings) (\s@UpdatePreset' {} a -> s {settings = a} :: UpdatePreset)

-- | The name of the preset you are modifying.
updatePreset_name :: Lens.Lens' UpdatePreset Prelude.Text
updatePreset_name = Lens.lens (\UpdatePreset' {name} -> name) (\s@UpdatePreset' {} a -> s {name = a} :: UpdatePreset)

instance Core.AWSRequest UpdatePreset where
  type AWSResponse UpdatePreset = UpdatePresetResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdatePresetResponse'
            Prelude.<$> (x Data..?> "preset")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdatePreset where
  hashWithSalt _salt UpdatePreset' {..} =
    _salt `Prelude.hashWithSalt` category
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` settings
      `Prelude.hashWithSalt` name

instance Prelude.NFData UpdatePreset where
  rnf UpdatePreset' {..} =
    Prelude.rnf category
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf settings
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders UpdatePreset where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdatePreset where
  toJSON UpdatePreset' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("category" Data..=) Prelude.<$> category,
            ("description" Data..=) Prelude.<$> description,
            ("settings" Data..=) Prelude.<$> settings
          ]
      )

instance Data.ToPath UpdatePreset where
  toPath UpdatePreset' {..} =
    Prelude.mconcat
      ["/2017-08-29/presets/", Data.toBS name]

instance Data.ToQuery UpdatePreset where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdatePresetResponse' smart constructor.
data UpdatePresetResponse = UpdatePresetResponse'
  { -- | A preset is a collection of preconfigured media conversion settings that
    -- you want MediaConvert to apply to the output during the conversion
    -- process.
    preset :: Prelude.Maybe Preset,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  UpdatePresetResponse
newUpdatePresetResponse pHttpStatus_ =
  UpdatePresetResponse'
    { preset = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A preset is a collection of preconfigured media conversion settings that
-- you want MediaConvert to apply to the output during the conversion
-- process.
updatePresetResponse_preset :: Lens.Lens' UpdatePresetResponse (Prelude.Maybe Preset)
updatePresetResponse_preset = Lens.lens (\UpdatePresetResponse' {preset} -> preset) (\s@UpdatePresetResponse' {} a -> s {preset = a} :: UpdatePresetResponse)

-- | The response's http status code.
updatePresetResponse_httpStatus :: Lens.Lens' UpdatePresetResponse Prelude.Int
updatePresetResponse_httpStatus = Lens.lens (\UpdatePresetResponse' {httpStatus} -> httpStatus) (\s@UpdatePresetResponse' {} a -> s {httpStatus = a} :: UpdatePresetResponse)

instance Prelude.NFData UpdatePresetResponse where
  rnf UpdatePresetResponse' {..} =
    Prelude.rnf preset
      `Prelude.seq` Prelude.rnf httpStatus
