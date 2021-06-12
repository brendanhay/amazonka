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
-- Module      : Network.AWS.MediaConvert.CreatePreset
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a new preset. For information about job templates see the User
-- Guide at
-- http:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/what-is.html
module Network.AWS.MediaConvert.CreatePreset
  ( -- * Creating a Request
    CreatePreset (..),
    newCreatePreset,

    -- * Request Lenses
    createPreset_category,
    createPreset_tags,
    createPreset_description,
    createPreset_settings,
    createPreset_name,

    -- * Destructuring the Response
    CreatePresetResponse (..),
    newCreatePresetResponse,

    -- * Response Lenses
    createPresetResponse_preset,
    createPresetResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreatePreset' smart constructor.
data CreatePreset = CreatePreset'
  { -- | Optional. A category for the preset you are creating.
    category :: Core.Maybe Core.Text,
    -- | The tags that you want to add to the resource. You can tag resources
    -- with a key-value pair or with only a key.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | Optional. A description of the preset you are creating.
    description :: Core.Maybe Core.Text,
    -- | Settings for preset
    settings :: PresetSettings,
    -- | The name of the preset you are creating.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreatePreset' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'category', 'createPreset_category' - Optional. A category for the preset you are creating.
--
-- 'tags', 'createPreset_tags' - The tags that you want to add to the resource. You can tag resources
-- with a key-value pair or with only a key.
--
-- 'description', 'createPreset_description' - Optional. A description of the preset you are creating.
--
-- 'settings', 'createPreset_settings' - Settings for preset
--
-- 'name', 'createPreset_name' - The name of the preset you are creating.
newCreatePreset ::
  -- | 'settings'
  PresetSettings ->
  -- | 'name'
  Core.Text ->
  CreatePreset
newCreatePreset pSettings_ pName_ =
  CreatePreset'
    { category = Core.Nothing,
      tags = Core.Nothing,
      description = Core.Nothing,
      settings = pSettings_,
      name = pName_
    }

-- | Optional. A category for the preset you are creating.
createPreset_category :: Lens.Lens' CreatePreset (Core.Maybe Core.Text)
createPreset_category = Lens.lens (\CreatePreset' {category} -> category) (\s@CreatePreset' {} a -> s {category = a} :: CreatePreset)

-- | The tags that you want to add to the resource. You can tag resources
-- with a key-value pair or with only a key.
createPreset_tags :: Lens.Lens' CreatePreset (Core.Maybe (Core.HashMap Core.Text Core.Text))
createPreset_tags = Lens.lens (\CreatePreset' {tags} -> tags) (\s@CreatePreset' {} a -> s {tags = a} :: CreatePreset) Core.. Lens.mapping Lens._Coerce

-- | Optional. A description of the preset you are creating.
createPreset_description :: Lens.Lens' CreatePreset (Core.Maybe Core.Text)
createPreset_description = Lens.lens (\CreatePreset' {description} -> description) (\s@CreatePreset' {} a -> s {description = a} :: CreatePreset)

-- | Settings for preset
createPreset_settings :: Lens.Lens' CreatePreset PresetSettings
createPreset_settings = Lens.lens (\CreatePreset' {settings} -> settings) (\s@CreatePreset' {} a -> s {settings = a} :: CreatePreset)

-- | The name of the preset you are creating.
createPreset_name :: Lens.Lens' CreatePreset Core.Text
createPreset_name = Lens.lens (\CreatePreset' {name} -> name) (\s@CreatePreset' {} a -> s {name = a} :: CreatePreset)

instance Core.AWSRequest CreatePreset where
  type AWSResponse CreatePreset = CreatePresetResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreatePresetResponse'
            Core.<$> (x Core..?> "preset")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreatePreset

instance Core.NFData CreatePreset

instance Core.ToHeaders CreatePreset where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreatePreset where
  toJSON CreatePreset' {..} =
    Core.object
      ( Core.catMaybes
          [ ("category" Core..=) Core.<$> category,
            ("tags" Core..=) Core.<$> tags,
            ("description" Core..=) Core.<$> description,
            Core.Just ("settings" Core..= settings),
            Core.Just ("name" Core..= name)
          ]
      )

instance Core.ToPath CreatePreset where
  toPath = Core.const "/2017-08-29/presets"

instance Core.ToQuery CreatePreset where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreatePresetResponse' smart constructor.
data CreatePresetResponse = CreatePresetResponse'
  { -- | A preset is a collection of preconfigured media conversion settings that
    -- you want MediaConvert to apply to the output during the conversion
    -- process.
    preset :: Core.Maybe Preset,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreatePresetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'preset', 'createPresetResponse_preset' - A preset is a collection of preconfigured media conversion settings that
-- you want MediaConvert to apply to the output during the conversion
-- process.
--
-- 'httpStatus', 'createPresetResponse_httpStatus' - The response's http status code.
newCreatePresetResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreatePresetResponse
newCreatePresetResponse pHttpStatus_ =
  CreatePresetResponse'
    { preset = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A preset is a collection of preconfigured media conversion settings that
-- you want MediaConvert to apply to the output during the conversion
-- process.
createPresetResponse_preset :: Lens.Lens' CreatePresetResponse (Core.Maybe Preset)
createPresetResponse_preset = Lens.lens (\CreatePresetResponse' {preset} -> preset) (\s@CreatePresetResponse' {} a -> s {preset = a} :: CreatePresetResponse)

-- | The response's http status code.
createPresetResponse_httpStatus :: Lens.Lens' CreatePresetResponse Core.Int
createPresetResponse_httpStatus = Lens.lens (\CreatePresetResponse' {httpStatus} -> httpStatus) (\s@CreatePresetResponse' {} a -> s {httpStatus = a} :: CreatePresetResponse)

instance Core.NFData CreatePresetResponse
