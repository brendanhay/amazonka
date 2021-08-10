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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreatePreset' smart constructor.
data CreatePreset = CreatePreset'
  { -- | Optional. A category for the preset you are creating.
    category :: Prelude.Maybe Prelude.Text,
    -- | The tags that you want to add to the resource. You can tag resources
    -- with a key-value pair or with only a key.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Optional. A description of the preset you are creating.
    description :: Prelude.Maybe Prelude.Text,
    -- | Settings for preset
    settings :: PresetSettings,
    -- | The name of the preset you are creating.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  CreatePreset
newCreatePreset pSettings_ pName_ =
  CreatePreset'
    { category = Prelude.Nothing,
      tags = Prelude.Nothing,
      description = Prelude.Nothing,
      settings = pSettings_,
      name = pName_
    }

-- | Optional. A category for the preset you are creating.
createPreset_category :: Lens.Lens' CreatePreset (Prelude.Maybe Prelude.Text)
createPreset_category = Lens.lens (\CreatePreset' {category} -> category) (\s@CreatePreset' {} a -> s {category = a} :: CreatePreset)

-- | The tags that you want to add to the resource. You can tag resources
-- with a key-value pair or with only a key.
createPreset_tags :: Lens.Lens' CreatePreset (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createPreset_tags = Lens.lens (\CreatePreset' {tags} -> tags) (\s@CreatePreset' {} a -> s {tags = a} :: CreatePreset) Prelude.. Lens.mapping Lens._Coerce

-- | Optional. A description of the preset you are creating.
createPreset_description :: Lens.Lens' CreatePreset (Prelude.Maybe Prelude.Text)
createPreset_description = Lens.lens (\CreatePreset' {description} -> description) (\s@CreatePreset' {} a -> s {description = a} :: CreatePreset)

-- | Settings for preset
createPreset_settings :: Lens.Lens' CreatePreset PresetSettings
createPreset_settings = Lens.lens (\CreatePreset' {settings} -> settings) (\s@CreatePreset' {} a -> s {settings = a} :: CreatePreset)

-- | The name of the preset you are creating.
createPreset_name :: Lens.Lens' CreatePreset Prelude.Text
createPreset_name = Lens.lens (\CreatePreset' {name} -> name) (\s@CreatePreset' {} a -> s {name = a} :: CreatePreset)

instance Core.AWSRequest CreatePreset where
  type AWSResponse CreatePreset = CreatePresetResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreatePresetResponse'
            Prelude.<$> (x Core..?> "preset")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreatePreset

instance Prelude.NFData CreatePreset

instance Core.ToHeaders CreatePreset where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreatePreset where
  toJSON CreatePreset' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("category" Core..=) Prelude.<$> category,
            ("tags" Core..=) Prelude.<$> tags,
            ("description" Core..=) Prelude.<$> description,
            Prelude.Just ("settings" Core..= settings),
            Prelude.Just ("name" Core..= name)
          ]
      )

instance Core.ToPath CreatePreset where
  toPath = Prelude.const "/2017-08-29/presets"

instance Core.ToQuery CreatePreset where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreatePresetResponse' smart constructor.
data CreatePresetResponse = CreatePresetResponse'
  { -- | A preset is a collection of preconfigured media conversion settings that
    -- you want MediaConvert to apply to the output during the conversion
    -- process.
    preset :: Prelude.Maybe Preset,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  CreatePresetResponse
newCreatePresetResponse pHttpStatus_ =
  CreatePresetResponse'
    { preset = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A preset is a collection of preconfigured media conversion settings that
-- you want MediaConvert to apply to the output during the conversion
-- process.
createPresetResponse_preset :: Lens.Lens' CreatePresetResponse (Prelude.Maybe Preset)
createPresetResponse_preset = Lens.lens (\CreatePresetResponse' {preset} -> preset) (\s@CreatePresetResponse' {} a -> s {preset = a} :: CreatePresetResponse)

-- | The response's http status code.
createPresetResponse_httpStatus :: Lens.Lens' CreatePresetResponse Prelude.Int
createPresetResponse_httpStatus = Lens.lens (\CreatePresetResponse' {httpStatus} -> httpStatus) (\s@CreatePresetResponse' {} a -> s {httpStatus = a} :: CreatePresetResponse)

instance Prelude.NFData CreatePresetResponse
