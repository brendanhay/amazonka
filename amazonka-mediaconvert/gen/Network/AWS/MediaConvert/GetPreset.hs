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
-- Module      : Network.AWS.MediaConvert.GetPreset
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieve the JSON for a specific preset.
module Network.AWS.MediaConvert.GetPreset
  ( -- * Creating a Request
    GetPreset (..),
    newGetPreset,

    -- * Request Lenses
    getPreset_name,

    -- * Destructuring the Response
    GetPresetResponse (..),
    newGetPresetResponse,

    -- * Response Lenses
    getPresetResponse_preset,
    getPresetResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetPreset' smart constructor.
data GetPreset = GetPreset'
  { -- | The name of the preset.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetPreset' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'getPreset_name' - The name of the preset.
newGetPreset ::
  -- | 'name'
  Core.Text ->
  GetPreset
newGetPreset pName_ = GetPreset' {name = pName_}

-- | The name of the preset.
getPreset_name :: Lens.Lens' GetPreset Core.Text
getPreset_name = Lens.lens (\GetPreset' {name} -> name) (\s@GetPreset' {} a -> s {name = a} :: GetPreset)

instance Core.AWSRequest GetPreset where
  type AWSResponse GetPreset = GetPresetResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetPresetResponse'
            Core.<$> (x Core..?> "preset")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetPreset

instance Core.NFData GetPreset

instance Core.ToHeaders GetPreset where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath GetPreset where
  toPath GetPreset' {..} =
    Core.mconcat
      ["/2017-08-29/presets/", Core.toBS name]

instance Core.ToQuery GetPreset where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetPresetResponse' smart constructor.
data GetPresetResponse = GetPresetResponse'
  { -- | A preset is a collection of preconfigured media conversion settings that
    -- you want MediaConvert to apply to the output during the conversion
    -- process.
    preset :: Core.Maybe Preset,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetPresetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'preset', 'getPresetResponse_preset' - A preset is a collection of preconfigured media conversion settings that
-- you want MediaConvert to apply to the output during the conversion
-- process.
--
-- 'httpStatus', 'getPresetResponse_httpStatus' - The response's http status code.
newGetPresetResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetPresetResponse
newGetPresetResponse pHttpStatus_ =
  GetPresetResponse'
    { preset = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A preset is a collection of preconfigured media conversion settings that
-- you want MediaConvert to apply to the output during the conversion
-- process.
getPresetResponse_preset :: Lens.Lens' GetPresetResponse (Core.Maybe Preset)
getPresetResponse_preset = Lens.lens (\GetPresetResponse' {preset} -> preset) (\s@GetPresetResponse' {} a -> s {preset = a} :: GetPresetResponse)

-- | The response's http status code.
getPresetResponse_httpStatus :: Lens.Lens' GetPresetResponse Core.Int
getPresetResponse_httpStatus = Lens.lens (\GetPresetResponse' {httpStatus} -> httpStatus) (\s@GetPresetResponse' {} a -> s {httpStatus = a} :: GetPresetResponse)

instance Core.NFData GetPresetResponse
