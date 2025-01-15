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
-- Module      : Amazonka.MediaConvert.GetPreset
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieve the JSON for a specific preset.
module Amazonka.MediaConvert.GetPreset
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConvert.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetPreset' smart constructor.
data GetPreset = GetPreset'
  { -- | The name of the preset.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  GetPreset
newGetPreset pName_ = GetPreset' {name = pName_}

-- | The name of the preset.
getPreset_name :: Lens.Lens' GetPreset Prelude.Text
getPreset_name = Lens.lens (\GetPreset' {name} -> name) (\s@GetPreset' {} a -> s {name = a} :: GetPreset)

instance Core.AWSRequest GetPreset where
  type AWSResponse GetPreset = GetPresetResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetPresetResponse'
            Prelude.<$> (x Data..?> "preset")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetPreset where
  hashWithSalt _salt GetPreset' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData GetPreset where
  rnf GetPreset' {..} = Prelude.rnf name

instance Data.ToHeaders GetPreset where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetPreset where
  toPath GetPreset' {..} =
    Prelude.mconcat
      ["/2017-08-29/presets/", Data.toBS name]

instance Data.ToQuery GetPreset where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetPresetResponse' smart constructor.
data GetPresetResponse = GetPresetResponse'
  { -- | A preset is a collection of preconfigured media conversion settings that
    -- you want MediaConvert to apply to the output during the conversion
    -- process.
    preset :: Prelude.Maybe Preset,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  GetPresetResponse
newGetPresetResponse pHttpStatus_ =
  GetPresetResponse'
    { preset = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A preset is a collection of preconfigured media conversion settings that
-- you want MediaConvert to apply to the output during the conversion
-- process.
getPresetResponse_preset :: Lens.Lens' GetPresetResponse (Prelude.Maybe Preset)
getPresetResponse_preset = Lens.lens (\GetPresetResponse' {preset} -> preset) (\s@GetPresetResponse' {} a -> s {preset = a} :: GetPresetResponse)

-- | The response's http status code.
getPresetResponse_httpStatus :: Lens.Lens' GetPresetResponse Prelude.Int
getPresetResponse_httpStatus = Lens.lens (\GetPresetResponse' {httpStatus} -> httpStatus) (\s@GetPresetResponse' {} a -> s {httpStatus = a} :: GetPresetResponse)

instance Prelude.NFData GetPresetResponse where
  rnf GetPresetResponse' {..} =
    Prelude.rnf preset `Prelude.seq`
      Prelude.rnf httpStatus
