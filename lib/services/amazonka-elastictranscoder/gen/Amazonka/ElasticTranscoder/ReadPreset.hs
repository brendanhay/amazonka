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
-- Module      : Amazonka.ElasticTranscoder.ReadPreset
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The ReadPreset operation gets detailed information about a preset.
module Amazonka.ElasticTranscoder.ReadPreset
  ( -- * Creating a Request
    ReadPreset (..),
    newReadPreset,

    -- * Request Lenses
    readPreset_id,

    -- * Destructuring the Response
    ReadPresetResponse (..),
    newReadPresetResponse,

    -- * Response Lenses
    readPresetResponse_preset,
    readPresetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticTranscoder.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The @ReadPresetRequest@ structure.
--
-- /See:/ 'newReadPreset' smart constructor.
data ReadPreset = ReadPreset'
  { -- | The identifier of the preset for which you want to get detailed
    -- information.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReadPreset' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'readPreset_id' - The identifier of the preset for which you want to get detailed
-- information.
newReadPreset ::
  -- | 'id'
  Prelude.Text ->
  ReadPreset
newReadPreset pId_ = ReadPreset' {id = pId_}

-- | The identifier of the preset for which you want to get detailed
-- information.
readPreset_id :: Lens.Lens' ReadPreset Prelude.Text
readPreset_id = Lens.lens (\ReadPreset' {id} -> id) (\s@ReadPreset' {} a -> s {id = a} :: ReadPreset)

instance Core.AWSRequest ReadPreset where
  type AWSResponse ReadPreset = ReadPresetResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ReadPresetResponse'
            Prelude.<$> (x Data..?> "Preset")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ReadPreset where
  hashWithSalt _salt ReadPreset' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData ReadPreset where
  rnf ReadPreset' {..} = Prelude.rnf id

instance Data.ToHeaders ReadPreset where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ReadPreset where
  toPath ReadPreset' {..} =
    Prelude.mconcat
      ["/2012-09-25/presets/", Data.toBS id]

instance Data.ToQuery ReadPreset where
  toQuery = Prelude.const Prelude.mempty

-- | The @ReadPresetResponse@ structure.
--
-- /See:/ 'newReadPresetResponse' smart constructor.
data ReadPresetResponse = ReadPresetResponse'
  { -- | A section of the response body that provides information about the
    -- preset.
    preset :: Prelude.Maybe Preset,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReadPresetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'preset', 'readPresetResponse_preset' - A section of the response body that provides information about the
-- preset.
--
-- 'httpStatus', 'readPresetResponse_httpStatus' - The response's http status code.
newReadPresetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ReadPresetResponse
newReadPresetResponse pHttpStatus_ =
  ReadPresetResponse'
    { preset = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A section of the response body that provides information about the
-- preset.
readPresetResponse_preset :: Lens.Lens' ReadPresetResponse (Prelude.Maybe Preset)
readPresetResponse_preset = Lens.lens (\ReadPresetResponse' {preset} -> preset) (\s@ReadPresetResponse' {} a -> s {preset = a} :: ReadPresetResponse)

-- | The response's http status code.
readPresetResponse_httpStatus :: Lens.Lens' ReadPresetResponse Prelude.Int
readPresetResponse_httpStatus = Lens.lens (\ReadPresetResponse' {httpStatus} -> httpStatus) (\s@ReadPresetResponse' {} a -> s {httpStatus = a} :: ReadPresetResponse)

instance Prelude.NFData ReadPresetResponse where
  rnf ReadPresetResponse' {..} =
    Prelude.rnf preset
      `Prelude.seq` Prelude.rnf httpStatus
