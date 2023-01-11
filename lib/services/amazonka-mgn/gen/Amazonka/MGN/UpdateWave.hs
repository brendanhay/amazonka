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
-- Module      : Amazonka.MGN.UpdateWave
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update wave.
module Amazonka.MGN.UpdateWave
  ( -- * Creating a Request
    UpdateWave (..),
    newUpdateWave,

    -- * Request Lenses
    updateWave_description,
    updateWave_name,
    updateWave_waveID,

    -- * Destructuring the Response
    Wave (..),
    newWave,

    -- * Response Lenses
    wave_arn,
    wave_creationDateTime,
    wave_description,
    wave_isArchived,
    wave_lastModifiedDateTime,
    wave_name,
    wave_tags,
    wave_waveAggregatedStatus,
    wave_waveID,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MGN.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateWave' smart constructor.
data UpdateWave = UpdateWave'
  { -- | Wave description.
    description :: Prelude.Maybe Prelude.Text,
    -- | Wave name.
    name :: Prelude.Maybe Prelude.Text,
    -- | Wave ID.
    waveID :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateWave' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateWave_description' - Wave description.
--
-- 'name', 'updateWave_name' - Wave name.
--
-- 'waveID', 'updateWave_waveID' - Wave ID.
newUpdateWave ::
  -- | 'waveID'
  Prelude.Text ->
  UpdateWave
newUpdateWave pWaveID_ =
  UpdateWave'
    { description = Prelude.Nothing,
      name = Prelude.Nothing,
      waveID = pWaveID_
    }

-- | Wave description.
updateWave_description :: Lens.Lens' UpdateWave (Prelude.Maybe Prelude.Text)
updateWave_description = Lens.lens (\UpdateWave' {description} -> description) (\s@UpdateWave' {} a -> s {description = a} :: UpdateWave)

-- | Wave name.
updateWave_name :: Lens.Lens' UpdateWave (Prelude.Maybe Prelude.Text)
updateWave_name = Lens.lens (\UpdateWave' {name} -> name) (\s@UpdateWave' {} a -> s {name = a} :: UpdateWave)

-- | Wave ID.
updateWave_waveID :: Lens.Lens' UpdateWave Prelude.Text
updateWave_waveID = Lens.lens (\UpdateWave' {waveID} -> waveID) (\s@UpdateWave' {} a -> s {waveID = a} :: UpdateWave)

instance Core.AWSRequest UpdateWave where
  type AWSResponse UpdateWave = Wave
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable UpdateWave where
  hashWithSalt _salt UpdateWave' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` waveID

instance Prelude.NFData UpdateWave where
  rnf UpdateWave' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf waveID

instance Data.ToHeaders UpdateWave where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateWave where
  toJSON UpdateWave' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("description" Data..=) Prelude.<$> description,
            ("name" Data..=) Prelude.<$> name,
            Prelude.Just ("waveID" Data..= waveID)
          ]
      )

instance Data.ToPath UpdateWave where
  toPath = Prelude.const "/UpdateWave"

instance Data.ToQuery UpdateWave where
  toQuery = Prelude.const Prelude.mempty
