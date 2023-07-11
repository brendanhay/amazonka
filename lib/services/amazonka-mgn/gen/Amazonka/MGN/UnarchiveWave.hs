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
-- Module      : Amazonka.MGN.UnarchiveWave
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Unarchive wave.
module Amazonka.MGN.UnarchiveWave
  ( -- * Creating a Request
    UnarchiveWave (..),
    newUnarchiveWave,

    -- * Request Lenses
    unarchiveWave_waveID,

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

-- | /See:/ 'newUnarchiveWave' smart constructor.
data UnarchiveWave = UnarchiveWave'
  { -- | Wave ID.
    waveID :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UnarchiveWave' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'waveID', 'unarchiveWave_waveID' - Wave ID.
newUnarchiveWave ::
  -- | 'waveID'
  Prelude.Text ->
  UnarchiveWave
newUnarchiveWave pWaveID_ =
  UnarchiveWave' {waveID = pWaveID_}

-- | Wave ID.
unarchiveWave_waveID :: Lens.Lens' UnarchiveWave Prelude.Text
unarchiveWave_waveID = Lens.lens (\UnarchiveWave' {waveID} -> waveID) (\s@UnarchiveWave' {} a -> s {waveID = a} :: UnarchiveWave)

instance Core.AWSRequest UnarchiveWave where
  type AWSResponse UnarchiveWave = Wave
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable UnarchiveWave where
  hashWithSalt _salt UnarchiveWave' {..} =
    _salt `Prelude.hashWithSalt` waveID

instance Prelude.NFData UnarchiveWave where
  rnf UnarchiveWave' {..} = Prelude.rnf waveID

instance Data.ToHeaders UnarchiveWave where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UnarchiveWave where
  toJSON UnarchiveWave' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("waveID" Data..= waveID)]
      )

instance Data.ToPath UnarchiveWave where
  toPath = Prelude.const "/UnarchiveWave"

instance Data.ToQuery UnarchiveWave where
  toQuery = Prelude.const Prelude.mempty
