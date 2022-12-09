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
-- Module      : Amazonka.MGN.ArchiveWave
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Archive wave.
module Amazonka.MGN.ArchiveWave
  ( -- * Creating a Request
    ArchiveWave (..),
    newArchiveWave,

    -- * Request Lenses
    archiveWave_waveID,

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

-- | /See:/ 'newArchiveWave' smart constructor.
data ArchiveWave = ArchiveWave'
  { -- | Wave ID.
    waveID :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ArchiveWave' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'waveID', 'archiveWave_waveID' - Wave ID.
newArchiveWave ::
  -- | 'waveID'
  Prelude.Text ->
  ArchiveWave
newArchiveWave pWaveID_ =
  ArchiveWave' {waveID = pWaveID_}

-- | Wave ID.
archiveWave_waveID :: Lens.Lens' ArchiveWave Prelude.Text
archiveWave_waveID = Lens.lens (\ArchiveWave' {waveID} -> waveID) (\s@ArchiveWave' {} a -> s {waveID = a} :: ArchiveWave)

instance Core.AWSRequest ArchiveWave where
  type AWSResponse ArchiveWave = Wave
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable ArchiveWave where
  hashWithSalt _salt ArchiveWave' {..} =
    _salt `Prelude.hashWithSalt` waveID

instance Prelude.NFData ArchiveWave where
  rnf ArchiveWave' {..} = Prelude.rnf waveID

instance Data.ToHeaders ArchiveWave where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ArchiveWave where
  toJSON ArchiveWave' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("waveID" Data..= waveID)]
      )

instance Data.ToPath ArchiveWave where
  toPath = Prelude.const "/ArchiveWave"

instance Data.ToQuery ArchiveWave where
  toQuery = Prelude.const Prelude.mempty
