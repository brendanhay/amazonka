{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MediaConvert.Types.TimedMetadataInsertion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.TimedMetadataInsertion where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConvert.Types.Id3Insertion
import qualified Amazonka.Prelude as Prelude

-- | Insert user-defined custom ID3 metadata (id3) at timecodes (timecode)
-- that you specify. In each output that you want to include this metadata,
-- you must set ID3 metadata (timedMetadata) to Passthrough (PASSTHROUGH).
--
-- /See:/ 'newTimedMetadataInsertion' smart constructor.
data TimedMetadataInsertion = TimedMetadataInsertion'
  { -- | Id3Insertions contains the array of Id3Insertion instances.
    id3Insertions :: Prelude.Maybe [Id3Insertion]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TimedMetadataInsertion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id3Insertions', 'timedMetadataInsertion_id3Insertions' - Id3Insertions contains the array of Id3Insertion instances.
newTimedMetadataInsertion ::
  TimedMetadataInsertion
newTimedMetadataInsertion =
  TimedMetadataInsertion'
    { id3Insertions =
        Prelude.Nothing
    }

-- | Id3Insertions contains the array of Id3Insertion instances.
timedMetadataInsertion_id3Insertions :: Lens.Lens' TimedMetadataInsertion (Prelude.Maybe [Id3Insertion])
timedMetadataInsertion_id3Insertions = Lens.lens (\TimedMetadataInsertion' {id3Insertions} -> id3Insertions) (\s@TimedMetadataInsertion' {} a -> s {id3Insertions = a} :: TimedMetadataInsertion) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON TimedMetadataInsertion where
  parseJSON =
    Data.withObject
      "TimedMetadataInsertion"
      ( \x ->
          TimedMetadataInsertion'
            Prelude.<$> (x Data..:? "id3Insertions" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable TimedMetadataInsertion where
  hashWithSalt _salt TimedMetadataInsertion' {..} =
    _salt `Prelude.hashWithSalt` id3Insertions

instance Prelude.NFData TimedMetadataInsertion where
  rnf TimedMetadataInsertion' {..} =
    Prelude.rnf id3Insertions

instance Data.ToJSON TimedMetadataInsertion where
  toJSON TimedMetadataInsertion' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("id3Insertions" Data..=)
              Prelude.<$> id3Insertions
          ]
      )
