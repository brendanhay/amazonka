{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.MediaConvert.Types.TimedMetadataInsertion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.TimedMetadataInsertion where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.Id3Insertion
import qualified Network.AWS.Prelude as Prelude

-- | Enable Timed metadata insertion (TimedMetadataInsertion) to include ID3
-- tags in any HLS outputs. To include timed metadata, you must enable it
-- here, enable it in each output container, and specify tags and timecodes
-- in ID3 insertion (Id3Insertion) objects.
--
-- /See:/ 'newTimedMetadataInsertion' smart constructor.
data TimedMetadataInsertion = TimedMetadataInsertion'
  { -- | Id3Insertions contains the array of Id3Insertion instances.
    id3Insertions :: Prelude.Maybe [Id3Insertion]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
timedMetadataInsertion_id3Insertions = Lens.lens (\TimedMetadataInsertion' {id3Insertions} -> id3Insertions) (\s@TimedMetadataInsertion' {} a -> s {id3Insertions = a} :: TimedMetadataInsertion) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON TimedMetadataInsertion where
  parseJSON =
    Prelude.withObject
      "TimedMetadataInsertion"
      ( \x ->
          TimedMetadataInsertion'
            Prelude.<$> ( x Prelude..:? "id3Insertions"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable TimedMetadataInsertion

instance Prelude.NFData TimedMetadataInsertion

instance Prelude.ToJSON TimedMetadataInsertion where
  toJSON TimedMetadataInsertion' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("id3Insertions" Prelude..=)
              Prelude.<$> id3Insertions
          ]
      )
