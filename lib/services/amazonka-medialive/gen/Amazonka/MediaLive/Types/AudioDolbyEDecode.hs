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
-- Module      : Amazonka.MediaLive.Types.AudioDolbyEDecode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.AudioDolbyEDecode where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types.DolbyEProgramSelection
import qualified Amazonka.Prelude as Prelude

-- | Audio Dolby EDecode
--
-- /See:/ 'newAudioDolbyEDecode' smart constructor.
data AudioDolbyEDecode = AudioDolbyEDecode'
  { -- | Applies only to Dolby E. Enter the program ID (according to the metadata
    -- in the audio) of the Dolby E program to extract from the specified
    -- track. One program extracted per audio selector. To select multiple
    -- programs, create multiple selectors with the same Track and different
    -- Program numbers. “All channels” means to ignore the program IDs and
    -- include all the channels in this selector; useful if metadata is known
    -- to be incorrect.
    programSelection :: DolbyEProgramSelection
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AudioDolbyEDecode' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'programSelection', 'audioDolbyEDecode_programSelection' - Applies only to Dolby E. Enter the program ID (according to the metadata
-- in the audio) of the Dolby E program to extract from the specified
-- track. One program extracted per audio selector. To select multiple
-- programs, create multiple selectors with the same Track and different
-- Program numbers. “All channels” means to ignore the program IDs and
-- include all the channels in this selector; useful if metadata is known
-- to be incorrect.
newAudioDolbyEDecode ::
  -- | 'programSelection'
  DolbyEProgramSelection ->
  AudioDolbyEDecode
newAudioDolbyEDecode pProgramSelection_ =
  AudioDolbyEDecode'
    { programSelection =
        pProgramSelection_
    }

-- | Applies only to Dolby E. Enter the program ID (according to the metadata
-- in the audio) of the Dolby E program to extract from the specified
-- track. One program extracted per audio selector. To select multiple
-- programs, create multiple selectors with the same Track and different
-- Program numbers. “All channels” means to ignore the program IDs and
-- include all the channels in this selector; useful if metadata is known
-- to be incorrect.
audioDolbyEDecode_programSelection :: Lens.Lens' AudioDolbyEDecode DolbyEProgramSelection
audioDolbyEDecode_programSelection = Lens.lens (\AudioDolbyEDecode' {programSelection} -> programSelection) (\s@AudioDolbyEDecode' {} a -> s {programSelection = a} :: AudioDolbyEDecode)

instance Data.FromJSON AudioDolbyEDecode where
  parseJSON =
    Data.withObject
      "AudioDolbyEDecode"
      ( \x ->
          AudioDolbyEDecode'
            Prelude.<$> (x Data..: "programSelection")
      )

instance Prelude.Hashable AudioDolbyEDecode where
  hashWithSalt _salt AudioDolbyEDecode' {..} =
    _salt `Prelude.hashWithSalt` programSelection

instance Prelude.NFData AudioDolbyEDecode where
  rnf AudioDolbyEDecode' {..} =
    Prelude.rnf programSelection

instance Data.ToJSON AudioDolbyEDecode where
  toJSON AudioDolbyEDecode' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("programSelection" Data..= programSelection)
          ]
      )
