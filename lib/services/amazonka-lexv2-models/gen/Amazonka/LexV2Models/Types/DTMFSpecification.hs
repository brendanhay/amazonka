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
-- Module      : Amazonka.LexV2Models.Types.DTMFSpecification
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.DTMFSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the DTMF input specifications.
--
-- /See:/ 'newDTMFSpecification' smart constructor.
data DTMFSpecification = DTMFSpecification'
  { -- | The maximum number of DTMF digits allowed in an utterance.
    maxLength :: Prelude.Natural,
    -- | How long the bot should wait after the last DTMF character input before
    -- assuming that the input has concluded.
    endTimeoutMs :: Prelude.Natural,
    -- | The DTMF character that clears the accumulated DTMF digits and
    -- immediately ends the input.
    deletionCharacter :: Prelude.Text,
    -- | The DTMF character that immediately ends input. If the user does not
    -- press this character, the input ends after the end timeout.
    endCharacter :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DTMFSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxLength', 'dTMFSpecification_maxLength' - The maximum number of DTMF digits allowed in an utterance.
--
-- 'endTimeoutMs', 'dTMFSpecification_endTimeoutMs' - How long the bot should wait after the last DTMF character input before
-- assuming that the input has concluded.
--
-- 'deletionCharacter', 'dTMFSpecification_deletionCharacter' - The DTMF character that clears the accumulated DTMF digits and
-- immediately ends the input.
--
-- 'endCharacter', 'dTMFSpecification_endCharacter' - The DTMF character that immediately ends input. If the user does not
-- press this character, the input ends after the end timeout.
newDTMFSpecification ::
  -- | 'maxLength'
  Prelude.Natural ->
  -- | 'endTimeoutMs'
  Prelude.Natural ->
  -- | 'deletionCharacter'
  Prelude.Text ->
  -- | 'endCharacter'
  Prelude.Text ->
  DTMFSpecification
newDTMFSpecification
  pMaxLength_
  pEndTimeoutMs_
  pDeletionCharacter_
  pEndCharacter_ =
    DTMFSpecification'
      { maxLength = pMaxLength_,
        endTimeoutMs = pEndTimeoutMs_,
        deletionCharacter = pDeletionCharacter_,
        endCharacter = pEndCharacter_
      }

-- | The maximum number of DTMF digits allowed in an utterance.
dTMFSpecification_maxLength :: Lens.Lens' DTMFSpecification Prelude.Natural
dTMFSpecification_maxLength = Lens.lens (\DTMFSpecification' {maxLength} -> maxLength) (\s@DTMFSpecification' {} a -> s {maxLength = a} :: DTMFSpecification)

-- | How long the bot should wait after the last DTMF character input before
-- assuming that the input has concluded.
dTMFSpecification_endTimeoutMs :: Lens.Lens' DTMFSpecification Prelude.Natural
dTMFSpecification_endTimeoutMs = Lens.lens (\DTMFSpecification' {endTimeoutMs} -> endTimeoutMs) (\s@DTMFSpecification' {} a -> s {endTimeoutMs = a} :: DTMFSpecification)

-- | The DTMF character that clears the accumulated DTMF digits and
-- immediately ends the input.
dTMFSpecification_deletionCharacter :: Lens.Lens' DTMFSpecification Prelude.Text
dTMFSpecification_deletionCharacter = Lens.lens (\DTMFSpecification' {deletionCharacter} -> deletionCharacter) (\s@DTMFSpecification' {} a -> s {deletionCharacter = a} :: DTMFSpecification)

-- | The DTMF character that immediately ends input. If the user does not
-- press this character, the input ends after the end timeout.
dTMFSpecification_endCharacter :: Lens.Lens' DTMFSpecification Prelude.Text
dTMFSpecification_endCharacter = Lens.lens (\DTMFSpecification' {endCharacter} -> endCharacter) (\s@DTMFSpecification' {} a -> s {endCharacter = a} :: DTMFSpecification)

instance Data.FromJSON DTMFSpecification where
  parseJSON =
    Data.withObject
      "DTMFSpecification"
      ( \x ->
          DTMFSpecification'
            Prelude.<$> (x Data..: "maxLength")
            Prelude.<*> (x Data..: "endTimeoutMs")
            Prelude.<*> (x Data..: "deletionCharacter")
            Prelude.<*> (x Data..: "endCharacter")
      )

instance Prelude.Hashable DTMFSpecification where
  hashWithSalt _salt DTMFSpecification' {..} =
    _salt `Prelude.hashWithSalt` maxLength
      `Prelude.hashWithSalt` endTimeoutMs
      `Prelude.hashWithSalt` deletionCharacter
      `Prelude.hashWithSalt` endCharacter

instance Prelude.NFData DTMFSpecification where
  rnf DTMFSpecification' {..} =
    Prelude.rnf maxLength
      `Prelude.seq` Prelude.rnf endTimeoutMs
      `Prelude.seq` Prelude.rnf deletionCharacter
      `Prelude.seq` Prelude.rnf endCharacter

instance Data.ToJSON DTMFSpecification where
  toJSON DTMFSpecification' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("maxLength" Data..= maxLength),
            Prelude.Just ("endTimeoutMs" Data..= endTimeoutMs),
            Prelude.Just
              ("deletionCharacter" Data..= deletionCharacter),
            Prelude.Just ("endCharacter" Data..= endCharacter)
          ]
      )
