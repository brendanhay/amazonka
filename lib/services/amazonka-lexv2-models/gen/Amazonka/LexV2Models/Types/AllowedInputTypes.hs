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
-- Module      : Amazonka.LexV2Models.Types.AllowedInputTypes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.AllowedInputTypes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the allowed input types.
--
-- /See:/ 'newAllowedInputTypes' smart constructor.
data AllowedInputTypes = AllowedInputTypes'
  { -- | Indicates whether audio input is allowed.
    allowAudioInput :: Prelude.Bool,
    -- | Indicates whether DTMF input is allowed.
    allowDTMFInput :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AllowedInputTypes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allowAudioInput', 'allowedInputTypes_allowAudioInput' - Indicates whether audio input is allowed.
--
-- 'allowDTMFInput', 'allowedInputTypes_allowDTMFInput' - Indicates whether DTMF input is allowed.
newAllowedInputTypes ::
  -- | 'allowAudioInput'
  Prelude.Bool ->
  -- | 'allowDTMFInput'
  Prelude.Bool ->
  AllowedInputTypes
newAllowedInputTypes
  pAllowAudioInput_
  pAllowDTMFInput_ =
    AllowedInputTypes'
      { allowAudioInput =
          pAllowAudioInput_,
        allowDTMFInput = pAllowDTMFInput_
      }

-- | Indicates whether audio input is allowed.
allowedInputTypes_allowAudioInput :: Lens.Lens' AllowedInputTypes Prelude.Bool
allowedInputTypes_allowAudioInput = Lens.lens (\AllowedInputTypes' {allowAudioInput} -> allowAudioInput) (\s@AllowedInputTypes' {} a -> s {allowAudioInput = a} :: AllowedInputTypes)

-- | Indicates whether DTMF input is allowed.
allowedInputTypes_allowDTMFInput :: Lens.Lens' AllowedInputTypes Prelude.Bool
allowedInputTypes_allowDTMFInput = Lens.lens (\AllowedInputTypes' {allowDTMFInput} -> allowDTMFInput) (\s@AllowedInputTypes' {} a -> s {allowDTMFInput = a} :: AllowedInputTypes)

instance Data.FromJSON AllowedInputTypes where
  parseJSON =
    Data.withObject
      "AllowedInputTypes"
      ( \x ->
          AllowedInputTypes'
            Prelude.<$> (x Data..: "allowAudioInput")
            Prelude.<*> (x Data..: "allowDTMFInput")
      )

instance Prelude.Hashable AllowedInputTypes where
  hashWithSalt _salt AllowedInputTypes' {..} =
    _salt
      `Prelude.hashWithSalt` allowAudioInput
      `Prelude.hashWithSalt` allowDTMFInput

instance Prelude.NFData AllowedInputTypes where
  rnf AllowedInputTypes' {..} =
    Prelude.rnf allowAudioInput `Prelude.seq`
      Prelude.rnf allowDTMFInput

instance Data.ToJSON AllowedInputTypes where
  toJSON AllowedInputTypes' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("allowAudioInput" Data..= allowAudioInput),
            Prelude.Just
              ("allowDTMFInput" Data..= allowDTMFInput)
          ]
      )
