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
-- Module      : Amazonka.ChimeSdkVoice.Types.CandidateAddress
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkVoice.Types.CandidateAddress where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newCandidateAddress' smart constructor.
data CandidateAddress = CandidateAddress'
  { city :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    country :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    postalCode :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    postalCodePlus4 :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    state :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    streetInfo :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    streetNumber :: Prelude.Maybe (Data.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CandidateAddress' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'city', 'candidateAddress_city' - Undocumented member.
--
-- 'country', 'candidateAddress_country' - Undocumented member.
--
-- 'postalCode', 'candidateAddress_postalCode' - Undocumented member.
--
-- 'postalCodePlus4', 'candidateAddress_postalCodePlus4' - Undocumented member.
--
-- 'state', 'candidateAddress_state' - Undocumented member.
--
-- 'streetInfo', 'candidateAddress_streetInfo' - Undocumented member.
--
-- 'streetNumber', 'candidateAddress_streetNumber' - Undocumented member.
newCandidateAddress ::
  CandidateAddress
newCandidateAddress =
  CandidateAddress'
    { city = Prelude.Nothing,
      country = Prelude.Nothing,
      postalCode = Prelude.Nothing,
      postalCodePlus4 = Prelude.Nothing,
      state = Prelude.Nothing,
      streetInfo = Prelude.Nothing,
      streetNumber = Prelude.Nothing
    }

-- | Undocumented member.
candidateAddress_city :: Lens.Lens' CandidateAddress (Prelude.Maybe Prelude.Text)
candidateAddress_city = Lens.lens (\CandidateAddress' {city} -> city) (\s@CandidateAddress' {} a -> s {city = a} :: CandidateAddress) Prelude.. Lens.mapping Data._Sensitive

-- | Undocumented member.
candidateAddress_country :: Lens.Lens' CandidateAddress (Prelude.Maybe Prelude.Text)
candidateAddress_country = Lens.lens (\CandidateAddress' {country} -> country) (\s@CandidateAddress' {} a -> s {country = a} :: CandidateAddress) Prelude.. Lens.mapping Data._Sensitive

-- | Undocumented member.
candidateAddress_postalCode :: Lens.Lens' CandidateAddress (Prelude.Maybe Prelude.Text)
candidateAddress_postalCode = Lens.lens (\CandidateAddress' {postalCode} -> postalCode) (\s@CandidateAddress' {} a -> s {postalCode = a} :: CandidateAddress) Prelude.. Lens.mapping Data._Sensitive

-- | Undocumented member.
candidateAddress_postalCodePlus4 :: Lens.Lens' CandidateAddress (Prelude.Maybe Prelude.Text)
candidateAddress_postalCodePlus4 = Lens.lens (\CandidateAddress' {postalCodePlus4} -> postalCodePlus4) (\s@CandidateAddress' {} a -> s {postalCodePlus4 = a} :: CandidateAddress) Prelude.. Lens.mapping Data._Sensitive

-- | Undocumented member.
candidateAddress_state :: Lens.Lens' CandidateAddress (Prelude.Maybe Prelude.Text)
candidateAddress_state = Lens.lens (\CandidateAddress' {state} -> state) (\s@CandidateAddress' {} a -> s {state = a} :: CandidateAddress) Prelude.. Lens.mapping Data._Sensitive

-- | Undocumented member.
candidateAddress_streetInfo :: Lens.Lens' CandidateAddress (Prelude.Maybe Prelude.Text)
candidateAddress_streetInfo = Lens.lens (\CandidateAddress' {streetInfo} -> streetInfo) (\s@CandidateAddress' {} a -> s {streetInfo = a} :: CandidateAddress) Prelude.. Lens.mapping Data._Sensitive

-- | Undocumented member.
candidateAddress_streetNumber :: Lens.Lens' CandidateAddress (Prelude.Maybe Prelude.Text)
candidateAddress_streetNumber = Lens.lens (\CandidateAddress' {streetNumber} -> streetNumber) (\s@CandidateAddress' {} a -> s {streetNumber = a} :: CandidateAddress) Prelude.. Lens.mapping Data._Sensitive

instance Data.FromJSON CandidateAddress where
  parseJSON =
    Data.withObject
      "CandidateAddress"
      ( \x ->
          CandidateAddress'
            Prelude.<$> (x Data..:? "city")
            Prelude.<*> (x Data..:? "country")
            Prelude.<*> (x Data..:? "postalCode")
            Prelude.<*> (x Data..:? "postalCodePlus4")
            Prelude.<*> (x Data..:? "state")
            Prelude.<*> (x Data..:? "streetInfo")
            Prelude.<*> (x Data..:? "streetNumber")
      )

instance Prelude.Hashable CandidateAddress where
  hashWithSalt _salt CandidateAddress' {..} =
    _salt
      `Prelude.hashWithSalt` city
      `Prelude.hashWithSalt` country
      `Prelude.hashWithSalt` postalCode
      `Prelude.hashWithSalt` postalCodePlus4
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` streetInfo
      `Prelude.hashWithSalt` streetNumber

instance Prelude.NFData CandidateAddress where
  rnf CandidateAddress' {..} =
    Prelude.rnf city `Prelude.seq`
      Prelude.rnf country `Prelude.seq`
        Prelude.rnf postalCode `Prelude.seq`
          Prelude.rnf postalCodePlus4 `Prelude.seq`
            Prelude.rnf state `Prelude.seq`
              Prelude.rnf streetInfo `Prelude.seq`
                Prelude.rnf streetNumber
