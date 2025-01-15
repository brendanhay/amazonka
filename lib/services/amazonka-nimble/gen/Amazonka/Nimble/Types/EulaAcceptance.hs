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
-- Module      : Amazonka.Nimble.Types.EulaAcceptance
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Nimble.Types.EulaAcceptance where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The acceptance of a EULA, required to use Amazon-provided streaming
-- images.
--
-- /See:/ 'newEulaAcceptance' smart constructor.
data EulaAcceptance = EulaAcceptance'
  { -- | The ISO timestamp in seconds for when the EULA was accepted.
    acceptedAt :: Prelude.Maybe Data.ISO8601,
    -- | The ID of the person who accepted the EULA.
    acceptedBy :: Prelude.Maybe Prelude.Text,
    -- | The ID of the acceptee.
    accepteeId :: Prelude.Maybe Prelude.Text,
    -- | The EULA acceptance ID.
    eulaAcceptanceId :: Prelude.Maybe Prelude.Text,
    -- | The EULA ID.
    eulaId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EulaAcceptance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'acceptedAt', 'eulaAcceptance_acceptedAt' - The ISO timestamp in seconds for when the EULA was accepted.
--
-- 'acceptedBy', 'eulaAcceptance_acceptedBy' - The ID of the person who accepted the EULA.
--
-- 'accepteeId', 'eulaAcceptance_accepteeId' - The ID of the acceptee.
--
-- 'eulaAcceptanceId', 'eulaAcceptance_eulaAcceptanceId' - The EULA acceptance ID.
--
-- 'eulaId', 'eulaAcceptance_eulaId' - The EULA ID.
newEulaAcceptance ::
  EulaAcceptance
newEulaAcceptance =
  EulaAcceptance'
    { acceptedAt = Prelude.Nothing,
      acceptedBy = Prelude.Nothing,
      accepteeId = Prelude.Nothing,
      eulaAcceptanceId = Prelude.Nothing,
      eulaId = Prelude.Nothing
    }

-- | The ISO timestamp in seconds for when the EULA was accepted.
eulaAcceptance_acceptedAt :: Lens.Lens' EulaAcceptance (Prelude.Maybe Prelude.UTCTime)
eulaAcceptance_acceptedAt = Lens.lens (\EulaAcceptance' {acceptedAt} -> acceptedAt) (\s@EulaAcceptance' {} a -> s {acceptedAt = a} :: EulaAcceptance) Prelude.. Lens.mapping Data._Time

-- | The ID of the person who accepted the EULA.
eulaAcceptance_acceptedBy :: Lens.Lens' EulaAcceptance (Prelude.Maybe Prelude.Text)
eulaAcceptance_acceptedBy = Lens.lens (\EulaAcceptance' {acceptedBy} -> acceptedBy) (\s@EulaAcceptance' {} a -> s {acceptedBy = a} :: EulaAcceptance)

-- | The ID of the acceptee.
eulaAcceptance_accepteeId :: Lens.Lens' EulaAcceptance (Prelude.Maybe Prelude.Text)
eulaAcceptance_accepteeId = Lens.lens (\EulaAcceptance' {accepteeId} -> accepteeId) (\s@EulaAcceptance' {} a -> s {accepteeId = a} :: EulaAcceptance)

-- | The EULA acceptance ID.
eulaAcceptance_eulaAcceptanceId :: Lens.Lens' EulaAcceptance (Prelude.Maybe Prelude.Text)
eulaAcceptance_eulaAcceptanceId = Lens.lens (\EulaAcceptance' {eulaAcceptanceId} -> eulaAcceptanceId) (\s@EulaAcceptance' {} a -> s {eulaAcceptanceId = a} :: EulaAcceptance)

-- | The EULA ID.
eulaAcceptance_eulaId :: Lens.Lens' EulaAcceptance (Prelude.Maybe Prelude.Text)
eulaAcceptance_eulaId = Lens.lens (\EulaAcceptance' {eulaId} -> eulaId) (\s@EulaAcceptance' {} a -> s {eulaId = a} :: EulaAcceptance)

instance Data.FromJSON EulaAcceptance where
  parseJSON =
    Data.withObject
      "EulaAcceptance"
      ( \x ->
          EulaAcceptance'
            Prelude.<$> (x Data..:? "acceptedAt")
            Prelude.<*> (x Data..:? "acceptedBy")
            Prelude.<*> (x Data..:? "accepteeId")
            Prelude.<*> (x Data..:? "eulaAcceptanceId")
            Prelude.<*> (x Data..:? "eulaId")
      )

instance Prelude.Hashable EulaAcceptance where
  hashWithSalt _salt EulaAcceptance' {..} =
    _salt
      `Prelude.hashWithSalt` acceptedAt
      `Prelude.hashWithSalt` acceptedBy
      `Prelude.hashWithSalt` accepteeId
      `Prelude.hashWithSalt` eulaAcceptanceId
      `Prelude.hashWithSalt` eulaId

instance Prelude.NFData EulaAcceptance where
  rnf EulaAcceptance' {..} =
    Prelude.rnf acceptedAt `Prelude.seq`
      Prelude.rnf acceptedBy `Prelude.seq`
        Prelude.rnf accepteeId `Prelude.seq`
          Prelude.rnf eulaAcceptanceId `Prelude.seq`
            Prelude.rnf eulaId
