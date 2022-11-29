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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Nimble.Types.EulaAcceptance where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The acceptance of a EULA, required to use Amazon-provided streaming
-- images.
--
-- /See:/ 'newEulaAcceptance' smart constructor.
data EulaAcceptance = EulaAcceptance'
  { -- | The EULA acceptance ID.
    eulaAcceptanceId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the acceptee.
    accepteeId :: Prelude.Maybe Prelude.Text,
    -- | The EULA ID.
    eulaId :: Prelude.Maybe Prelude.Text,
    -- | The Unix epoch timestamp in seconds for when the EULA was accepted.
    acceptedAt :: Prelude.Maybe Core.POSIX,
    -- | The ID of the person who accepted the EULA.
    acceptedBy :: Prelude.Maybe Prelude.Text
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
-- 'eulaAcceptanceId', 'eulaAcceptance_eulaAcceptanceId' - The EULA acceptance ID.
--
-- 'accepteeId', 'eulaAcceptance_accepteeId' - The ID of the acceptee.
--
-- 'eulaId', 'eulaAcceptance_eulaId' - The EULA ID.
--
-- 'acceptedAt', 'eulaAcceptance_acceptedAt' - The Unix epoch timestamp in seconds for when the EULA was accepted.
--
-- 'acceptedBy', 'eulaAcceptance_acceptedBy' - The ID of the person who accepted the EULA.
newEulaAcceptance ::
  EulaAcceptance
newEulaAcceptance =
  EulaAcceptance'
    { eulaAcceptanceId = Prelude.Nothing,
      accepteeId = Prelude.Nothing,
      eulaId = Prelude.Nothing,
      acceptedAt = Prelude.Nothing,
      acceptedBy = Prelude.Nothing
    }

-- | The EULA acceptance ID.
eulaAcceptance_eulaAcceptanceId :: Lens.Lens' EulaAcceptance (Prelude.Maybe Prelude.Text)
eulaAcceptance_eulaAcceptanceId = Lens.lens (\EulaAcceptance' {eulaAcceptanceId} -> eulaAcceptanceId) (\s@EulaAcceptance' {} a -> s {eulaAcceptanceId = a} :: EulaAcceptance)

-- | The ID of the acceptee.
eulaAcceptance_accepteeId :: Lens.Lens' EulaAcceptance (Prelude.Maybe Prelude.Text)
eulaAcceptance_accepteeId = Lens.lens (\EulaAcceptance' {accepteeId} -> accepteeId) (\s@EulaAcceptance' {} a -> s {accepteeId = a} :: EulaAcceptance)

-- | The EULA ID.
eulaAcceptance_eulaId :: Lens.Lens' EulaAcceptance (Prelude.Maybe Prelude.Text)
eulaAcceptance_eulaId = Lens.lens (\EulaAcceptance' {eulaId} -> eulaId) (\s@EulaAcceptance' {} a -> s {eulaId = a} :: EulaAcceptance)

-- | The Unix epoch timestamp in seconds for when the EULA was accepted.
eulaAcceptance_acceptedAt :: Lens.Lens' EulaAcceptance (Prelude.Maybe Prelude.UTCTime)
eulaAcceptance_acceptedAt = Lens.lens (\EulaAcceptance' {acceptedAt} -> acceptedAt) (\s@EulaAcceptance' {} a -> s {acceptedAt = a} :: EulaAcceptance) Prelude.. Lens.mapping Core._Time

-- | The ID of the person who accepted the EULA.
eulaAcceptance_acceptedBy :: Lens.Lens' EulaAcceptance (Prelude.Maybe Prelude.Text)
eulaAcceptance_acceptedBy = Lens.lens (\EulaAcceptance' {acceptedBy} -> acceptedBy) (\s@EulaAcceptance' {} a -> s {acceptedBy = a} :: EulaAcceptance)

instance Core.FromJSON EulaAcceptance where
  parseJSON =
    Core.withObject
      "EulaAcceptance"
      ( \x ->
          EulaAcceptance'
            Prelude.<$> (x Core..:? "eulaAcceptanceId")
            Prelude.<*> (x Core..:? "accepteeId")
            Prelude.<*> (x Core..:? "eulaId")
            Prelude.<*> (x Core..:? "acceptedAt")
            Prelude.<*> (x Core..:? "acceptedBy")
      )

instance Prelude.Hashable EulaAcceptance where
  hashWithSalt _salt EulaAcceptance' {..} =
    _salt `Prelude.hashWithSalt` eulaAcceptanceId
      `Prelude.hashWithSalt` accepteeId
      `Prelude.hashWithSalt` eulaId
      `Prelude.hashWithSalt` acceptedAt
      `Prelude.hashWithSalt` acceptedBy

instance Prelude.NFData EulaAcceptance where
  rnf EulaAcceptance' {..} =
    Prelude.rnf eulaAcceptanceId
      `Prelude.seq` Prelude.rnf accepteeId
      `Prelude.seq` Prelude.rnf eulaId
      `Prelude.seq` Prelude.rnf acceptedAt
      `Prelude.seq` Prelude.rnf acceptedBy
