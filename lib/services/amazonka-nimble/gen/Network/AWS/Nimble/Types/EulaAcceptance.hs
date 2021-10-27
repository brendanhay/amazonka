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
-- Module      : Network.AWS.Nimble.Types.EulaAcceptance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Nimble.Types.EulaAcceptance where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- |
--
-- /See:/ 'newEulaAcceptance' smart constructor.
data EulaAcceptance = EulaAcceptance'
  { -- | The ID of the acceptee.
    accepteeId :: Prelude.Maybe Prelude.Text,
    -- | The EULA ID.
    eulaId :: Prelude.Maybe Prelude.Text,
    -- | The Unix epoch timestamp in seconds for when the EULA was accepted.
    acceptedAt :: Prelude.Maybe Core.POSIX,
    -- | The ID of the person who accepted the EULA.
    acceptedBy :: Prelude.Maybe Prelude.Text,
    -- | The EULA acceptance ID.
    eulaAcceptanceId :: Prelude.Maybe Prelude.Text
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
-- 'accepteeId', 'eulaAcceptance_accepteeId' - The ID of the acceptee.
--
-- 'eulaId', 'eulaAcceptance_eulaId' - The EULA ID.
--
-- 'acceptedAt', 'eulaAcceptance_acceptedAt' - The Unix epoch timestamp in seconds for when the EULA was accepted.
--
-- 'acceptedBy', 'eulaAcceptance_acceptedBy' - The ID of the person who accepted the EULA.
--
-- 'eulaAcceptanceId', 'eulaAcceptance_eulaAcceptanceId' - The EULA acceptance ID.
newEulaAcceptance ::
  EulaAcceptance
newEulaAcceptance =
  EulaAcceptance'
    { accepteeId = Prelude.Nothing,
      eulaId = Prelude.Nothing,
      acceptedAt = Prelude.Nothing,
      acceptedBy = Prelude.Nothing,
      eulaAcceptanceId = Prelude.Nothing
    }

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

-- | The EULA acceptance ID.
eulaAcceptance_eulaAcceptanceId :: Lens.Lens' EulaAcceptance (Prelude.Maybe Prelude.Text)
eulaAcceptance_eulaAcceptanceId = Lens.lens (\EulaAcceptance' {eulaAcceptanceId} -> eulaAcceptanceId) (\s@EulaAcceptance' {} a -> s {eulaAcceptanceId = a} :: EulaAcceptance)

instance Core.FromJSON EulaAcceptance where
  parseJSON =
    Core.withObject
      "EulaAcceptance"
      ( \x ->
          EulaAcceptance'
            Prelude.<$> (x Core..:? "accepteeId")
            Prelude.<*> (x Core..:? "eulaId")
            Prelude.<*> (x Core..:? "acceptedAt")
            Prelude.<*> (x Core..:? "acceptedBy")
            Prelude.<*> (x Core..:? "eulaAcceptanceId")
      )

instance Prelude.Hashable EulaAcceptance

instance Prelude.NFData EulaAcceptance
