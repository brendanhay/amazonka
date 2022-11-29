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
-- Module      : Amazonka.DirectoryService.Types.Trust
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DirectoryService.Types.Trust where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DirectoryService.Types.SelectiveAuth
import Amazonka.DirectoryService.Types.TrustDirection
import Amazonka.DirectoryService.Types.TrustState
import Amazonka.DirectoryService.Types.TrustType
import qualified Amazonka.Prelude as Prelude

-- | Describes a trust relationship between an Managed Microsoft AD directory
-- and an external domain.
--
-- /See:/ 'newTrust' smart constructor.
data Trust = Trust'
  { -- | The Directory ID of the Amazon Web Services directory involved in the
    -- trust relationship.
    directoryId :: Prelude.Maybe Prelude.Text,
    -- | The Fully Qualified Domain Name (FQDN) of the external domain involved
    -- in the trust relationship.
    remoteDomainName :: Prelude.Maybe Prelude.Text,
    -- | The trust relationship type. @Forest@ is the default.
    trustType :: Prelude.Maybe TrustType,
    -- | The unique ID of the trust relationship.
    trustId :: Prelude.Maybe Prelude.Text,
    -- | The trust relationship state.
    trustState :: Prelude.Maybe TrustState,
    -- | The date and time that the trust relationship was created.
    createdDateTime :: Prelude.Maybe Core.POSIX,
    -- | Current state of selective authentication for the trust.
    selectiveAuth :: Prelude.Maybe SelectiveAuth,
    -- | The reason for the TrustState.
    trustStateReason :: Prelude.Maybe Prelude.Text,
    -- | The trust relationship direction.
    trustDirection :: Prelude.Maybe TrustDirection,
    -- | The date and time that the TrustState was last updated.
    stateLastUpdatedDateTime :: Prelude.Maybe Core.POSIX,
    -- | The date and time that the trust relationship was last updated.
    lastUpdatedDateTime :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Trust' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directoryId', 'trust_directoryId' - The Directory ID of the Amazon Web Services directory involved in the
-- trust relationship.
--
-- 'remoteDomainName', 'trust_remoteDomainName' - The Fully Qualified Domain Name (FQDN) of the external domain involved
-- in the trust relationship.
--
-- 'trustType', 'trust_trustType' - The trust relationship type. @Forest@ is the default.
--
-- 'trustId', 'trust_trustId' - The unique ID of the trust relationship.
--
-- 'trustState', 'trust_trustState' - The trust relationship state.
--
-- 'createdDateTime', 'trust_createdDateTime' - The date and time that the trust relationship was created.
--
-- 'selectiveAuth', 'trust_selectiveAuth' - Current state of selective authentication for the trust.
--
-- 'trustStateReason', 'trust_trustStateReason' - The reason for the TrustState.
--
-- 'trustDirection', 'trust_trustDirection' - The trust relationship direction.
--
-- 'stateLastUpdatedDateTime', 'trust_stateLastUpdatedDateTime' - The date and time that the TrustState was last updated.
--
-- 'lastUpdatedDateTime', 'trust_lastUpdatedDateTime' - The date and time that the trust relationship was last updated.
newTrust ::
  Trust
newTrust =
  Trust'
    { directoryId = Prelude.Nothing,
      remoteDomainName = Prelude.Nothing,
      trustType = Prelude.Nothing,
      trustId = Prelude.Nothing,
      trustState = Prelude.Nothing,
      createdDateTime = Prelude.Nothing,
      selectiveAuth = Prelude.Nothing,
      trustStateReason = Prelude.Nothing,
      trustDirection = Prelude.Nothing,
      stateLastUpdatedDateTime = Prelude.Nothing,
      lastUpdatedDateTime = Prelude.Nothing
    }

-- | The Directory ID of the Amazon Web Services directory involved in the
-- trust relationship.
trust_directoryId :: Lens.Lens' Trust (Prelude.Maybe Prelude.Text)
trust_directoryId = Lens.lens (\Trust' {directoryId} -> directoryId) (\s@Trust' {} a -> s {directoryId = a} :: Trust)

-- | The Fully Qualified Domain Name (FQDN) of the external domain involved
-- in the trust relationship.
trust_remoteDomainName :: Lens.Lens' Trust (Prelude.Maybe Prelude.Text)
trust_remoteDomainName = Lens.lens (\Trust' {remoteDomainName} -> remoteDomainName) (\s@Trust' {} a -> s {remoteDomainName = a} :: Trust)

-- | The trust relationship type. @Forest@ is the default.
trust_trustType :: Lens.Lens' Trust (Prelude.Maybe TrustType)
trust_trustType = Lens.lens (\Trust' {trustType} -> trustType) (\s@Trust' {} a -> s {trustType = a} :: Trust)

-- | The unique ID of the trust relationship.
trust_trustId :: Lens.Lens' Trust (Prelude.Maybe Prelude.Text)
trust_trustId = Lens.lens (\Trust' {trustId} -> trustId) (\s@Trust' {} a -> s {trustId = a} :: Trust)

-- | The trust relationship state.
trust_trustState :: Lens.Lens' Trust (Prelude.Maybe TrustState)
trust_trustState = Lens.lens (\Trust' {trustState} -> trustState) (\s@Trust' {} a -> s {trustState = a} :: Trust)

-- | The date and time that the trust relationship was created.
trust_createdDateTime :: Lens.Lens' Trust (Prelude.Maybe Prelude.UTCTime)
trust_createdDateTime = Lens.lens (\Trust' {createdDateTime} -> createdDateTime) (\s@Trust' {} a -> s {createdDateTime = a} :: Trust) Prelude.. Lens.mapping Core._Time

-- | Current state of selective authentication for the trust.
trust_selectiveAuth :: Lens.Lens' Trust (Prelude.Maybe SelectiveAuth)
trust_selectiveAuth = Lens.lens (\Trust' {selectiveAuth} -> selectiveAuth) (\s@Trust' {} a -> s {selectiveAuth = a} :: Trust)

-- | The reason for the TrustState.
trust_trustStateReason :: Lens.Lens' Trust (Prelude.Maybe Prelude.Text)
trust_trustStateReason = Lens.lens (\Trust' {trustStateReason} -> trustStateReason) (\s@Trust' {} a -> s {trustStateReason = a} :: Trust)

-- | The trust relationship direction.
trust_trustDirection :: Lens.Lens' Trust (Prelude.Maybe TrustDirection)
trust_trustDirection = Lens.lens (\Trust' {trustDirection} -> trustDirection) (\s@Trust' {} a -> s {trustDirection = a} :: Trust)

-- | The date and time that the TrustState was last updated.
trust_stateLastUpdatedDateTime :: Lens.Lens' Trust (Prelude.Maybe Prelude.UTCTime)
trust_stateLastUpdatedDateTime = Lens.lens (\Trust' {stateLastUpdatedDateTime} -> stateLastUpdatedDateTime) (\s@Trust' {} a -> s {stateLastUpdatedDateTime = a} :: Trust) Prelude.. Lens.mapping Core._Time

-- | The date and time that the trust relationship was last updated.
trust_lastUpdatedDateTime :: Lens.Lens' Trust (Prelude.Maybe Prelude.UTCTime)
trust_lastUpdatedDateTime = Lens.lens (\Trust' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@Trust' {} a -> s {lastUpdatedDateTime = a} :: Trust) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON Trust where
  parseJSON =
    Core.withObject
      "Trust"
      ( \x ->
          Trust'
            Prelude.<$> (x Core..:? "DirectoryId")
            Prelude.<*> (x Core..:? "RemoteDomainName")
            Prelude.<*> (x Core..:? "TrustType")
            Prelude.<*> (x Core..:? "TrustId")
            Prelude.<*> (x Core..:? "TrustState")
            Prelude.<*> (x Core..:? "CreatedDateTime")
            Prelude.<*> (x Core..:? "SelectiveAuth")
            Prelude.<*> (x Core..:? "TrustStateReason")
            Prelude.<*> (x Core..:? "TrustDirection")
            Prelude.<*> (x Core..:? "StateLastUpdatedDateTime")
            Prelude.<*> (x Core..:? "LastUpdatedDateTime")
      )

instance Prelude.Hashable Trust where
  hashWithSalt _salt Trust' {..} =
    _salt `Prelude.hashWithSalt` directoryId
      `Prelude.hashWithSalt` remoteDomainName
      `Prelude.hashWithSalt` trustType
      `Prelude.hashWithSalt` trustId
      `Prelude.hashWithSalt` trustState
      `Prelude.hashWithSalt` createdDateTime
      `Prelude.hashWithSalt` selectiveAuth
      `Prelude.hashWithSalt` trustStateReason
      `Prelude.hashWithSalt` trustDirection
      `Prelude.hashWithSalt` stateLastUpdatedDateTime
      `Prelude.hashWithSalt` lastUpdatedDateTime

instance Prelude.NFData Trust where
  rnf Trust' {..} =
    Prelude.rnf directoryId
      `Prelude.seq` Prelude.rnf remoteDomainName
      `Prelude.seq` Prelude.rnf trustType
      `Prelude.seq` Prelude.rnf trustId
      `Prelude.seq` Prelude.rnf trustState
      `Prelude.seq` Prelude.rnf createdDateTime
      `Prelude.seq` Prelude.rnf selectiveAuth
      `Prelude.seq` Prelude.rnf trustStateReason
      `Prelude.seq` Prelude.rnf trustDirection
      `Prelude.seq` Prelude.rnf stateLastUpdatedDateTime
      `Prelude.seq` Prelude.rnf lastUpdatedDateTime
