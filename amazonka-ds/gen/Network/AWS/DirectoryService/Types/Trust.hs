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
-- Module      : Network.AWS.DirectoryService.Types.Trust
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.Trust where

import qualified Network.AWS.Core as Core
import Network.AWS.DirectoryService.Types.SelectiveAuth
import Network.AWS.DirectoryService.Types.TrustDirection
import Network.AWS.DirectoryService.Types.TrustState
import Network.AWS.DirectoryService.Types.TrustType
import qualified Network.AWS.Lens as Lens

-- | Describes a trust relationship between an AWS Managed Microsoft AD
-- directory and an external domain.
--
-- /See:/ 'newTrust' smart constructor.
data Trust = Trust'
  { -- | The trust relationship type. @Forest@ is the default.
    trustType :: Core.Maybe TrustType,
    -- | The date and time that the trust relationship was created.
    createdDateTime :: Core.Maybe Core.POSIX,
    -- | The unique ID of the trust relationship.
    trustId :: Core.Maybe Core.Text,
    -- | The trust relationship direction.
    trustDirection :: Core.Maybe TrustDirection,
    -- | The date and time that the TrustState was last updated.
    stateLastUpdatedDateTime :: Core.Maybe Core.POSIX,
    -- | The date and time that the trust relationship was last updated.
    lastUpdatedDateTime :: Core.Maybe Core.POSIX,
    -- | The trust relationship state.
    trustState :: Core.Maybe TrustState,
    -- | Current state of selective authentication for the trust.
    selectiveAuth :: Core.Maybe SelectiveAuth,
    -- | The reason for the TrustState.
    trustStateReason :: Core.Maybe Core.Text,
    -- | The Directory ID of the AWS directory involved in the trust
    -- relationship.
    directoryId :: Core.Maybe Core.Text,
    -- | The Fully Qualified Domain Name (FQDN) of the external domain involved
    -- in the trust relationship.
    remoteDomainName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Trust' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trustType', 'trust_trustType' - The trust relationship type. @Forest@ is the default.
--
-- 'createdDateTime', 'trust_createdDateTime' - The date and time that the trust relationship was created.
--
-- 'trustId', 'trust_trustId' - The unique ID of the trust relationship.
--
-- 'trustDirection', 'trust_trustDirection' - The trust relationship direction.
--
-- 'stateLastUpdatedDateTime', 'trust_stateLastUpdatedDateTime' - The date and time that the TrustState was last updated.
--
-- 'lastUpdatedDateTime', 'trust_lastUpdatedDateTime' - The date and time that the trust relationship was last updated.
--
-- 'trustState', 'trust_trustState' - The trust relationship state.
--
-- 'selectiveAuth', 'trust_selectiveAuth' - Current state of selective authentication for the trust.
--
-- 'trustStateReason', 'trust_trustStateReason' - The reason for the TrustState.
--
-- 'directoryId', 'trust_directoryId' - The Directory ID of the AWS directory involved in the trust
-- relationship.
--
-- 'remoteDomainName', 'trust_remoteDomainName' - The Fully Qualified Domain Name (FQDN) of the external domain involved
-- in the trust relationship.
newTrust ::
  Trust
newTrust =
  Trust'
    { trustType = Core.Nothing,
      createdDateTime = Core.Nothing,
      trustId = Core.Nothing,
      trustDirection = Core.Nothing,
      stateLastUpdatedDateTime = Core.Nothing,
      lastUpdatedDateTime = Core.Nothing,
      trustState = Core.Nothing,
      selectiveAuth = Core.Nothing,
      trustStateReason = Core.Nothing,
      directoryId = Core.Nothing,
      remoteDomainName = Core.Nothing
    }

-- | The trust relationship type. @Forest@ is the default.
trust_trustType :: Lens.Lens' Trust (Core.Maybe TrustType)
trust_trustType = Lens.lens (\Trust' {trustType} -> trustType) (\s@Trust' {} a -> s {trustType = a} :: Trust)

-- | The date and time that the trust relationship was created.
trust_createdDateTime :: Lens.Lens' Trust (Core.Maybe Core.UTCTime)
trust_createdDateTime = Lens.lens (\Trust' {createdDateTime} -> createdDateTime) (\s@Trust' {} a -> s {createdDateTime = a} :: Trust) Core.. Lens.mapping Core._Time

-- | The unique ID of the trust relationship.
trust_trustId :: Lens.Lens' Trust (Core.Maybe Core.Text)
trust_trustId = Lens.lens (\Trust' {trustId} -> trustId) (\s@Trust' {} a -> s {trustId = a} :: Trust)

-- | The trust relationship direction.
trust_trustDirection :: Lens.Lens' Trust (Core.Maybe TrustDirection)
trust_trustDirection = Lens.lens (\Trust' {trustDirection} -> trustDirection) (\s@Trust' {} a -> s {trustDirection = a} :: Trust)

-- | The date and time that the TrustState was last updated.
trust_stateLastUpdatedDateTime :: Lens.Lens' Trust (Core.Maybe Core.UTCTime)
trust_stateLastUpdatedDateTime = Lens.lens (\Trust' {stateLastUpdatedDateTime} -> stateLastUpdatedDateTime) (\s@Trust' {} a -> s {stateLastUpdatedDateTime = a} :: Trust) Core.. Lens.mapping Core._Time

-- | The date and time that the trust relationship was last updated.
trust_lastUpdatedDateTime :: Lens.Lens' Trust (Core.Maybe Core.UTCTime)
trust_lastUpdatedDateTime = Lens.lens (\Trust' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@Trust' {} a -> s {lastUpdatedDateTime = a} :: Trust) Core.. Lens.mapping Core._Time

-- | The trust relationship state.
trust_trustState :: Lens.Lens' Trust (Core.Maybe TrustState)
trust_trustState = Lens.lens (\Trust' {trustState} -> trustState) (\s@Trust' {} a -> s {trustState = a} :: Trust)

-- | Current state of selective authentication for the trust.
trust_selectiveAuth :: Lens.Lens' Trust (Core.Maybe SelectiveAuth)
trust_selectiveAuth = Lens.lens (\Trust' {selectiveAuth} -> selectiveAuth) (\s@Trust' {} a -> s {selectiveAuth = a} :: Trust)

-- | The reason for the TrustState.
trust_trustStateReason :: Lens.Lens' Trust (Core.Maybe Core.Text)
trust_trustStateReason = Lens.lens (\Trust' {trustStateReason} -> trustStateReason) (\s@Trust' {} a -> s {trustStateReason = a} :: Trust)

-- | The Directory ID of the AWS directory involved in the trust
-- relationship.
trust_directoryId :: Lens.Lens' Trust (Core.Maybe Core.Text)
trust_directoryId = Lens.lens (\Trust' {directoryId} -> directoryId) (\s@Trust' {} a -> s {directoryId = a} :: Trust)

-- | The Fully Qualified Domain Name (FQDN) of the external domain involved
-- in the trust relationship.
trust_remoteDomainName :: Lens.Lens' Trust (Core.Maybe Core.Text)
trust_remoteDomainName = Lens.lens (\Trust' {remoteDomainName} -> remoteDomainName) (\s@Trust' {} a -> s {remoteDomainName = a} :: Trust)

instance Core.FromJSON Trust where
  parseJSON =
    Core.withObject
      "Trust"
      ( \x ->
          Trust'
            Core.<$> (x Core..:? "TrustType")
            Core.<*> (x Core..:? "CreatedDateTime")
            Core.<*> (x Core..:? "TrustId")
            Core.<*> (x Core..:? "TrustDirection")
            Core.<*> (x Core..:? "StateLastUpdatedDateTime")
            Core.<*> (x Core..:? "LastUpdatedDateTime")
            Core.<*> (x Core..:? "TrustState")
            Core.<*> (x Core..:? "SelectiveAuth")
            Core.<*> (x Core..:? "TrustStateReason")
            Core.<*> (x Core..:? "DirectoryId")
            Core.<*> (x Core..:? "RemoteDomainName")
      )

instance Core.Hashable Trust

instance Core.NFData Trust
