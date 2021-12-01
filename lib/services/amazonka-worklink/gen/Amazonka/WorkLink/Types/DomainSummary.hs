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
-- Module      : Amazonka.WorkLink.Types.DomainSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkLink.Types.DomainSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.WorkLink.Types.DomainStatus

-- | The summary of the domain.
--
-- /See:/ 'newDomainSummary' smart constructor.
data DomainSummary = DomainSummary'
  { -- | The name to display.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | The name of the domain.
    domainName :: Prelude.Text,
    -- | The time that the domain was created.
    createdTime :: Core.POSIX,
    -- | The status of the domain.
    domainStatus :: DomainStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DomainSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'displayName', 'domainSummary_displayName' - The name to display.
--
-- 'domainName', 'domainSummary_domainName' - The name of the domain.
--
-- 'createdTime', 'domainSummary_createdTime' - The time that the domain was created.
--
-- 'domainStatus', 'domainSummary_domainStatus' - The status of the domain.
newDomainSummary ::
  -- | 'domainName'
  Prelude.Text ->
  -- | 'createdTime'
  Prelude.UTCTime ->
  -- | 'domainStatus'
  DomainStatus ->
  DomainSummary
newDomainSummary
  pDomainName_
  pCreatedTime_
  pDomainStatus_ =
    DomainSummary'
      { displayName = Prelude.Nothing,
        domainName = pDomainName_,
        createdTime = Core._Time Lens.# pCreatedTime_,
        domainStatus = pDomainStatus_
      }

-- | The name to display.
domainSummary_displayName :: Lens.Lens' DomainSummary (Prelude.Maybe Prelude.Text)
domainSummary_displayName = Lens.lens (\DomainSummary' {displayName} -> displayName) (\s@DomainSummary' {} a -> s {displayName = a} :: DomainSummary)

-- | The name of the domain.
domainSummary_domainName :: Lens.Lens' DomainSummary Prelude.Text
domainSummary_domainName = Lens.lens (\DomainSummary' {domainName} -> domainName) (\s@DomainSummary' {} a -> s {domainName = a} :: DomainSummary)

-- | The time that the domain was created.
domainSummary_createdTime :: Lens.Lens' DomainSummary Prelude.UTCTime
domainSummary_createdTime = Lens.lens (\DomainSummary' {createdTime} -> createdTime) (\s@DomainSummary' {} a -> s {createdTime = a} :: DomainSummary) Prelude.. Core._Time

-- | The status of the domain.
domainSummary_domainStatus :: Lens.Lens' DomainSummary DomainStatus
domainSummary_domainStatus = Lens.lens (\DomainSummary' {domainStatus} -> domainStatus) (\s@DomainSummary' {} a -> s {domainStatus = a} :: DomainSummary)

instance Core.FromJSON DomainSummary where
  parseJSON =
    Core.withObject
      "DomainSummary"
      ( \x ->
          DomainSummary'
            Prelude.<$> (x Core..:? "DisplayName")
            Prelude.<*> (x Core..: "DomainName")
            Prelude.<*> (x Core..: "CreatedTime")
            Prelude.<*> (x Core..: "DomainStatus")
      )

instance Prelude.Hashable DomainSummary where
  hashWithSalt salt' DomainSummary' {..} =
    salt' `Prelude.hashWithSalt` domainStatus
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` domainName
      `Prelude.hashWithSalt` displayName

instance Prelude.NFData DomainSummary where
  rnf DomainSummary' {..} =
    Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf domainStatus
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf domainName
