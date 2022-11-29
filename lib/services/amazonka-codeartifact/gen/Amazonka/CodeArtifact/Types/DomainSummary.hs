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
-- Module      : Amazonka.CodeArtifact.Types.DomainSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeArtifact.Types.DomainSummary where

import Amazonka.CodeArtifact.Types.DomainStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about a domain, including its name, Amazon Resource Name
-- (ARN), and status. The
-- <https://docs.aws.amazon.com/codeartifact/latest/APIReference/API_ListDomains.html ListDomains>
-- operation returns a list of @DomainSummary@ objects.
--
-- /See:/ 'newDomainSummary' smart constructor.
data DomainSummary = DomainSummary'
  { -- | The name of the domain.
    name :: Prelude.Maybe Prelude.Text,
    -- | A timestamp that contains the date and time the domain was created.
    createdTime :: Prelude.Maybe Core.POSIX,
    -- | The ARN of the domain.
    arn :: Prelude.Maybe Prelude.Text,
    -- | A string that contains the status of the domain.
    status :: Prelude.Maybe DomainStatus,
    -- | The 12-digit account number of the Amazon Web Services account that owns
    -- the domain. It does not include dashes or spaces.
    owner :: Prelude.Maybe Prelude.Text,
    -- | The key used to encrypt the domain.
    encryptionKey :: Prelude.Maybe Prelude.Text
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
-- 'name', 'domainSummary_name' - The name of the domain.
--
-- 'createdTime', 'domainSummary_createdTime' - A timestamp that contains the date and time the domain was created.
--
-- 'arn', 'domainSummary_arn' - The ARN of the domain.
--
-- 'status', 'domainSummary_status' - A string that contains the status of the domain.
--
-- 'owner', 'domainSummary_owner' - The 12-digit account number of the Amazon Web Services account that owns
-- the domain. It does not include dashes or spaces.
--
-- 'encryptionKey', 'domainSummary_encryptionKey' - The key used to encrypt the domain.
newDomainSummary ::
  DomainSummary
newDomainSummary =
  DomainSummary'
    { name = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      arn = Prelude.Nothing,
      status = Prelude.Nothing,
      owner = Prelude.Nothing,
      encryptionKey = Prelude.Nothing
    }

-- | The name of the domain.
domainSummary_name :: Lens.Lens' DomainSummary (Prelude.Maybe Prelude.Text)
domainSummary_name = Lens.lens (\DomainSummary' {name} -> name) (\s@DomainSummary' {} a -> s {name = a} :: DomainSummary)

-- | A timestamp that contains the date and time the domain was created.
domainSummary_createdTime :: Lens.Lens' DomainSummary (Prelude.Maybe Prelude.UTCTime)
domainSummary_createdTime = Lens.lens (\DomainSummary' {createdTime} -> createdTime) (\s@DomainSummary' {} a -> s {createdTime = a} :: DomainSummary) Prelude.. Lens.mapping Core._Time

-- | The ARN of the domain.
domainSummary_arn :: Lens.Lens' DomainSummary (Prelude.Maybe Prelude.Text)
domainSummary_arn = Lens.lens (\DomainSummary' {arn} -> arn) (\s@DomainSummary' {} a -> s {arn = a} :: DomainSummary)

-- | A string that contains the status of the domain.
domainSummary_status :: Lens.Lens' DomainSummary (Prelude.Maybe DomainStatus)
domainSummary_status = Lens.lens (\DomainSummary' {status} -> status) (\s@DomainSummary' {} a -> s {status = a} :: DomainSummary)

-- | The 12-digit account number of the Amazon Web Services account that owns
-- the domain. It does not include dashes or spaces.
domainSummary_owner :: Lens.Lens' DomainSummary (Prelude.Maybe Prelude.Text)
domainSummary_owner = Lens.lens (\DomainSummary' {owner} -> owner) (\s@DomainSummary' {} a -> s {owner = a} :: DomainSummary)

-- | The key used to encrypt the domain.
domainSummary_encryptionKey :: Lens.Lens' DomainSummary (Prelude.Maybe Prelude.Text)
domainSummary_encryptionKey = Lens.lens (\DomainSummary' {encryptionKey} -> encryptionKey) (\s@DomainSummary' {} a -> s {encryptionKey = a} :: DomainSummary)

instance Core.FromJSON DomainSummary where
  parseJSON =
    Core.withObject
      "DomainSummary"
      ( \x ->
          DomainSummary'
            Prelude.<$> (x Core..:? "name")
            Prelude.<*> (x Core..:? "createdTime")
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "status")
            Prelude.<*> (x Core..:? "owner")
            Prelude.<*> (x Core..:? "encryptionKey")
      )

instance Prelude.Hashable DomainSummary where
  hashWithSalt _salt DomainSummary' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` owner
      `Prelude.hashWithSalt` encryptionKey

instance Prelude.NFData DomainSummary where
  rnf DomainSummary' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf owner
      `Prelude.seq` Prelude.rnf encryptionKey
