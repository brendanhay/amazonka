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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeArtifact.Types.DomainSummary where

import Amazonka.CodeArtifact.Types.DomainStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about a domain, including its name, Amazon Resource Name
-- (ARN), and status. The
-- <https://docs.aws.amazon.com/codeartifact/latest/APIReference/API_ListDomains.html ListDomains>
-- operation returns a list of @DomainSummary@ objects.
--
-- /See:/ 'newDomainSummary' smart constructor.
data DomainSummary = DomainSummary'
  { -- | The ARN of the domain.
    arn :: Prelude.Maybe Prelude.Text,
    -- | A timestamp that contains the date and time the domain was created.
    createdTime :: Prelude.Maybe Data.POSIX,
    -- | The key used to encrypt the domain.
    encryptionKey :: Prelude.Maybe Prelude.Text,
    -- | The name of the domain.
    name :: Prelude.Maybe Prelude.Text,
    -- | The 12-digit account number of the Amazon Web Services account that owns
    -- the domain. It does not include dashes or spaces.
    owner :: Prelude.Maybe Prelude.Text,
    -- | A string that contains the status of the domain.
    status :: Prelude.Maybe DomainStatus
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
-- 'arn', 'domainSummary_arn' - The ARN of the domain.
--
-- 'createdTime', 'domainSummary_createdTime' - A timestamp that contains the date and time the domain was created.
--
-- 'encryptionKey', 'domainSummary_encryptionKey' - The key used to encrypt the domain.
--
-- 'name', 'domainSummary_name' - The name of the domain.
--
-- 'owner', 'domainSummary_owner' - The 12-digit account number of the Amazon Web Services account that owns
-- the domain. It does not include dashes or spaces.
--
-- 'status', 'domainSummary_status' - A string that contains the status of the domain.
newDomainSummary ::
  DomainSummary
newDomainSummary =
  DomainSummary'
    { arn = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      encryptionKey = Prelude.Nothing,
      name = Prelude.Nothing,
      owner = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The ARN of the domain.
domainSummary_arn :: Lens.Lens' DomainSummary (Prelude.Maybe Prelude.Text)
domainSummary_arn = Lens.lens (\DomainSummary' {arn} -> arn) (\s@DomainSummary' {} a -> s {arn = a} :: DomainSummary)

-- | A timestamp that contains the date and time the domain was created.
domainSummary_createdTime :: Lens.Lens' DomainSummary (Prelude.Maybe Prelude.UTCTime)
domainSummary_createdTime = Lens.lens (\DomainSummary' {createdTime} -> createdTime) (\s@DomainSummary' {} a -> s {createdTime = a} :: DomainSummary) Prelude.. Lens.mapping Data._Time

-- | The key used to encrypt the domain.
domainSummary_encryptionKey :: Lens.Lens' DomainSummary (Prelude.Maybe Prelude.Text)
domainSummary_encryptionKey = Lens.lens (\DomainSummary' {encryptionKey} -> encryptionKey) (\s@DomainSummary' {} a -> s {encryptionKey = a} :: DomainSummary)

-- | The name of the domain.
domainSummary_name :: Lens.Lens' DomainSummary (Prelude.Maybe Prelude.Text)
domainSummary_name = Lens.lens (\DomainSummary' {name} -> name) (\s@DomainSummary' {} a -> s {name = a} :: DomainSummary)

-- | The 12-digit account number of the Amazon Web Services account that owns
-- the domain. It does not include dashes or spaces.
domainSummary_owner :: Lens.Lens' DomainSummary (Prelude.Maybe Prelude.Text)
domainSummary_owner = Lens.lens (\DomainSummary' {owner} -> owner) (\s@DomainSummary' {} a -> s {owner = a} :: DomainSummary)

-- | A string that contains the status of the domain.
domainSummary_status :: Lens.Lens' DomainSummary (Prelude.Maybe DomainStatus)
domainSummary_status = Lens.lens (\DomainSummary' {status} -> status) (\s@DomainSummary' {} a -> s {status = a} :: DomainSummary)

instance Data.FromJSON DomainSummary where
  parseJSON =
    Data.withObject
      "DomainSummary"
      ( \x ->
          DomainSummary'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "createdTime")
            Prelude.<*> (x Data..:? "encryptionKey")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "owner")
            Prelude.<*> (x Data..:? "status")
      )

instance Prelude.Hashable DomainSummary where
  hashWithSalt _salt DomainSummary' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` encryptionKey
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` owner
      `Prelude.hashWithSalt` status

instance Prelude.NFData DomainSummary where
  rnf DomainSummary' {..} =
    Prelude.rnf arn `Prelude.seq`
      Prelude.rnf createdTime `Prelude.seq`
        Prelude.rnf encryptionKey `Prelude.seq`
          Prelude.rnf name `Prelude.seq`
            Prelude.rnf owner `Prelude.seq`
              Prelude.rnf status
