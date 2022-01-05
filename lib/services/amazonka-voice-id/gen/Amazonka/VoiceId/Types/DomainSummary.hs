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
-- Module      : Amazonka.VoiceId.Types.DomainSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VoiceId.Types.DomainSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.VoiceId.Types.DomainStatus
import Amazonka.VoiceId.Types.ServerSideEncryptionConfiguration

-- | Contains a summary of information about a domain.
--
-- /See:/ 'newDomainSummary' smart constructor.
data DomainSummary = DomainSummary'
  { -- | The current status of the domain.
    domainStatus :: Prelude.Maybe DomainStatus,
    -- | The Amazon Resource Name (ARN) for the domain.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The timestamp showing when the domain is created.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The client-provided name for the domain.
    name :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The service-generated identifier for the domain.
    domainId :: Prelude.Maybe Prelude.Text,
    -- | The timestamp showing the domain\'s last update.
    updatedAt :: Prelude.Maybe Core.POSIX,
    -- | The client-provided description of the domain.
    description :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The server-side encryption configuration containing the KMS Key
    -- Identifier you want Voice ID to use to encrypt your data..
    serverSideEncryptionConfiguration :: Prelude.Maybe ServerSideEncryptionConfiguration
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DomainSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainStatus', 'domainSummary_domainStatus' - The current status of the domain.
--
-- 'arn', 'domainSummary_arn' - The Amazon Resource Name (ARN) for the domain.
--
-- 'createdAt', 'domainSummary_createdAt' - The timestamp showing when the domain is created.
--
-- 'name', 'domainSummary_name' - The client-provided name for the domain.
--
-- 'domainId', 'domainSummary_domainId' - The service-generated identifier for the domain.
--
-- 'updatedAt', 'domainSummary_updatedAt' - The timestamp showing the domain\'s last update.
--
-- 'description', 'domainSummary_description' - The client-provided description of the domain.
--
-- 'serverSideEncryptionConfiguration', 'domainSummary_serverSideEncryptionConfiguration' - The server-side encryption configuration containing the KMS Key
-- Identifier you want Voice ID to use to encrypt your data..
newDomainSummary ::
  DomainSummary
newDomainSummary =
  DomainSummary'
    { domainStatus = Prelude.Nothing,
      arn = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      name = Prelude.Nothing,
      domainId = Prelude.Nothing,
      updatedAt = Prelude.Nothing,
      description = Prelude.Nothing,
      serverSideEncryptionConfiguration = Prelude.Nothing
    }

-- | The current status of the domain.
domainSummary_domainStatus :: Lens.Lens' DomainSummary (Prelude.Maybe DomainStatus)
domainSummary_domainStatus = Lens.lens (\DomainSummary' {domainStatus} -> domainStatus) (\s@DomainSummary' {} a -> s {domainStatus = a} :: DomainSummary)

-- | The Amazon Resource Name (ARN) for the domain.
domainSummary_arn :: Lens.Lens' DomainSummary (Prelude.Maybe Prelude.Text)
domainSummary_arn = Lens.lens (\DomainSummary' {arn} -> arn) (\s@DomainSummary' {} a -> s {arn = a} :: DomainSummary)

-- | The timestamp showing when the domain is created.
domainSummary_createdAt :: Lens.Lens' DomainSummary (Prelude.Maybe Prelude.UTCTime)
domainSummary_createdAt = Lens.lens (\DomainSummary' {createdAt} -> createdAt) (\s@DomainSummary' {} a -> s {createdAt = a} :: DomainSummary) Prelude.. Lens.mapping Core._Time

-- | The client-provided name for the domain.
domainSummary_name :: Lens.Lens' DomainSummary (Prelude.Maybe Prelude.Text)
domainSummary_name = Lens.lens (\DomainSummary' {name} -> name) (\s@DomainSummary' {} a -> s {name = a} :: DomainSummary) Prelude.. Lens.mapping Core._Sensitive

-- | The service-generated identifier for the domain.
domainSummary_domainId :: Lens.Lens' DomainSummary (Prelude.Maybe Prelude.Text)
domainSummary_domainId = Lens.lens (\DomainSummary' {domainId} -> domainId) (\s@DomainSummary' {} a -> s {domainId = a} :: DomainSummary)

-- | The timestamp showing the domain\'s last update.
domainSummary_updatedAt :: Lens.Lens' DomainSummary (Prelude.Maybe Prelude.UTCTime)
domainSummary_updatedAt = Lens.lens (\DomainSummary' {updatedAt} -> updatedAt) (\s@DomainSummary' {} a -> s {updatedAt = a} :: DomainSummary) Prelude.. Lens.mapping Core._Time

-- | The client-provided description of the domain.
domainSummary_description :: Lens.Lens' DomainSummary (Prelude.Maybe Prelude.Text)
domainSummary_description = Lens.lens (\DomainSummary' {description} -> description) (\s@DomainSummary' {} a -> s {description = a} :: DomainSummary) Prelude.. Lens.mapping Core._Sensitive

-- | The server-side encryption configuration containing the KMS Key
-- Identifier you want Voice ID to use to encrypt your data..
domainSummary_serverSideEncryptionConfiguration :: Lens.Lens' DomainSummary (Prelude.Maybe ServerSideEncryptionConfiguration)
domainSummary_serverSideEncryptionConfiguration = Lens.lens (\DomainSummary' {serverSideEncryptionConfiguration} -> serverSideEncryptionConfiguration) (\s@DomainSummary' {} a -> s {serverSideEncryptionConfiguration = a} :: DomainSummary)

instance Core.FromJSON DomainSummary where
  parseJSON =
    Core.withObject
      "DomainSummary"
      ( \x ->
          DomainSummary'
            Prelude.<$> (x Core..:? "DomainStatus")
            Prelude.<*> (x Core..:? "Arn")
            Prelude.<*> (x Core..:? "CreatedAt")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "DomainId")
            Prelude.<*> (x Core..:? "UpdatedAt")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "ServerSideEncryptionConfiguration")
      )

instance Prelude.Hashable DomainSummary where
  hashWithSalt _salt DomainSummary' {..} =
    _salt `Prelude.hashWithSalt` domainStatus
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` domainId
      `Prelude.hashWithSalt` updatedAt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` serverSideEncryptionConfiguration

instance Prelude.NFData DomainSummary where
  rnf DomainSummary' {..} =
    Prelude.rnf domainStatus
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf domainId
      `Prelude.seq` Prelude.rnf updatedAt
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf serverSideEncryptionConfiguration
