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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VoiceId.Types.DomainSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.VoiceId.Types.DomainStatus
import Amazonka.VoiceId.Types.ServerSideEncryptionConfiguration
import Amazonka.VoiceId.Types.ServerSideEncryptionUpdateDetails

-- | Contains a summary of information about a domain.
--
-- /See:/ 'newDomainSummary' smart constructor.
data DomainSummary = DomainSummary'
  { -- | The client-provided name for the domain.
    name :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | Details about the most recent server-side encryption configuration
    -- update. When the server-side encryption configuration is changed,
    -- dependency on the old KMS key is removed through an asynchronous
    -- process. When this update is complete, the domain\'s data can only be
    -- accessed using the new KMS key.
    serverSideEncryptionUpdateDetails :: Prelude.Maybe ServerSideEncryptionUpdateDetails,
    -- | The server-side encryption configuration containing the KMS key
    -- identifier you want Voice ID to use to encrypt your data.
    serverSideEncryptionConfiguration :: Prelude.Maybe ServerSideEncryptionConfiguration,
    -- | The Amazon Resource Name (ARN) for the domain.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The client-provided description of the domain.
    description :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The current status of the domain.
    domainStatus :: Prelude.Maybe DomainStatus,
    -- | The service-generated identifier for the domain.
    domainId :: Prelude.Maybe Prelude.Text,
    -- | The timestamp showing when the domain is created.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | The timestamp showing the domain\'s last update.
    updatedAt :: Prelude.Maybe Data.POSIX
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
-- 'name', 'domainSummary_name' - The client-provided name for the domain.
--
-- 'serverSideEncryptionUpdateDetails', 'domainSummary_serverSideEncryptionUpdateDetails' - Details about the most recent server-side encryption configuration
-- update. When the server-side encryption configuration is changed,
-- dependency on the old KMS key is removed through an asynchronous
-- process. When this update is complete, the domain\'s data can only be
-- accessed using the new KMS key.
--
-- 'serverSideEncryptionConfiguration', 'domainSummary_serverSideEncryptionConfiguration' - The server-side encryption configuration containing the KMS key
-- identifier you want Voice ID to use to encrypt your data.
--
-- 'arn', 'domainSummary_arn' - The Amazon Resource Name (ARN) for the domain.
--
-- 'description', 'domainSummary_description' - The client-provided description of the domain.
--
-- 'domainStatus', 'domainSummary_domainStatus' - The current status of the domain.
--
-- 'domainId', 'domainSummary_domainId' - The service-generated identifier for the domain.
--
-- 'createdAt', 'domainSummary_createdAt' - The timestamp showing when the domain is created.
--
-- 'updatedAt', 'domainSummary_updatedAt' - The timestamp showing the domain\'s last update.
newDomainSummary ::
  DomainSummary
newDomainSummary =
  DomainSummary'
    { name = Prelude.Nothing,
      serverSideEncryptionUpdateDetails = Prelude.Nothing,
      serverSideEncryptionConfiguration = Prelude.Nothing,
      arn = Prelude.Nothing,
      description = Prelude.Nothing,
      domainStatus = Prelude.Nothing,
      domainId = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      updatedAt = Prelude.Nothing
    }

-- | The client-provided name for the domain.
domainSummary_name :: Lens.Lens' DomainSummary (Prelude.Maybe Prelude.Text)
domainSummary_name = Lens.lens (\DomainSummary' {name} -> name) (\s@DomainSummary' {} a -> s {name = a} :: DomainSummary) Prelude.. Lens.mapping Data._Sensitive

-- | Details about the most recent server-side encryption configuration
-- update. When the server-side encryption configuration is changed,
-- dependency on the old KMS key is removed through an asynchronous
-- process. When this update is complete, the domain\'s data can only be
-- accessed using the new KMS key.
domainSummary_serverSideEncryptionUpdateDetails :: Lens.Lens' DomainSummary (Prelude.Maybe ServerSideEncryptionUpdateDetails)
domainSummary_serverSideEncryptionUpdateDetails = Lens.lens (\DomainSummary' {serverSideEncryptionUpdateDetails} -> serverSideEncryptionUpdateDetails) (\s@DomainSummary' {} a -> s {serverSideEncryptionUpdateDetails = a} :: DomainSummary)

-- | The server-side encryption configuration containing the KMS key
-- identifier you want Voice ID to use to encrypt your data.
domainSummary_serverSideEncryptionConfiguration :: Lens.Lens' DomainSummary (Prelude.Maybe ServerSideEncryptionConfiguration)
domainSummary_serverSideEncryptionConfiguration = Lens.lens (\DomainSummary' {serverSideEncryptionConfiguration} -> serverSideEncryptionConfiguration) (\s@DomainSummary' {} a -> s {serverSideEncryptionConfiguration = a} :: DomainSummary)

-- | The Amazon Resource Name (ARN) for the domain.
domainSummary_arn :: Lens.Lens' DomainSummary (Prelude.Maybe Prelude.Text)
domainSummary_arn = Lens.lens (\DomainSummary' {arn} -> arn) (\s@DomainSummary' {} a -> s {arn = a} :: DomainSummary)

-- | The client-provided description of the domain.
domainSummary_description :: Lens.Lens' DomainSummary (Prelude.Maybe Prelude.Text)
domainSummary_description = Lens.lens (\DomainSummary' {description} -> description) (\s@DomainSummary' {} a -> s {description = a} :: DomainSummary) Prelude.. Lens.mapping Data._Sensitive

-- | The current status of the domain.
domainSummary_domainStatus :: Lens.Lens' DomainSummary (Prelude.Maybe DomainStatus)
domainSummary_domainStatus = Lens.lens (\DomainSummary' {domainStatus} -> domainStatus) (\s@DomainSummary' {} a -> s {domainStatus = a} :: DomainSummary)

-- | The service-generated identifier for the domain.
domainSummary_domainId :: Lens.Lens' DomainSummary (Prelude.Maybe Prelude.Text)
domainSummary_domainId = Lens.lens (\DomainSummary' {domainId} -> domainId) (\s@DomainSummary' {} a -> s {domainId = a} :: DomainSummary)

-- | The timestamp showing when the domain is created.
domainSummary_createdAt :: Lens.Lens' DomainSummary (Prelude.Maybe Prelude.UTCTime)
domainSummary_createdAt = Lens.lens (\DomainSummary' {createdAt} -> createdAt) (\s@DomainSummary' {} a -> s {createdAt = a} :: DomainSummary) Prelude.. Lens.mapping Data._Time

-- | The timestamp showing the domain\'s last update.
domainSummary_updatedAt :: Lens.Lens' DomainSummary (Prelude.Maybe Prelude.UTCTime)
domainSummary_updatedAt = Lens.lens (\DomainSummary' {updatedAt} -> updatedAt) (\s@DomainSummary' {} a -> s {updatedAt = a} :: DomainSummary) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON DomainSummary where
  parseJSON =
    Data.withObject
      "DomainSummary"
      ( \x ->
          DomainSummary'
            Prelude.<$> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "ServerSideEncryptionUpdateDetails")
            Prelude.<*> (x Data..:? "ServerSideEncryptionConfiguration")
            Prelude.<*> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "DomainStatus")
            Prelude.<*> (x Data..:? "DomainId")
            Prelude.<*> (x Data..:? "CreatedAt")
            Prelude.<*> (x Data..:? "UpdatedAt")
      )

instance Prelude.Hashable DomainSummary where
  hashWithSalt _salt DomainSummary' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` serverSideEncryptionUpdateDetails
      `Prelude.hashWithSalt` serverSideEncryptionConfiguration
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` domainStatus
      `Prelude.hashWithSalt` domainId
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` updatedAt

instance Prelude.NFData DomainSummary where
  rnf DomainSummary' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf serverSideEncryptionUpdateDetails
      `Prelude.seq` Prelude.rnf serverSideEncryptionConfiguration
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf domainStatus
      `Prelude.seq` Prelude.rnf domainId
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf updatedAt
