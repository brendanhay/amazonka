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
-- Module      : Amazonka.VoiceId.Types.Domain
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VoiceId.Types.Domain where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.VoiceId.Types.DomainStatus
import Amazonka.VoiceId.Types.ServerSideEncryptionConfiguration
import Amazonka.VoiceId.Types.ServerSideEncryptionUpdateDetails

-- | Contains all the information about a domain.
--
-- /See:/ 'newDomain' smart constructor.
data Domain = Domain'
  { -- | The Amazon Resource Name (ARN) for the domain.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The timestamp at which the domain is created.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | The client-provided description of the domain.
    description :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The service-generated identifier for the domain.
    domainId :: Prelude.Maybe Prelude.Text,
    -- | The current status of the domain.
    domainStatus :: Prelude.Maybe DomainStatus,
    -- | The client-provided name for the domain.
    name :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The server-side encryption configuration containing the KMS key
    -- identifier you want Voice ID to use to encrypt your data.
    serverSideEncryptionConfiguration :: Prelude.Maybe ServerSideEncryptionConfiguration,
    -- | Details about the most recent server-side encryption configuration
    -- update. When the server-side encryption configuration is changed,
    -- dependency on the old KMS key is removed through an asynchronous
    -- process. When this update is complete, the domain\'s data can only be
    -- accessed using the new KMS key.
    serverSideEncryptionUpdateDetails :: Prelude.Maybe ServerSideEncryptionUpdateDetails,
    -- | The timestamp showing the domain\'s last update.
    updatedAt :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Domain' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'domain_arn' - The Amazon Resource Name (ARN) for the domain.
--
-- 'createdAt', 'domain_createdAt' - The timestamp at which the domain is created.
--
-- 'description', 'domain_description' - The client-provided description of the domain.
--
-- 'domainId', 'domain_domainId' - The service-generated identifier for the domain.
--
-- 'domainStatus', 'domain_domainStatus' - The current status of the domain.
--
-- 'name', 'domain_name' - The client-provided name for the domain.
--
-- 'serverSideEncryptionConfiguration', 'domain_serverSideEncryptionConfiguration' - The server-side encryption configuration containing the KMS key
-- identifier you want Voice ID to use to encrypt your data.
--
-- 'serverSideEncryptionUpdateDetails', 'domain_serverSideEncryptionUpdateDetails' - Details about the most recent server-side encryption configuration
-- update. When the server-side encryption configuration is changed,
-- dependency on the old KMS key is removed through an asynchronous
-- process. When this update is complete, the domain\'s data can only be
-- accessed using the new KMS key.
--
-- 'updatedAt', 'domain_updatedAt' - The timestamp showing the domain\'s last update.
newDomain ::
  Domain
newDomain =
  Domain'
    { arn = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      description = Prelude.Nothing,
      domainId = Prelude.Nothing,
      domainStatus = Prelude.Nothing,
      name = Prelude.Nothing,
      serverSideEncryptionConfiguration = Prelude.Nothing,
      serverSideEncryptionUpdateDetails = Prelude.Nothing,
      updatedAt = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) for the domain.
domain_arn :: Lens.Lens' Domain (Prelude.Maybe Prelude.Text)
domain_arn = Lens.lens (\Domain' {arn} -> arn) (\s@Domain' {} a -> s {arn = a} :: Domain)

-- | The timestamp at which the domain is created.
domain_createdAt :: Lens.Lens' Domain (Prelude.Maybe Prelude.UTCTime)
domain_createdAt = Lens.lens (\Domain' {createdAt} -> createdAt) (\s@Domain' {} a -> s {createdAt = a} :: Domain) Prelude.. Lens.mapping Data._Time

-- | The client-provided description of the domain.
domain_description :: Lens.Lens' Domain (Prelude.Maybe Prelude.Text)
domain_description = Lens.lens (\Domain' {description} -> description) (\s@Domain' {} a -> s {description = a} :: Domain) Prelude.. Lens.mapping Data._Sensitive

-- | The service-generated identifier for the domain.
domain_domainId :: Lens.Lens' Domain (Prelude.Maybe Prelude.Text)
domain_domainId = Lens.lens (\Domain' {domainId} -> domainId) (\s@Domain' {} a -> s {domainId = a} :: Domain)

-- | The current status of the domain.
domain_domainStatus :: Lens.Lens' Domain (Prelude.Maybe DomainStatus)
domain_domainStatus = Lens.lens (\Domain' {domainStatus} -> domainStatus) (\s@Domain' {} a -> s {domainStatus = a} :: Domain)

-- | The client-provided name for the domain.
domain_name :: Lens.Lens' Domain (Prelude.Maybe Prelude.Text)
domain_name = Lens.lens (\Domain' {name} -> name) (\s@Domain' {} a -> s {name = a} :: Domain) Prelude.. Lens.mapping Data._Sensitive

-- | The server-side encryption configuration containing the KMS key
-- identifier you want Voice ID to use to encrypt your data.
domain_serverSideEncryptionConfiguration :: Lens.Lens' Domain (Prelude.Maybe ServerSideEncryptionConfiguration)
domain_serverSideEncryptionConfiguration = Lens.lens (\Domain' {serverSideEncryptionConfiguration} -> serverSideEncryptionConfiguration) (\s@Domain' {} a -> s {serverSideEncryptionConfiguration = a} :: Domain)

-- | Details about the most recent server-side encryption configuration
-- update. When the server-side encryption configuration is changed,
-- dependency on the old KMS key is removed through an asynchronous
-- process. When this update is complete, the domain\'s data can only be
-- accessed using the new KMS key.
domain_serverSideEncryptionUpdateDetails :: Lens.Lens' Domain (Prelude.Maybe ServerSideEncryptionUpdateDetails)
domain_serverSideEncryptionUpdateDetails = Lens.lens (\Domain' {serverSideEncryptionUpdateDetails} -> serverSideEncryptionUpdateDetails) (\s@Domain' {} a -> s {serverSideEncryptionUpdateDetails = a} :: Domain)

-- | The timestamp showing the domain\'s last update.
domain_updatedAt :: Lens.Lens' Domain (Prelude.Maybe Prelude.UTCTime)
domain_updatedAt = Lens.lens (\Domain' {updatedAt} -> updatedAt) (\s@Domain' {} a -> s {updatedAt = a} :: Domain) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON Domain where
  parseJSON =
    Data.withObject
      "Domain"
      ( \x ->
          Domain'
            Prelude.<$> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "CreatedAt")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "DomainId")
            Prelude.<*> (x Data..:? "DomainStatus")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "ServerSideEncryptionConfiguration")
            Prelude.<*> (x Data..:? "ServerSideEncryptionUpdateDetails")
            Prelude.<*> (x Data..:? "UpdatedAt")
      )

instance Prelude.Hashable Domain where
  hashWithSalt _salt Domain' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` domainId
      `Prelude.hashWithSalt` domainStatus
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` serverSideEncryptionConfiguration
      `Prelude.hashWithSalt` serverSideEncryptionUpdateDetails
      `Prelude.hashWithSalt` updatedAt

instance Prelude.NFData Domain where
  rnf Domain' {..} =
    Prelude.rnf arn `Prelude.seq`
      Prelude.rnf createdAt `Prelude.seq`
        Prelude.rnf description `Prelude.seq`
          Prelude.rnf domainId `Prelude.seq`
            Prelude.rnf domainStatus `Prelude.seq`
              Prelude.rnf name `Prelude.seq`
                Prelude.rnf serverSideEncryptionConfiguration `Prelude.seq`
                  Prelude.rnf serverSideEncryptionUpdateDetails `Prelude.seq`
                    Prelude.rnf updatedAt
