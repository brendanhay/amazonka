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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VoiceId.Types.Domain where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.VoiceId.Types.DomainStatus
import Amazonka.VoiceId.Types.ServerSideEncryptionConfiguration
import Amazonka.VoiceId.Types.ServerSideEncryptionUpdateDetails

-- | Contains all the information about a domain.
--
-- /See:/ 'newDomain' smart constructor.
data Domain = Domain'
  { -- | The client-provided name for the domain.
    name :: Prelude.Maybe (Core.Sensitive Prelude.Text),
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
    description :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The current status of the domain.
    domainStatus :: Prelude.Maybe DomainStatus,
    -- | The service-generated identifier for the domain.
    domainId :: Prelude.Maybe Prelude.Text,
    -- | The timestamp at which the domain is created.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The timestamp showing the domain\'s last update.
    updatedAt :: Prelude.Maybe Core.POSIX
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
-- 'name', 'domain_name' - The client-provided name for the domain.
--
-- 'serverSideEncryptionUpdateDetails', 'domain_serverSideEncryptionUpdateDetails' - Details about the most recent server-side encryption configuration
-- update. When the server-side encryption configuration is changed,
-- dependency on the old KMS key is removed through an asynchronous
-- process. When this update is complete, the domain\'s data can only be
-- accessed using the new KMS key.
--
-- 'serverSideEncryptionConfiguration', 'domain_serverSideEncryptionConfiguration' - The server-side encryption configuration containing the KMS key
-- identifier you want Voice ID to use to encrypt your data.
--
-- 'arn', 'domain_arn' - The Amazon Resource Name (ARN) for the domain.
--
-- 'description', 'domain_description' - The client-provided description of the domain.
--
-- 'domainStatus', 'domain_domainStatus' - The current status of the domain.
--
-- 'domainId', 'domain_domainId' - The service-generated identifier for the domain.
--
-- 'createdAt', 'domain_createdAt' - The timestamp at which the domain is created.
--
-- 'updatedAt', 'domain_updatedAt' - The timestamp showing the domain\'s last update.
newDomain ::
  Domain
newDomain =
  Domain'
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
domain_name :: Lens.Lens' Domain (Prelude.Maybe Prelude.Text)
domain_name = Lens.lens (\Domain' {name} -> name) (\s@Domain' {} a -> s {name = a} :: Domain) Prelude.. Lens.mapping Core._Sensitive

-- | Details about the most recent server-side encryption configuration
-- update. When the server-side encryption configuration is changed,
-- dependency on the old KMS key is removed through an asynchronous
-- process. When this update is complete, the domain\'s data can only be
-- accessed using the new KMS key.
domain_serverSideEncryptionUpdateDetails :: Lens.Lens' Domain (Prelude.Maybe ServerSideEncryptionUpdateDetails)
domain_serverSideEncryptionUpdateDetails = Lens.lens (\Domain' {serverSideEncryptionUpdateDetails} -> serverSideEncryptionUpdateDetails) (\s@Domain' {} a -> s {serverSideEncryptionUpdateDetails = a} :: Domain)

-- | The server-side encryption configuration containing the KMS key
-- identifier you want Voice ID to use to encrypt your data.
domain_serverSideEncryptionConfiguration :: Lens.Lens' Domain (Prelude.Maybe ServerSideEncryptionConfiguration)
domain_serverSideEncryptionConfiguration = Lens.lens (\Domain' {serverSideEncryptionConfiguration} -> serverSideEncryptionConfiguration) (\s@Domain' {} a -> s {serverSideEncryptionConfiguration = a} :: Domain)

-- | The Amazon Resource Name (ARN) for the domain.
domain_arn :: Lens.Lens' Domain (Prelude.Maybe Prelude.Text)
domain_arn = Lens.lens (\Domain' {arn} -> arn) (\s@Domain' {} a -> s {arn = a} :: Domain)

-- | The client-provided description of the domain.
domain_description :: Lens.Lens' Domain (Prelude.Maybe Prelude.Text)
domain_description = Lens.lens (\Domain' {description} -> description) (\s@Domain' {} a -> s {description = a} :: Domain) Prelude.. Lens.mapping Core._Sensitive

-- | The current status of the domain.
domain_domainStatus :: Lens.Lens' Domain (Prelude.Maybe DomainStatus)
domain_domainStatus = Lens.lens (\Domain' {domainStatus} -> domainStatus) (\s@Domain' {} a -> s {domainStatus = a} :: Domain)

-- | The service-generated identifier for the domain.
domain_domainId :: Lens.Lens' Domain (Prelude.Maybe Prelude.Text)
domain_domainId = Lens.lens (\Domain' {domainId} -> domainId) (\s@Domain' {} a -> s {domainId = a} :: Domain)

-- | The timestamp at which the domain is created.
domain_createdAt :: Lens.Lens' Domain (Prelude.Maybe Prelude.UTCTime)
domain_createdAt = Lens.lens (\Domain' {createdAt} -> createdAt) (\s@Domain' {} a -> s {createdAt = a} :: Domain) Prelude.. Lens.mapping Core._Time

-- | The timestamp showing the domain\'s last update.
domain_updatedAt :: Lens.Lens' Domain (Prelude.Maybe Prelude.UTCTime)
domain_updatedAt = Lens.lens (\Domain' {updatedAt} -> updatedAt) (\s@Domain' {} a -> s {updatedAt = a} :: Domain) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON Domain where
  parseJSON =
    Core.withObject
      "Domain"
      ( \x ->
          Domain'
            Prelude.<$> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "ServerSideEncryptionUpdateDetails")
            Prelude.<*> (x Core..:? "ServerSideEncryptionConfiguration")
            Prelude.<*> (x Core..:? "Arn")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "DomainStatus")
            Prelude.<*> (x Core..:? "DomainId")
            Prelude.<*> (x Core..:? "CreatedAt")
            Prelude.<*> (x Core..:? "UpdatedAt")
      )

instance Prelude.Hashable Domain where
  hashWithSalt _salt Domain' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` serverSideEncryptionUpdateDetails
      `Prelude.hashWithSalt` serverSideEncryptionConfiguration
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` domainStatus
      `Prelude.hashWithSalt` domainId
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` updatedAt

instance Prelude.NFData Domain where
  rnf Domain' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf serverSideEncryptionUpdateDetails
      `Prelude.seq` Prelude.rnf serverSideEncryptionConfiguration
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf domainStatus
      `Prelude.seq` Prelude.rnf domainId
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf updatedAt
