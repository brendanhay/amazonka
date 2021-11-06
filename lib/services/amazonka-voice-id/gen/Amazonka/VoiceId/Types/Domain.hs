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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VoiceId.Types.Domain where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.VoiceId.Types.DomainStatus
import Amazonka.VoiceId.Types.ServerSideEncryptionConfiguration

-- | Contains all the information about a domain.
--
-- /See:/ 'newDomain' smart constructor.
data Domain = Domain'
  { -- | The current status of the domain.
    domainStatus :: Prelude.Maybe DomainStatus,
    -- | The Amazon Resource Name (ARN) for the domain.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The timestamp at which the domain is created.
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
    -- Identifier you want Voice ID to use to encrypt your data.
    serverSideEncryptionConfiguration :: Prelude.Maybe ServerSideEncryptionConfiguration
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
-- 'domainStatus', 'domain_domainStatus' - The current status of the domain.
--
-- 'arn', 'domain_arn' - The Amazon Resource Name (ARN) for the domain.
--
-- 'createdAt', 'domain_createdAt' - The timestamp at which the domain is created.
--
-- 'name', 'domain_name' - The client-provided name for the domain.
--
-- 'domainId', 'domain_domainId' - The service-generated identifier for the domain.
--
-- 'updatedAt', 'domain_updatedAt' - The timestamp showing the domain\'s last update.
--
-- 'description', 'domain_description' - The client-provided description of the domain.
--
-- 'serverSideEncryptionConfiguration', 'domain_serverSideEncryptionConfiguration' - The server-side encryption configuration containing the KMS Key
-- Identifier you want Voice ID to use to encrypt your data.
newDomain ::
  Domain
newDomain =
  Domain'
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
domain_domainStatus :: Lens.Lens' Domain (Prelude.Maybe DomainStatus)
domain_domainStatus = Lens.lens (\Domain' {domainStatus} -> domainStatus) (\s@Domain' {} a -> s {domainStatus = a} :: Domain)

-- | The Amazon Resource Name (ARN) for the domain.
domain_arn :: Lens.Lens' Domain (Prelude.Maybe Prelude.Text)
domain_arn = Lens.lens (\Domain' {arn} -> arn) (\s@Domain' {} a -> s {arn = a} :: Domain)

-- | The timestamp at which the domain is created.
domain_createdAt :: Lens.Lens' Domain (Prelude.Maybe Prelude.UTCTime)
domain_createdAt = Lens.lens (\Domain' {createdAt} -> createdAt) (\s@Domain' {} a -> s {createdAt = a} :: Domain) Prelude.. Lens.mapping Core._Time

-- | The client-provided name for the domain.
domain_name :: Lens.Lens' Domain (Prelude.Maybe Prelude.Text)
domain_name = Lens.lens (\Domain' {name} -> name) (\s@Domain' {} a -> s {name = a} :: Domain) Prelude.. Lens.mapping Core._Sensitive

-- | The service-generated identifier for the domain.
domain_domainId :: Lens.Lens' Domain (Prelude.Maybe Prelude.Text)
domain_domainId = Lens.lens (\Domain' {domainId} -> domainId) (\s@Domain' {} a -> s {domainId = a} :: Domain)

-- | The timestamp showing the domain\'s last update.
domain_updatedAt :: Lens.Lens' Domain (Prelude.Maybe Prelude.UTCTime)
domain_updatedAt = Lens.lens (\Domain' {updatedAt} -> updatedAt) (\s@Domain' {} a -> s {updatedAt = a} :: Domain) Prelude.. Lens.mapping Core._Time

-- | The client-provided description of the domain.
domain_description :: Lens.Lens' Domain (Prelude.Maybe Prelude.Text)
domain_description = Lens.lens (\Domain' {description} -> description) (\s@Domain' {} a -> s {description = a} :: Domain) Prelude.. Lens.mapping Core._Sensitive

-- | The server-side encryption configuration containing the KMS Key
-- Identifier you want Voice ID to use to encrypt your data.
domain_serverSideEncryptionConfiguration :: Lens.Lens' Domain (Prelude.Maybe ServerSideEncryptionConfiguration)
domain_serverSideEncryptionConfiguration = Lens.lens (\Domain' {serverSideEncryptionConfiguration} -> serverSideEncryptionConfiguration) (\s@Domain' {} a -> s {serverSideEncryptionConfiguration = a} :: Domain)

instance Core.FromJSON Domain where
  parseJSON =
    Core.withObject
      "Domain"
      ( \x ->
          Domain'
            Prelude.<$> (x Core..:? "DomainStatus")
            Prelude.<*> (x Core..:? "Arn")
            Prelude.<*> (x Core..:? "CreatedAt")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "DomainId")
            Prelude.<*> (x Core..:? "UpdatedAt")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "ServerSideEncryptionConfiguration")
      )

instance Prelude.Hashable Domain

instance Prelude.NFData Domain
