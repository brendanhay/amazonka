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
-- Module      : Amazonka.CodeArtifact.Types.DomainDescription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeArtifact.Types.DomainDescription where

import Amazonka.CodeArtifact.Types.DomainStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about a domain. A domain is a container for repositories.
-- When you create a domain, it is empty until you add one or more
-- repositories.
--
-- /See:/ 'newDomainDescription' smart constructor.
data DomainDescription = DomainDescription'
  { -- | The Amazon Resource Name (ARN) of the domain.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The total size of all assets in the domain.
    assetSizeBytes :: Prelude.Maybe Prelude.Integer,
    -- | A timestamp that represents the date and time the domain was created.
    createdTime :: Prelude.Maybe Data.POSIX,
    -- | The ARN of an Key Management Service (KMS) key associated with a domain.
    encryptionKey :: Prelude.Maybe Prelude.Text,
    -- | The name of the domain.
    name :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services account ID that owns the domain.
    owner :: Prelude.Maybe Prelude.Text,
    -- | The number of repositories in the domain.
    repositoryCount :: Prelude.Maybe Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the Amazon S3 bucket that is used to
    -- store package assets in the domain.
    s3BucketArn :: Prelude.Maybe Prelude.Text,
    -- | The current status of a domain.
    status :: Prelude.Maybe DomainStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DomainDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'domainDescription_arn' - The Amazon Resource Name (ARN) of the domain.
--
-- 'assetSizeBytes', 'domainDescription_assetSizeBytes' - The total size of all assets in the domain.
--
-- 'createdTime', 'domainDescription_createdTime' - A timestamp that represents the date and time the domain was created.
--
-- 'encryptionKey', 'domainDescription_encryptionKey' - The ARN of an Key Management Service (KMS) key associated with a domain.
--
-- 'name', 'domainDescription_name' - The name of the domain.
--
-- 'owner', 'domainDescription_owner' - The Amazon Web Services account ID that owns the domain.
--
-- 'repositoryCount', 'domainDescription_repositoryCount' - The number of repositories in the domain.
--
-- 's3BucketArn', 'domainDescription_s3BucketArn' - The Amazon Resource Name (ARN) of the Amazon S3 bucket that is used to
-- store package assets in the domain.
--
-- 'status', 'domainDescription_status' - The current status of a domain.
newDomainDescription ::
  DomainDescription
newDomainDescription =
  DomainDescription'
    { arn = Prelude.Nothing,
      assetSizeBytes = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      encryptionKey = Prelude.Nothing,
      name = Prelude.Nothing,
      owner = Prelude.Nothing,
      repositoryCount = Prelude.Nothing,
      s3BucketArn = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the domain.
domainDescription_arn :: Lens.Lens' DomainDescription (Prelude.Maybe Prelude.Text)
domainDescription_arn = Lens.lens (\DomainDescription' {arn} -> arn) (\s@DomainDescription' {} a -> s {arn = a} :: DomainDescription)

-- | The total size of all assets in the domain.
domainDescription_assetSizeBytes :: Lens.Lens' DomainDescription (Prelude.Maybe Prelude.Integer)
domainDescription_assetSizeBytes = Lens.lens (\DomainDescription' {assetSizeBytes} -> assetSizeBytes) (\s@DomainDescription' {} a -> s {assetSizeBytes = a} :: DomainDescription)

-- | A timestamp that represents the date and time the domain was created.
domainDescription_createdTime :: Lens.Lens' DomainDescription (Prelude.Maybe Prelude.UTCTime)
domainDescription_createdTime = Lens.lens (\DomainDescription' {createdTime} -> createdTime) (\s@DomainDescription' {} a -> s {createdTime = a} :: DomainDescription) Prelude.. Lens.mapping Data._Time

-- | The ARN of an Key Management Service (KMS) key associated with a domain.
domainDescription_encryptionKey :: Lens.Lens' DomainDescription (Prelude.Maybe Prelude.Text)
domainDescription_encryptionKey = Lens.lens (\DomainDescription' {encryptionKey} -> encryptionKey) (\s@DomainDescription' {} a -> s {encryptionKey = a} :: DomainDescription)

-- | The name of the domain.
domainDescription_name :: Lens.Lens' DomainDescription (Prelude.Maybe Prelude.Text)
domainDescription_name = Lens.lens (\DomainDescription' {name} -> name) (\s@DomainDescription' {} a -> s {name = a} :: DomainDescription)

-- | The Amazon Web Services account ID that owns the domain.
domainDescription_owner :: Lens.Lens' DomainDescription (Prelude.Maybe Prelude.Text)
domainDescription_owner = Lens.lens (\DomainDescription' {owner} -> owner) (\s@DomainDescription' {} a -> s {owner = a} :: DomainDescription)

-- | The number of repositories in the domain.
domainDescription_repositoryCount :: Lens.Lens' DomainDescription (Prelude.Maybe Prelude.Int)
domainDescription_repositoryCount = Lens.lens (\DomainDescription' {repositoryCount} -> repositoryCount) (\s@DomainDescription' {} a -> s {repositoryCount = a} :: DomainDescription)

-- | The Amazon Resource Name (ARN) of the Amazon S3 bucket that is used to
-- store package assets in the domain.
domainDescription_s3BucketArn :: Lens.Lens' DomainDescription (Prelude.Maybe Prelude.Text)
domainDescription_s3BucketArn = Lens.lens (\DomainDescription' {s3BucketArn} -> s3BucketArn) (\s@DomainDescription' {} a -> s {s3BucketArn = a} :: DomainDescription)

-- | The current status of a domain.
domainDescription_status :: Lens.Lens' DomainDescription (Prelude.Maybe DomainStatus)
domainDescription_status = Lens.lens (\DomainDescription' {status} -> status) (\s@DomainDescription' {} a -> s {status = a} :: DomainDescription)

instance Data.FromJSON DomainDescription where
  parseJSON =
    Data.withObject
      "DomainDescription"
      ( \x ->
          DomainDescription'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "assetSizeBytes")
            Prelude.<*> (x Data..:? "createdTime")
            Prelude.<*> (x Data..:? "encryptionKey")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "owner")
            Prelude.<*> (x Data..:? "repositoryCount")
            Prelude.<*> (x Data..:? "s3BucketArn")
            Prelude.<*> (x Data..:? "status")
      )

instance Prelude.Hashable DomainDescription where
  hashWithSalt _salt DomainDescription' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` assetSizeBytes
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` encryptionKey
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` owner
      `Prelude.hashWithSalt` repositoryCount
      `Prelude.hashWithSalt` s3BucketArn
      `Prelude.hashWithSalt` status

instance Prelude.NFData DomainDescription where
  rnf DomainDescription' {..} =
    Prelude.rnf arn `Prelude.seq`
      Prelude.rnf assetSizeBytes `Prelude.seq`
        Prelude.rnf createdTime `Prelude.seq`
          Prelude.rnf encryptionKey `Prelude.seq`
            Prelude.rnf name `Prelude.seq`
              Prelude.rnf owner `Prelude.seq`
                Prelude.rnf repositoryCount `Prelude.seq`
                  Prelude.rnf s3BucketArn `Prelude.seq`
                    Prelude.rnf status
