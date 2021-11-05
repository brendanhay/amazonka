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
-- Module      : Network.AWS.CodeArtifact.Types.DomainDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeArtifact.Types.DomainDescription where

import Network.AWS.CodeArtifact.Types.DomainStatus
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about a domain. A domain is a container for repositories.
-- When you create a domain, it is empty until you add one or more
-- repositories.
--
-- /See:/ 'newDomainDescription' smart constructor.
data DomainDescription = DomainDescription'
  { -- | The total size of all assets in the domain.
    assetSizeBytes :: Prelude.Maybe Prelude.Integer,
    -- | The current status of a domain. The valid values are
    --
    -- -   @Active@
    --
    -- -   @Deleted@
    status :: Prelude.Maybe DomainStatus,
    -- | The Amazon Resource Name (ARN) of the domain.
    arn :: Prelude.Maybe Prelude.Text,
    -- | A timestamp that represents the date and time the domain was created.
    createdTime :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the Amazon S3 bucket that is used to
    -- store package assets in the domain.
    s3BucketArn :: Prelude.Maybe Prelude.Text,
    -- | The AWS account ID that owns the domain.
    owner :: Prelude.Maybe Prelude.Text,
    -- | The number of repositories in the domain.
    repositoryCount :: Prelude.Maybe Prelude.Int,
    -- | The name of the domain.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ARN of an AWS Key Management Service (AWS KMS) key associated with a
    -- domain.
    encryptionKey :: Prelude.Maybe Prelude.Text
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
-- 'assetSizeBytes', 'domainDescription_assetSizeBytes' - The total size of all assets in the domain.
--
-- 'status', 'domainDescription_status' - The current status of a domain. The valid values are
--
-- -   @Active@
--
-- -   @Deleted@
--
-- 'arn', 'domainDescription_arn' - The Amazon Resource Name (ARN) of the domain.
--
-- 'createdTime', 'domainDescription_createdTime' - A timestamp that represents the date and time the domain was created.
--
-- 's3BucketArn', 'domainDescription_s3BucketArn' - The Amazon Resource Name (ARN) of the Amazon S3 bucket that is used to
-- store package assets in the domain.
--
-- 'owner', 'domainDescription_owner' - The AWS account ID that owns the domain.
--
-- 'repositoryCount', 'domainDescription_repositoryCount' - The number of repositories in the domain.
--
-- 'name', 'domainDescription_name' - The name of the domain.
--
-- 'encryptionKey', 'domainDescription_encryptionKey' - The ARN of an AWS Key Management Service (AWS KMS) key associated with a
-- domain.
newDomainDescription ::
  DomainDescription
newDomainDescription =
  DomainDescription'
    { assetSizeBytes =
        Prelude.Nothing,
      status = Prelude.Nothing,
      arn = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      s3BucketArn = Prelude.Nothing,
      owner = Prelude.Nothing,
      repositoryCount = Prelude.Nothing,
      name = Prelude.Nothing,
      encryptionKey = Prelude.Nothing
    }

-- | The total size of all assets in the domain.
domainDescription_assetSizeBytes :: Lens.Lens' DomainDescription (Prelude.Maybe Prelude.Integer)
domainDescription_assetSizeBytes = Lens.lens (\DomainDescription' {assetSizeBytes} -> assetSizeBytes) (\s@DomainDescription' {} a -> s {assetSizeBytes = a} :: DomainDescription)

-- | The current status of a domain. The valid values are
--
-- -   @Active@
--
-- -   @Deleted@
domainDescription_status :: Lens.Lens' DomainDescription (Prelude.Maybe DomainStatus)
domainDescription_status = Lens.lens (\DomainDescription' {status} -> status) (\s@DomainDescription' {} a -> s {status = a} :: DomainDescription)

-- | The Amazon Resource Name (ARN) of the domain.
domainDescription_arn :: Lens.Lens' DomainDescription (Prelude.Maybe Prelude.Text)
domainDescription_arn = Lens.lens (\DomainDescription' {arn} -> arn) (\s@DomainDescription' {} a -> s {arn = a} :: DomainDescription)

-- | A timestamp that represents the date and time the domain was created.
domainDescription_createdTime :: Lens.Lens' DomainDescription (Prelude.Maybe Prelude.UTCTime)
domainDescription_createdTime = Lens.lens (\DomainDescription' {createdTime} -> createdTime) (\s@DomainDescription' {} a -> s {createdTime = a} :: DomainDescription) Prelude.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the Amazon S3 bucket that is used to
-- store package assets in the domain.
domainDescription_s3BucketArn :: Lens.Lens' DomainDescription (Prelude.Maybe Prelude.Text)
domainDescription_s3BucketArn = Lens.lens (\DomainDescription' {s3BucketArn} -> s3BucketArn) (\s@DomainDescription' {} a -> s {s3BucketArn = a} :: DomainDescription)

-- | The AWS account ID that owns the domain.
domainDescription_owner :: Lens.Lens' DomainDescription (Prelude.Maybe Prelude.Text)
domainDescription_owner = Lens.lens (\DomainDescription' {owner} -> owner) (\s@DomainDescription' {} a -> s {owner = a} :: DomainDescription)

-- | The number of repositories in the domain.
domainDescription_repositoryCount :: Lens.Lens' DomainDescription (Prelude.Maybe Prelude.Int)
domainDescription_repositoryCount = Lens.lens (\DomainDescription' {repositoryCount} -> repositoryCount) (\s@DomainDescription' {} a -> s {repositoryCount = a} :: DomainDescription)

-- | The name of the domain.
domainDescription_name :: Lens.Lens' DomainDescription (Prelude.Maybe Prelude.Text)
domainDescription_name = Lens.lens (\DomainDescription' {name} -> name) (\s@DomainDescription' {} a -> s {name = a} :: DomainDescription)

-- | The ARN of an AWS Key Management Service (AWS KMS) key associated with a
-- domain.
domainDescription_encryptionKey :: Lens.Lens' DomainDescription (Prelude.Maybe Prelude.Text)
domainDescription_encryptionKey = Lens.lens (\DomainDescription' {encryptionKey} -> encryptionKey) (\s@DomainDescription' {} a -> s {encryptionKey = a} :: DomainDescription)

instance Core.FromJSON DomainDescription where
  parseJSON =
    Core.withObject
      "DomainDescription"
      ( \x ->
          DomainDescription'
            Prelude.<$> (x Core..:? "assetSizeBytes")
            Prelude.<*> (x Core..:? "status")
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "createdTime")
            Prelude.<*> (x Core..:? "s3BucketArn")
            Prelude.<*> (x Core..:? "owner")
            Prelude.<*> (x Core..:? "repositoryCount")
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "encryptionKey")
      )

instance Prelude.Hashable DomainDescription

instance Prelude.NFData DomainDescription
