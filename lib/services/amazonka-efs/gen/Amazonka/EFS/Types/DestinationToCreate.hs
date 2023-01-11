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
-- Module      : Amazonka.EFS.Types.DestinationToCreate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EFS.Types.DestinationToCreate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the destination file system to create in the replication
-- configuration.
--
-- /See:/ 'newDestinationToCreate' smart constructor.
data DestinationToCreate = DestinationToCreate'
  { -- | To create a file system that uses EFS One Zone storage, specify the name
    -- of the Availability Zone in which to create the destination file system.
    availabilityZoneName :: Prelude.Maybe Prelude.Text,
    -- | Specifies the Key Management Service (KMS) key that you want to use to
    -- encrypt the destination file system. If you do not specify a KMS key,
    -- Amazon EFS uses your default KMS key for Amazon EFS,
    -- @\/aws\/elasticfilesystem@. This ID can be in one of the following
    -- formats:
    --
    -- -   Key ID - The unique identifier of the key, for example
    --     @1234abcd-12ab-34cd-56ef-1234567890ab@.
    --
    -- -   ARN - The Amazon Resource Name (ARN) for the key, for example
    --     @arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@.
    --
    -- -   Key alias - A previously created display name for a key, for example
    --     @alias\/projectKey1@.
    --
    -- -   Key alias ARN - The ARN for a key alias, for example
    --     @arn:aws:kms:us-west-2:444455556666:alias\/projectKey1@.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | To create a file system that uses Regional storage, specify the Amazon
    -- Web Services Region in which to create the destination file system.
    region :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DestinationToCreate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availabilityZoneName', 'destinationToCreate_availabilityZoneName' - To create a file system that uses EFS One Zone storage, specify the name
-- of the Availability Zone in which to create the destination file system.
--
-- 'kmsKeyId', 'destinationToCreate_kmsKeyId' - Specifies the Key Management Service (KMS) key that you want to use to
-- encrypt the destination file system. If you do not specify a KMS key,
-- Amazon EFS uses your default KMS key for Amazon EFS,
-- @\/aws\/elasticfilesystem@. This ID can be in one of the following
-- formats:
--
-- -   Key ID - The unique identifier of the key, for example
--     @1234abcd-12ab-34cd-56ef-1234567890ab@.
--
-- -   ARN - The Amazon Resource Name (ARN) for the key, for example
--     @arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@.
--
-- -   Key alias - A previously created display name for a key, for example
--     @alias\/projectKey1@.
--
-- -   Key alias ARN - The ARN for a key alias, for example
--     @arn:aws:kms:us-west-2:444455556666:alias\/projectKey1@.
--
-- 'region', 'destinationToCreate_region' - To create a file system that uses Regional storage, specify the Amazon
-- Web Services Region in which to create the destination file system.
newDestinationToCreate ::
  DestinationToCreate
newDestinationToCreate =
  DestinationToCreate'
    { availabilityZoneName =
        Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      region = Prelude.Nothing
    }

-- | To create a file system that uses EFS One Zone storage, specify the name
-- of the Availability Zone in which to create the destination file system.
destinationToCreate_availabilityZoneName :: Lens.Lens' DestinationToCreate (Prelude.Maybe Prelude.Text)
destinationToCreate_availabilityZoneName = Lens.lens (\DestinationToCreate' {availabilityZoneName} -> availabilityZoneName) (\s@DestinationToCreate' {} a -> s {availabilityZoneName = a} :: DestinationToCreate)

-- | Specifies the Key Management Service (KMS) key that you want to use to
-- encrypt the destination file system. If you do not specify a KMS key,
-- Amazon EFS uses your default KMS key for Amazon EFS,
-- @\/aws\/elasticfilesystem@. This ID can be in one of the following
-- formats:
--
-- -   Key ID - The unique identifier of the key, for example
--     @1234abcd-12ab-34cd-56ef-1234567890ab@.
--
-- -   ARN - The Amazon Resource Name (ARN) for the key, for example
--     @arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@.
--
-- -   Key alias - A previously created display name for a key, for example
--     @alias\/projectKey1@.
--
-- -   Key alias ARN - The ARN for a key alias, for example
--     @arn:aws:kms:us-west-2:444455556666:alias\/projectKey1@.
destinationToCreate_kmsKeyId :: Lens.Lens' DestinationToCreate (Prelude.Maybe Prelude.Text)
destinationToCreate_kmsKeyId = Lens.lens (\DestinationToCreate' {kmsKeyId} -> kmsKeyId) (\s@DestinationToCreate' {} a -> s {kmsKeyId = a} :: DestinationToCreate)

-- | To create a file system that uses Regional storage, specify the Amazon
-- Web Services Region in which to create the destination file system.
destinationToCreate_region :: Lens.Lens' DestinationToCreate (Prelude.Maybe Prelude.Text)
destinationToCreate_region = Lens.lens (\DestinationToCreate' {region} -> region) (\s@DestinationToCreate' {} a -> s {region = a} :: DestinationToCreate)

instance Prelude.Hashable DestinationToCreate where
  hashWithSalt _salt DestinationToCreate' {..} =
    _salt `Prelude.hashWithSalt` availabilityZoneName
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` region

instance Prelude.NFData DestinationToCreate where
  rnf DestinationToCreate' {..} =
    Prelude.rnf availabilityZoneName
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf region

instance Data.ToJSON DestinationToCreate where
  toJSON DestinationToCreate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AvailabilityZoneName" Data..=)
              Prelude.<$> availabilityZoneName,
            ("KmsKeyId" Data..=) Prelude.<$> kmsKeyId,
            ("Region" Data..=) Prelude.<$> region
          ]
      )
