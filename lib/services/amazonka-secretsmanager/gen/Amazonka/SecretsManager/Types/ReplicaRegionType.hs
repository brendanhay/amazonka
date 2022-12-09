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
-- Module      : Amazonka.SecretsManager.Types.ReplicaRegionType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecretsManager.Types.ReplicaRegionType where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A custom type that specifies a @Region@ and the @KmsKeyId@ for a replica
-- secret.
--
-- /See:/ 'newReplicaRegionType' smart constructor.
data ReplicaRegionType = ReplicaRegionType'
  { -- | The ARN, key ID, or alias of the KMS key to encrypt the secret. If you
    -- don\'t include this field, Secrets Manager uses @aws\/secretsmanager@.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | A Region code. For a list of Region codes, see
    -- <https://docs.aws.amazon.com/general/latest/gr/rande.html#regional-endpoints Name and code of Regions>.
    region :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReplicaRegionType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsKeyId', 'replicaRegionType_kmsKeyId' - The ARN, key ID, or alias of the KMS key to encrypt the secret. If you
-- don\'t include this field, Secrets Manager uses @aws\/secretsmanager@.
--
-- 'region', 'replicaRegionType_region' - A Region code. For a list of Region codes, see
-- <https://docs.aws.amazon.com/general/latest/gr/rande.html#regional-endpoints Name and code of Regions>.
newReplicaRegionType ::
  ReplicaRegionType
newReplicaRegionType =
  ReplicaRegionType'
    { kmsKeyId = Prelude.Nothing,
      region = Prelude.Nothing
    }

-- | The ARN, key ID, or alias of the KMS key to encrypt the secret. If you
-- don\'t include this field, Secrets Manager uses @aws\/secretsmanager@.
replicaRegionType_kmsKeyId :: Lens.Lens' ReplicaRegionType (Prelude.Maybe Prelude.Text)
replicaRegionType_kmsKeyId = Lens.lens (\ReplicaRegionType' {kmsKeyId} -> kmsKeyId) (\s@ReplicaRegionType' {} a -> s {kmsKeyId = a} :: ReplicaRegionType)

-- | A Region code. For a list of Region codes, see
-- <https://docs.aws.amazon.com/general/latest/gr/rande.html#regional-endpoints Name and code of Regions>.
replicaRegionType_region :: Lens.Lens' ReplicaRegionType (Prelude.Maybe Prelude.Text)
replicaRegionType_region = Lens.lens (\ReplicaRegionType' {region} -> region) (\s@ReplicaRegionType' {} a -> s {region = a} :: ReplicaRegionType)

instance Prelude.Hashable ReplicaRegionType where
  hashWithSalt _salt ReplicaRegionType' {..} =
    _salt `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` region

instance Prelude.NFData ReplicaRegionType where
  rnf ReplicaRegionType' {..} =
    Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf region

instance Data.ToJSON ReplicaRegionType where
  toJSON ReplicaRegionType' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("KmsKeyId" Data..=) Prelude.<$> kmsKeyId,
            ("Region" Data..=) Prelude.<$> region
          ]
      )
