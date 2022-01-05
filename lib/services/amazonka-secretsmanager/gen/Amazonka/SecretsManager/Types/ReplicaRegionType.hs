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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecretsManager.Types.ReplicaRegionType where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | (Optional) Custom type consisting of a @Region@ (required) and the
-- @KmsKeyId@ which can be an @ARN@, @Key ID@, or @Alias@.
--
-- /See:/ 'newReplicaRegionType' smart constructor.
data ReplicaRegionType = ReplicaRegionType'
  { -- | Can be an @ARN@, @Key ID@, or @Alias@.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | Describes a single instance of Region objects.
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
-- 'kmsKeyId', 'replicaRegionType_kmsKeyId' - Can be an @ARN@, @Key ID@, or @Alias@.
--
-- 'region', 'replicaRegionType_region' - Describes a single instance of Region objects.
newReplicaRegionType ::
  ReplicaRegionType
newReplicaRegionType =
  ReplicaRegionType'
    { kmsKeyId = Prelude.Nothing,
      region = Prelude.Nothing
    }

-- | Can be an @ARN@, @Key ID@, or @Alias@.
replicaRegionType_kmsKeyId :: Lens.Lens' ReplicaRegionType (Prelude.Maybe Prelude.Text)
replicaRegionType_kmsKeyId = Lens.lens (\ReplicaRegionType' {kmsKeyId} -> kmsKeyId) (\s@ReplicaRegionType' {} a -> s {kmsKeyId = a} :: ReplicaRegionType)

-- | Describes a single instance of Region objects.
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

instance Core.ToJSON ReplicaRegionType where
  toJSON ReplicaRegionType' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("KmsKeyId" Core..=) Prelude.<$> kmsKeyId,
            ("Region" Core..=) Prelude.<$> region
          ]
      )
