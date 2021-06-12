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
-- Module      : Network.AWS.SecretsManager.Types.ReplicaRegionType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SecretsManager.Types.ReplicaRegionType where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | (Optional) Custom type consisting of a @Region@ (required) and the
-- @KmsKeyId@ which can be an @ARN@, @Key ID@, or @Alias@.
--
-- /See:/ 'newReplicaRegionType' smart constructor.
data ReplicaRegionType = ReplicaRegionType'
  { -- | Can be an @ARN@, @Key ID@, or @Alias@.
    kmsKeyId :: Core.Maybe Core.Text,
    -- | Describes a single instance of Region objects.
    region :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { kmsKeyId = Core.Nothing,
      region = Core.Nothing
    }

-- | Can be an @ARN@, @Key ID@, or @Alias@.
replicaRegionType_kmsKeyId :: Lens.Lens' ReplicaRegionType (Core.Maybe Core.Text)
replicaRegionType_kmsKeyId = Lens.lens (\ReplicaRegionType' {kmsKeyId} -> kmsKeyId) (\s@ReplicaRegionType' {} a -> s {kmsKeyId = a} :: ReplicaRegionType)

-- | Describes a single instance of Region objects.
replicaRegionType_region :: Lens.Lens' ReplicaRegionType (Core.Maybe Core.Text)
replicaRegionType_region = Lens.lens (\ReplicaRegionType' {region} -> region) (\s@ReplicaRegionType' {} a -> s {region = a} :: ReplicaRegionType)

instance Core.Hashable ReplicaRegionType

instance Core.NFData ReplicaRegionType

instance Core.ToJSON ReplicaRegionType where
  toJSON ReplicaRegionType' {..} =
    Core.object
      ( Core.catMaybes
          [ ("KmsKeyId" Core..=) Core.<$> kmsKeyId,
            ("Region" Core..=) Core.<$> region
          ]
      )
