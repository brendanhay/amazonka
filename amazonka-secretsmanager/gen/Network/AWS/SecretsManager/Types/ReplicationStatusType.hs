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
-- Module      : Network.AWS.SecretsManager.Types.ReplicationStatusType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SecretsManager.Types.ReplicationStatusType where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SecretsManager.Types.StatusType

-- | A replication object consisting of a @RegionReplicationStatus@ object
-- and includes a Region, KMSKeyId, status, and status message.
--
-- /See:/ 'newReplicationStatusType' smart constructor.
data ReplicationStatusType = ReplicationStatusType'
  { -- | Status message such as \"/Secret with this name already exists in this
    -- region/\".
    statusMessage :: Core.Maybe Core.Text,
    -- | The status can be @InProgress@, @Failed@, or @InSync@.
    status :: Core.Maybe StatusType,
    -- | Can be an @ARN@, @Key ID@, or @Alias@.
    kmsKeyId :: Core.Maybe Core.Text,
    -- | The date that you last accessed the secret in the Region.
    lastAccessedDate :: Core.Maybe Core.POSIX,
    -- | The Region where replication occurs.
    region :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ReplicationStatusType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'statusMessage', 'replicationStatusType_statusMessage' - Status message such as \"/Secret with this name already exists in this
-- region/\".
--
-- 'status', 'replicationStatusType_status' - The status can be @InProgress@, @Failed@, or @InSync@.
--
-- 'kmsKeyId', 'replicationStatusType_kmsKeyId' - Can be an @ARN@, @Key ID@, or @Alias@.
--
-- 'lastAccessedDate', 'replicationStatusType_lastAccessedDate' - The date that you last accessed the secret in the Region.
--
-- 'region', 'replicationStatusType_region' - The Region where replication occurs.
newReplicationStatusType ::
  ReplicationStatusType
newReplicationStatusType =
  ReplicationStatusType'
    { statusMessage =
        Core.Nothing,
      status = Core.Nothing,
      kmsKeyId = Core.Nothing,
      lastAccessedDate = Core.Nothing,
      region = Core.Nothing
    }

-- | Status message such as \"/Secret with this name already exists in this
-- region/\".
replicationStatusType_statusMessage :: Lens.Lens' ReplicationStatusType (Core.Maybe Core.Text)
replicationStatusType_statusMessage = Lens.lens (\ReplicationStatusType' {statusMessage} -> statusMessage) (\s@ReplicationStatusType' {} a -> s {statusMessage = a} :: ReplicationStatusType)

-- | The status can be @InProgress@, @Failed@, or @InSync@.
replicationStatusType_status :: Lens.Lens' ReplicationStatusType (Core.Maybe StatusType)
replicationStatusType_status = Lens.lens (\ReplicationStatusType' {status} -> status) (\s@ReplicationStatusType' {} a -> s {status = a} :: ReplicationStatusType)

-- | Can be an @ARN@, @Key ID@, or @Alias@.
replicationStatusType_kmsKeyId :: Lens.Lens' ReplicationStatusType (Core.Maybe Core.Text)
replicationStatusType_kmsKeyId = Lens.lens (\ReplicationStatusType' {kmsKeyId} -> kmsKeyId) (\s@ReplicationStatusType' {} a -> s {kmsKeyId = a} :: ReplicationStatusType)

-- | The date that you last accessed the secret in the Region.
replicationStatusType_lastAccessedDate :: Lens.Lens' ReplicationStatusType (Core.Maybe Core.UTCTime)
replicationStatusType_lastAccessedDate = Lens.lens (\ReplicationStatusType' {lastAccessedDate} -> lastAccessedDate) (\s@ReplicationStatusType' {} a -> s {lastAccessedDate = a} :: ReplicationStatusType) Core.. Lens.mapping Core._Time

-- | The Region where replication occurs.
replicationStatusType_region :: Lens.Lens' ReplicationStatusType (Core.Maybe Core.Text)
replicationStatusType_region = Lens.lens (\ReplicationStatusType' {region} -> region) (\s@ReplicationStatusType' {} a -> s {region = a} :: ReplicationStatusType)

instance Core.FromJSON ReplicationStatusType where
  parseJSON =
    Core.withObject
      "ReplicationStatusType"
      ( \x ->
          ReplicationStatusType'
            Core.<$> (x Core..:? "StatusMessage")
            Core.<*> (x Core..:? "Status")
            Core.<*> (x Core..:? "KmsKeyId")
            Core.<*> (x Core..:? "LastAccessedDate")
            Core.<*> (x Core..:? "Region")
      )

instance Core.Hashable ReplicationStatusType

instance Core.NFData ReplicationStatusType
