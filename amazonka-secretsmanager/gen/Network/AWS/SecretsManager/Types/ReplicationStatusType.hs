{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SecretsManager.Types.StatusType

-- | A replication object consisting of a @RegionReplicationStatus@ object
-- and includes a Region, KMSKeyId, status, and status message.
--
-- /See:/ 'newReplicationStatusType' smart constructor.
data ReplicationStatusType = ReplicationStatusType'
  { -- | Status message such as \"/Secret with this name already exists in this
    -- region/\".
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The status can be @InProgress@, @Failed@, or @InSync@.
    status :: Prelude.Maybe StatusType,
    -- | Can be an @ARN@, @Key ID@, or @Alias@.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The date that you last accessed the secret in the Region.
    lastAccessedDate :: Prelude.Maybe Prelude.POSIX,
    -- | The Region where replication occurs.
    region :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      status = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      lastAccessedDate = Prelude.Nothing,
      region = Prelude.Nothing
    }

-- | Status message such as \"/Secret with this name already exists in this
-- region/\".
replicationStatusType_statusMessage :: Lens.Lens' ReplicationStatusType (Prelude.Maybe Prelude.Text)
replicationStatusType_statusMessage = Lens.lens (\ReplicationStatusType' {statusMessage} -> statusMessage) (\s@ReplicationStatusType' {} a -> s {statusMessage = a} :: ReplicationStatusType)

-- | The status can be @InProgress@, @Failed@, or @InSync@.
replicationStatusType_status :: Lens.Lens' ReplicationStatusType (Prelude.Maybe StatusType)
replicationStatusType_status = Lens.lens (\ReplicationStatusType' {status} -> status) (\s@ReplicationStatusType' {} a -> s {status = a} :: ReplicationStatusType)

-- | Can be an @ARN@, @Key ID@, or @Alias@.
replicationStatusType_kmsKeyId :: Lens.Lens' ReplicationStatusType (Prelude.Maybe Prelude.Text)
replicationStatusType_kmsKeyId = Lens.lens (\ReplicationStatusType' {kmsKeyId} -> kmsKeyId) (\s@ReplicationStatusType' {} a -> s {kmsKeyId = a} :: ReplicationStatusType)

-- | The date that you last accessed the secret in the Region.
replicationStatusType_lastAccessedDate :: Lens.Lens' ReplicationStatusType (Prelude.Maybe Prelude.UTCTime)
replicationStatusType_lastAccessedDate = Lens.lens (\ReplicationStatusType' {lastAccessedDate} -> lastAccessedDate) (\s@ReplicationStatusType' {} a -> s {lastAccessedDate = a} :: ReplicationStatusType) Prelude.. Lens.mapping Prelude._Time

-- | The Region where replication occurs.
replicationStatusType_region :: Lens.Lens' ReplicationStatusType (Prelude.Maybe Prelude.Text)
replicationStatusType_region = Lens.lens (\ReplicationStatusType' {region} -> region) (\s@ReplicationStatusType' {} a -> s {region = a} :: ReplicationStatusType)

instance Prelude.FromJSON ReplicationStatusType where
  parseJSON =
    Prelude.withObject
      "ReplicationStatusType"
      ( \x ->
          ReplicationStatusType'
            Prelude.<$> (x Prelude..:? "StatusMessage")
            Prelude.<*> (x Prelude..:? "Status")
            Prelude.<*> (x Prelude..:? "KmsKeyId")
            Prelude.<*> (x Prelude..:? "LastAccessedDate")
            Prelude.<*> (x Prelude..:? "Region")
      )

instance Prelude.Hashable ReplicationStatusType

instance Prelude.NFData ReplicationStatusType
