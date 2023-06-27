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
-- Module      : Amazonka.SecretsManager.Types.ReplicationStatusType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecretsManager.Types.ReplicationStatusType where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecretsManager.Types.StatusType

-- | A replication object consisting of a @RegionReplicationStatus@ object
-- and includes a Region, KMSKeyId, status, and status message.
--
-- /See:/ 'newReplicationStatusType' smart constructor.
data ReplicationStatusType = ReplicationStatusType'
  { -- | Can be an @ARN@, @Key ID@, or @Alias@.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The date that the secret was last accessed in the Region. This field is
    -- omitted if the secret has never been retrieved in the Region.
    lastAccessedDate :: Prelude.Maybe Data.POSIX,
    -- | The Region where replication occurs.
    region :: Prelude.Maybe Prelude.Text,
    -- | The status can be @InProgress@, @Failed@, or @InSync@.
    status :: Prelude.Maybe StatusType,
    -- | Status message such as \"/Secret with this name already exists in this
    -- region/\".
    statusMessage :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReplicationStatusType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsKeyId', 'replicationStatusType_kmsKeyId' - Can be an @ARN@, @Key ID@, or @Alias@.
--
-- 'lastAccessedDate', 'replicationStatusType_lastAccessedDate' - The date that the secret was last accessed in the Region. This field is
-- omitted if the secret has never been retrieved in the Region.
--
-- 'region', 'replicationStatusType_region' - The Region where replication occurs.
--
-- 'status', 'replicationStatusType_status' - The status can be @InProgress@, @Failed@, or @InSync@.
--
-- 'statusMessage', 'replicationStatusType_statusMessage' - Status message such as \"/Secret with this name already exists in this
-- region/\".
newReplicationStatusType ::
  ReplicationStatusType
newReplicationStatusType =
  ReplicationStatusType'
    { kmsKeyId = Prelude.Nothing,
      lastAccessedDate = Prelude.Nothing,
      region = Prelude.Nothing,
      status = Prelude.Nothing,
      statusMessage = Prelude.Nothing
    }

-- | Can be an @ARN@, @Key ID@, or @Alias@.
replicationStatusType_kmsKeyId :: Lens.Lens' ReplicationStatusType (Prelude.Maybe Prelude.Text)
replicationStatusType_kmsKeyId = Lens.lens (\ReplicationStatusType' {kmsKeyId} -> kmsKeyId) (\s@ReplicationStatusType' {} a -> s {kmsKeyId = a} :: ReplicationStatusType)

-- | The date that the secret was last accessed in the Region. This field is
-- omitted if the secret has never been retrieved in the Region.
replicationStatusType_lastAccessedDate :: Lens.Lens' ReplicationStatusType (Prelude.Maybe Prelude.UTCTime)
replicationStatusType_lastAccessedDate = Lens.lens (\ReplicationStatusType' {lastAccessedDate} -> lastAccessedDate) (\s@ReplicationStatusType' {} a -> s {lastAccessedDate = a} :: ReplicationStatusType) Prelude.. Lens.mapping Data._Time

-- | The Region where replication occurs.
replicationStatusType_region :: Lens.Lens' ReplicationStatusType (Prelude.Maybe Prelude.Text)
replicationStatusType_region = Lens.lens (\ReplicationStatusType' {region} -> region) (\s@ReplicationStatusType' {} a -> s {region = a} :: ReplicationStatusType)

-- | The status can be @InProgress@, @Failed@, or @InSync@.
replicationStatusType_status :: Lens.Lens' ReplicationStatusType (Prelude.Maybe StatusType)
replicationStatusType_status = Lens.lens (\ReplicationStatusType' {status} -> status) (\s@ReplicationStatusType' {} a -> s {status = a} :: ReplicationStatusType)

-- | Status message such as \"/Secret with this name already exists in this
-- region/\".
replicationStatusType_statusMessage :: Lens.Lens' ReplicationStatusType (Prelude.Maybe Prelude.Text)
replicationStatusType_statusMessage = Lens.lens (\ReplicationStatusType' {statusMessage} -> statusMessage) (\s@ReplicationStatusType' {} a -> s {statusMessage = a} :: ReplicationStatusType)

instance Data.FromJSON ReplicationStatusType where
  parseJSON =
    Data.withObject
      "ReplicationStatusType"
      ( \x ->
          ReplicationStatusType'
            Prelude.<$> (x Data..:? "KmsKeyId")
            Prelude.<*> (x Data..:? "LastAccessedDate")
            Prelude.<*> (x Data..:? "Region")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "StatusMessage")
      )

instance Prelude.Hashable ReplicationStatusType where
  hashWithSalt _salt ReplicationStatusType' {..} =
    _salt
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` lastAccessedDate
      `Prelude.hashWithSalt` region
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` statusMessage

instance Prelude.NFData ReplicationStatusType where
  rnf ReplicationStatusType' {..} =
    Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf lastAccessedDate
      `Prelude.seq` Prelude.rnf region
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf statusMessage
