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
-- Module      : Network.AWS.S3.Types.DeleteMarkerReplication
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.DeleteMarkerReplication where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.DeleteMarkerReplicationStatus

-- | Specifies whether Amazon S3 replicates delete markers. If you specify a
-- @Filter@ in your replication configuration, you must also include a
-- @DeleteMarkerReplication@ element. If your @Filter@ includes a @Tag@
-- element, the @DeleteMarkerReplication@ @Status@ must be set to Disabled,
-- because Amazon S3 does not support replicating delete markers for
-- tag-based rules. For an example configuration, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/replication-add-config.html#replication-config-min-rule-config Basic Rule Configuration>.
--
-- For more information about delete marker replication, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/delete-marker-replication.html Basic Rule Configuration>.
--
-- If you are using an earlier version of the replication configuration,
-- Amazon S3 handles replication of delete markers differently. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/replication-add-config.html#replication-backward-compat-considerations Backward Compatibility>.
--
-- /See:/ 'newDeleteMarkerReplication' smart constructor.
data DeleteMarkerReplication = DeleteMarkerReplication'
  { -- | Indicates whether to replicate delete markers.
    --
    -- Indicates whether to replicate delete markers.
    status :: Prelude.Maybe DeleteMarkerReplicationStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteMarkerReplication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'deleteMarkerReplication_status' - Indicates whether to replicate delete markers.
--
-- Indicates whether to replicate delete markers.
newDeleteMarkerReplication ::
  DeleteMarkerReplication
newDeleteMarkerReplication =
  DeleteMarkerReplication' {status = Prelude.Nothing}

-- | Indicates whether to replicate delete markers.
--
-- Indicates whether to replicate delete markers.
deleteMarkerReplication_status :: Lens.Lens' DeleteMarkerReplication (Prelude.Maybe DeleteMarkerReplicationStatus)
deleteMarkerReplication_status = Lens.lens (\DeleteMarkerReplication' {status} -> status) (\s@DeleteMarkerReplication' {} a -> s {status = a} :: DeleteMarkerReplication)

instance Prelude.FromXML DeleteMarkerReplication where
  parseXML x =
    DeleteMarkerReplication'
      Prelude.<$> (x Prelude..@? "Status")

instance Prelude.Hashable DeleteMarkerReplication

instance Prelude.NFData DeleteMarkerReplication

instance Prelude.ToXML DeleteMarkerReplication where
  toXML DeleteMarkerReplication' {..} =
    Prelude.mconcat ["Status" Prelude.@= status]
