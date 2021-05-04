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
-- Module      : Network.AWS.S3.Types.ReplicaModifications
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.ReplicaModifications where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.ReplicaModificationsStatus

-- | A filter that you can specify for selection for modifications on
-- replicas. Amazon S3 doesn\'t replicate replica modifications by default.
-- In the latest version of replication configuration (when @Filter@ is
-- specified), you can specify this element and set the status to @Enabled@
-- to replicate modifications on replicas.
--
-- If you don\'t specify the @Filter@ element, Amazon S3 assumes that the
-- replication configuration is the earlier version, V1. In the earlier
-- version, this element is not allowed.
--
-- /See:/ 'newReplicaModifications' smart constructor.
data ReplicaModifications = ReplicaModifications'
  { -- | Specifies whether Amazon S3 replicates modifications on replicas.
    status :: ReplicaModificationsStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ReplicaModifications' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'replicaModifications_status' - Specifies whether Amazon S3 replicates modifications on replicas.
newReplicaModifications ::
  -- | 'status'
  ReplicaModificationsStatus ->
  ReplicaModifications
newReplicaModifications pStatus_ =
  ReplicaModifications' {status = pStatus_}

-- | Specifies whether Amazon S3 replicates modifications on replicas.
replicaModifications_status :: Lens.Lens' ReplicaModifications ReplicaModificationsStatus
replicaModifications_status = Lens.lens (\ReplicaModifications' {status} -> status) (\s@ReplicaModifications' {} a -> s {status = a} :: ReplicaModifications)

instance Prelude.FromXML ReplicaModifications where
  parseXML x =
    ReplicaModifications'
      Prelude.<$> (x Prelude..@ "Status")

instance Prelude.Hashable ReplicaModifications

instance Prelude.NFData ReplicaModifications

instance Prelude.ToXML ReplicaModifications where
  toXML ReplicaModifications' {..} =
    Prelude.mconcat ["Status" Prelude.@= status]
