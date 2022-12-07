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
-- Module      : Amazonka.S3.Types.ReplicaModifications
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.ReplicaModifications where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal
import Amazonka.S3.Types.ReplicaModificationsStatus

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Data.FromXML ReplicaModifications where
  parseXML x =
    ReplicaModifications'
      Prelude.<$> (x Data..@ "Status")

instance Prelude.Hashable ReplicaModifications where
  hashWithSalt _salt ReplicaModifications' {..} =
    _salt `Prelude.hashWithSalt` status

instance Prelude.NFData ReplicaModifications where
  rnf ReplicaModifications' {..} = Prelude.rnf status

instance Data.ToXML ReplicaModifications where
  toXML ReplicaModifications' {..} =
    Prelude.mconcat ["Status" Data.@= status]
