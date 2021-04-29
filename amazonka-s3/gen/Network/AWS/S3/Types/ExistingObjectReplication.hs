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
-- Module      : Network.AWS.S3.Types.ExistingObjectReplication
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.ExistingObjectReplication where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.ExistingObjectReplicationStatus

-- | Optional configuration to replicate existing source bucket objects. For
-- more information, see
-- <%20https://docs.aws.amazon.com/AmazonS3/latest/dev/replication-what-is-isnot-replicated.html#existing-object-replication Replicating Existing Objects>
-- in the /Amazon S3 Developer Guide/.
--
-- /See:/ 'newExistingObjectReplication' smart constructor.
data ExistingObjectReplication = ExistingObjectReplication'
  { status :: ExistingObjectReplicationStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ExistingObjectReplication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'existingObjectReplication_status' -
newExistingObjectReplication ::
  -- | 'status'
  ExistingObjectReplicationStatus ->
  ExistingObjectReplication
newExistingObjectReplication pStatus_ =
  ExistingObjectReplication' {status = pStatus_}

-- |
existingObjectReplication_status :: Lens.Lens' ExistingObjectReplication ExistingObjectReplicationStatus
existingObjectReplication_status = Lens.lens (\ExistingObjectReplication' {status} -> status) (\s@ExistingObjectReplication' {} a -> s {status = a} :: ExistingObjectReplication)

instance Prelude.FromXML ExistingObjectReplication where
  parseXML x =
    ExistingObjectReplication'
      Prelude.<$> (x Prelude..@ "Status")

instance Prelude.Hashable ExistingObjectReplication

instance Prelude.NFData ExistingObjectReplication

instance Prelude.ToXML ExistingObjectReplication where
  toXML ExistingObjectReplication' {..} =
    Prelude.mconcat ["Status" Prelude.@= status]
