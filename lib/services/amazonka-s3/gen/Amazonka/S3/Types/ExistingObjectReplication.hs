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
-- Module      : Amazonka.S3.Types.ExistingObjectReplication
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.ExistingObjectReplication where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal
import Amazonka.S3.Types.ExistingObjectReplicationStatus

-- | Optional configuration to replicate existing source bucket objects. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/replication-what-is-isnot-replicated.html#existing-object-replication Replicating Existing Objects>
-- in the /Amazon S3 User Guide/.
--
-- /See:/ 'newExistingObjectReplication' smart constructor.
data ExistingObjectReplication = ExistingObjectReplication'
  { -- | Specifies whether Amazon S3 replicates existing source bucket objects.
    status :: ExistingObjectReplicationStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExistingObjectReplication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'existingObjectReplication_status' - Specifies whether Amazon S3 replicates existing source bucket objects.
newExistingObjectReplication ::
  -- | 'status'
  ExistingObjectReplicationStatus ->
  ExistingObjectReplication
newExistingObjectReplication pStatus_ =
  ExistingObjectReplication' {status = pStatus_}

-- | Specifies whether Amazon S3 replicates existing source bucket objects.
existingObjectReplication_status :: Lens.Lens' ExistingObjectReplication ExistingObjectReplicationStatus
existingObjectReplication_status = Lens.lens (\ExistingObjectReplication' {status} -> status) (\s@ExistingObjectReplication' {} a -> s {status = a} :: ExistingObjectReplication)

instance Data.FromXML ExistingObjectReplication where
  parseXML x =
    ExistingObjectReplication'
      Prelude.<$> (x Data..@ "Status")

instance Prelude.Hashable ExistingObjectReplication where
  hashWithSalt _salt ExistingObjectReplication' {..} =
    _salt `Prelude.hashWithSalt` status

instance Prelude.NFData ExistingObjectReplication where
  rnf ExistingObjectReplication' {..} =
    Prelude.rnf status

instance Data.ToXML ExistingObjectReplication where
  toXML ExistingObjectReplication' {..} =
    Prelude.mconcat ["Status" Data.@= status]
