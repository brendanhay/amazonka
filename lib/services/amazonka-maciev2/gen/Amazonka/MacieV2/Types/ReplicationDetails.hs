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
-- Module      : Amazonka.MacieV2.Types.ReplicationDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.ReplicationDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides information about settings that define whether one or more
-- objects in an S3 bucket are replicated to S3 buckets for other Amazon
-- Web Services accounts and, if so, which accounts.
--
-- /See:/ 'newReplicationDetails' smart constructor.
data ReplicationDetails = ReplicationDetails'
  { -- | Specifies whether the bucket is configured to replicate one or more
    -- objects to any destination.
    replicated :: Prelude.Maybe Prelude.Bool,
    -- | An array of Amazon Web Services account IDs, one for each Amazon Web
    -- Services account that the bucket is configured to replicate one or more
    -- objects to.
    replicationAccounts :: Prelude.Maybe [Prelude.Text],
    -- | Specifies whether the bucket is configured to replicate one or more
    -- objects to an Amazon Web Services account that isn\'t part of the same
    -- Amazon Macie organization.
    replicatedExternally :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReplicationDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replicated', 'replicationDetails_replicated' - Specifies whether the bucket is configured to replicate one or more
-- objects to any destination.
--
-- 'replicationAccounts', 'replicationDetails_replicationAccounts' - An array of Amazon Web Services account IDs, one for each Amazon Web
-- Services account that the bucket is configured to replicate one or more
-- objects to.
--
-- 'replicatedExternally', 'replicationDetails_replicatedExternally' - Specifies whether the bucket is configured to replicate one or more
-- objects to an Amazon Web Services account that isn\'t part of the same
-- Amazon Macie organization.
newReplicationDetails ::
  ReplicationDetails
newReplicationDetails =
  ReplicationDetails'
    { replicated = Prelude.Nothing,
      replicationAccounts = Prelude.Nothing,
      replicatedExternally = Prelude.Nothing
    }

-- | Specifies whether the bucket is configured to replicate one or more
-- objects to any destination.
replicationDetails_replicated :: Lens.Lens' ReplicationDetails (Prelude.Maybe Prelude.Bool)
replicationDetails_replicated = Lens.lens (\ReplicationDetails' {replicated} -> replicated) (\s@ReplicationDetails' {} a -> s {replicated = a} :: ReplicationDetails)

-- | An array of Amazon Web Services account IDs, one for each Amazon Web
-- Services account that the bucket is configured to replicate one or more
-- objects to.
replicationDetails_replicationAccounts :: Lens.Lens' ReplicationDetails (Prelude.Maybe [Prelude.Text])
replicationDetails_replicationAccounts = Lens.lens (\ReplicationDetails' {replicationAccounts} -> replicationAccounts) (\s@ReplicationDetails' {} a -> s {replicationAccounts = a} :: ReplicationDetails) Prelude.. Lens.mapping Lens.coerced

-- | Specifies whether the bucket is configured to replicate one or more
-- objects to an Amazon Web Services account that isn\'t part of the same
-- Amazon Macie organization.
replicationDetails_replicatedExternally :: Lens.Lens' ReplicationDetails (Prelude.Maybe Prelude.Bool)
replicationDetails_replicatedExternally = Lens.lens (\ReplicationDetails' {replicatedExternally} -> replicatedExternally) (\s@ReplicationDetails' {} a -> s {replicatedExternally = a} :: ReplicationDetails)

instance Core.FromJSON ReplicationDetails where
  parseJSON =
    Core.withObject
      "ReplicationDetails"
      ( \x ->
          ReplicationDetails'
            Prelude.<$> (x Core..:? "replicated")
            Prelude.<*> ( x Core..:? "replicationAccounts"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "replicatedExternally")
      )

instance Prelude.Hashable ReplicationDetails

instance Prelude.NFData ReplicationDetails
