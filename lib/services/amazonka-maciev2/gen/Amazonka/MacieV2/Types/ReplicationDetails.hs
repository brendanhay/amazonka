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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.ReplicationDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
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
    -- | Specifies whether the bucket is configured to replicate one or more
    -- objects to an Amazon Web Services account that isn\'t part of the same
    -- Amazon Macie organization.
    replicatedExternally :: Prelude.Maybe Prelude.Bool,
    -- | An array of Amazon Web Services account IDs, one for each Amazon Web
    -- Services account that the bucket is configured to replicate one or more
    -- objects to.
    replicationAccounts :: Prelude.Maybe [Prelude.Text]
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
-- 'replicatedExternally', 'replicationDetails_replicatedExternally' - Specifies whether the bucket is configured to replicate one or more
-- objects to an Amazon Web Services account that isn\'t part of the same
-- Amazon Macie organization.
--
-- 'replicationAccounts', 'replicationDetails_replicationAccounts' - An array of Amazon Web Services account IDs, one for each Amazon Web
-- Services account that the bucket is configured to replicate one or more
-- objects to.
newReplicationDetails ::
  ReplicationDetails
newReplicationDetails =
  ReplicationDetails'
    { replicated = Prelude.Nothing,
      replicatedExternally = Prelude.Nothing,
      replicationAccounts = Prelude.Nothing
    }

-- | Specifies whether the bucket is configured to replicate one or more
-- objects to any destination.
replicationDetails_replicated :: Lens.Lens' ReplicationDetails (Prelude.Maybe Prelude.Bool)
replicationDetails_replicated = Lens.lens (\ReplicationDetails' {replicated} -> replicated) (\s@ReplicationDetails' {} a -> s {replicated = a} :: ReplicationDetails)

-- | Specifies whether the bucket is configured to replicate one or more
-- objects to an Amazon Web Services account that isn\'t part of the same
-- Amazon Macie organization.
replicationDetails_replicatedExternally :: Lens.Lens' ReplicationDetails (Prelude.Maybe Prelude.Bool)
replicationDetails_replicatedExternally = Lens.lens (\ReplicationDetails' {replicatedExternally} -> replicatedExternally) (\s@ReplicationDetails' {} a -> s {replicatedExternally = a} :: ReplicationDetails)

-- | An array of Amazon Web Services account IDs, one for each Amazon Web
-- Services account that the bucket is configured to replicate one or more
-- objects to.
replicationDetails_replicationAccounts :: Lens.Lens' ReplicationDetails (Prelude.Maybe [Prelude.Text])
replicationDetails_replicationAccounts = Lens.lens (\ReplicationDetails' {replicationAccounts} -> replicationAccounts) (\s@ReplicationDetails' {} a -> s {replicationAccounts = a} :: ReplicationDetails) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ReplicationDetails where
  parseJSON =
    Data.withObject
      "ReplicationDetails"
      ( \x ->
          ReplicationDetails'
            Prelude.<$> (x Data..:? "replicated")
            Prelude.<*> (x Data..:? "replicatedExternally")
            Prelude.<*> ( x
                            Data..:? "replicationAccounts"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable ReplicationDetails where
  hashWithSalt _salt ReplicationDetails' {..} =
    _salt
      `Prelude.hashWithSalt` replicated
      `Prelude.hashWithSalt` replicatedExternally
      `Prelude.hashWithSalt` replicationAccounts

instance Prelude.NFData ReplicationDetails where
  rnf ReplicationDetails' {..} =
    Prelude.rnf replicated
      `Prelude.seq` Prelude.rnf replicatedExternally
      `Prelude.seq` Prelude.rnf replicationAccounts
