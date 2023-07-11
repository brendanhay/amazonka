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
-- Module      : Amazonka.DocDbElastic.Types.ClusterInList
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DocDbElastic.Types.ClusterInList where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DocDbElastic.Types.Status
import qualified Amazonka.Prelude as Prelude

-- | A list of Elastic DocumentDB cluster.
--
-- /See:/ 'newClusterInList' smart constructor.
data ClusterInList = ClusterInList'
  { -- | The arn of the Elastic DocumentDB cluster.
    clusterArn :: Prelude.Text,
    -- | The name of the Elastic DocumentDB cluster.
    clusterName :: Prelude.Text,
    -- | The status of the Elastic DocumentDB cluster.
    status :: Status
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ClusterInList' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterArn', 'clusterInList_clusterArn' - The arn of the Elastic DocumentDB cluster.
--
-- 'clusterName', 'clusterInList_clusterName' - The name of the Elastic DocumentDB cluster.
--
-- 'status', 'clusterInList_status' - The status of the Elastic DocumentDB cluster.
newClusterInList ::
  -- | 'clusterArn'
  Prelude.Text ->
  -- | 'clusterName'
  Prelude.Text ->
  -- | 'status'
  Status ->
  ClusterInList
newClusterInList pClusterArn_ pClusterName_ pStatus_ =
  ClusterInList'
    { clusterArn = pClusterArn_,
      clusterName = pClusterName_,
      status = pStatus_
    }

-- | The arn of the Elastic DocumentDB cluster.
clusterInList_clusterArn :: Lens.Lens' ClusterInList Prelude.Text
clusterInList_clusterArn = Lens.lens (\ClusterInList' {clusterArn} -> clusterArn) (\s@ClusterInList' {} a -> s {clusterArn = a} :: ClusterInList)

-- | The name of the Elastic DocumentDB cluster.
clusterInList_clusterName :: Lens.Lens' ClusterInList Prelude.Text
clusterInList_clusterName = Lens.lens (\ClusterInList' {clusterName} -> clusterName) (\s@ClusterInList' {} a -> s {clusterName = a} :: ClusterInList)

-- | The status of the Elastic DocumentDB cluster.
clusterInList_status :: Lens.Lens' ClusterInList Status
clusterInList_status = Lens.lens (\ClusterInList' {status} -> status) (\s@ClusterInList' {} a -> s {status = a} :: ClusterInList)

instance Data.FromJSON ClusterInList where
  parseJSON =
    Data.withObject
      "ClusterInList"
      ( \x ->
          ClusterInList'
            Prelude.<$> (x Data..: "clusterArn")
            Prelude.<*> (x Data..: "clusterName")
            Prelude.<*> (x Data..: "status")
      )

instance Prelude.Hashable ClusterInList where
  hashWithSalt _salt ClusterInList' {..} =
    _salt
      `Prelude.hashWithSalt` clusterArn
      `Prelude.hashWithSalt` clusterName
      `Prelude.hashWithSalt` status

instance Prelude.NFData ClusterInList where
  rnf ClusterInList' {..} =
    Prelude.rnf clusterArn
      `Prelude.seq` Prelude.rnf clusterName
      `Prelude.seq` Prelude.rnf status
