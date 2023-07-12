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
-- Module      : Amazonka.OpenSearch.Types.ClusterConfigStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.ClusterConfigStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearch.Types.ClusterConfig
import Amazonka.OpenSearch.Types.OptionStatus
import qualified Amazonka.Prelude as Prelude

-- | The cluster configuration status for a domain.
--
-- /See:/ 'newClusterConfigStatus' smart constructor.
data ClusterConfigStatus = ClusterConfigStatus'
  { -- | Cluster configuration options for the specified domain.
    options :: ClusterConfig,
    -- | The status of cluster configuration options for the specified domain.
    status :: OptionStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ClusterConfigStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'options', 'clusterConfigStatus_options' - Cluster configuration options for the specified domain.
--
-- 'status', 'clusterConfigStatus_status' - The status of cluster configuration options for the specified domain.
newClusterConfigStatus ::
  -- | 'options'
  ClusterConfig ->
  -- | 'status'
  OptionStatus ->
  ClusterConfigStatus
newClusterConfigStatus pOptions_ pStatus_ =
  ClusterConfigStatus'
    { options = pOptions_,
      status = pStatus_
    }

-- | Cluster configuration options for the specified domain.
clusterConfigStatus_options :: Lens.Lens' ClusterConfigStatus ClusterConfig
clusterConfigStatus_options = Lens.lens (\ClusterConfigStatus' {options} -> options) (\s@ClusterConfigStatus' {} a -> s {options = a} :: ClusterConfigStatus)

-- | The status of cluster configuration options for the specified domain.
clusterConfigStatus_status :: Lens.Lens' ClusterConfigStatus OptionStatus
clusterConfigStatus_status = Lens.lens (\ClusterConfigStatus' {status} -> status) (\s@ClusterConfigStatus' {} a -> s {status = a} :: ClusterConfigStatus)

instance Data.FromJSON ClusterConfigStatus where
  parseJSON =
    Data.withObject
      "ClusterConfigStatus"
      ( \x ->
          ClusterConfigStatus'
            Prelude.<$> (x Data..: "Options")
            Prelude.<*> (x Data..: "Status")
      )

instance Prelude.Hashable ClusterConfigStatus where
  hashWithSalt _salt ClusterConfigStatus' {..} =
    _salt
      `Prelude.hashWithSalt` options
      `Prelude.hashWithSalt` status

instance Prelude.NFData ClusterConfigStatus where
  rnf ClusterConfigStatus' {..} =
    Prelude.rnf options
      `Prelude.seq` Prelude.rnf status
