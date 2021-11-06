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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.ClusterConfigStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.OpenSearch.Types.ClusterConfig
import Amazonka.OpenSearch.Types.OptionStatus
import qualified Amazonka.Prelude as Prelude

-- | The configuration status for the specified domain.
--
-- /See:/ 'newClusterConfigStatus' smart constructor.
data ClusterConfigStatus = ClusterConfigStatus'
  { -- | The cluster configuration for the specified domain.
    options :: ClusterConfig,
    -- | The cluster configuration status for the specified domain.
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
-- 'options', 'clusterConfigStatus_options' - The cluster configuration for the specified domain.
--
-- 'status', 'clusterConfigStatus_status' - The cluster configuration status for the specified domain.
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

-- | The cluster configuration for the specified domain.
clusterConfigStatus_options :: Lens.Lens' ClusterConfigStatus ClusterConfig
clusterConfigStatus_options = Lens.lens (\ClusterConfigStatus' {options} -> options) (\s@ClusterConfigStatus' {} a -> s {options = a} :: ClusterConfigStatus)

-- | The cluster configuration status for the specified domain.
clusterConfigStatus_status :: Lens.Lens' ClusterConfigStatus OptionStatus
clusterConfigStatus_status = Lens.lens (\ClusterConfigStatus' {status} -> status) (\s@ClusterConfigStatus' {} a -> s {status = a} :: ClusterConfigStatus)

instance Core.FromJSON ClusterConfigStatus where
  parseJSON =
    Core.withObject
      "ClusterConfigStatus"
      ( \x ->
          ClusterConfigStatus'
            Prelude.<$> (x Core..: "Options")
            Prelude.<*> (x Core..: "Status")
      )

instance Prelude.Hashable ClusterConfigStatus

instance Prelude.NFData ClusterConfigStatus
