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
-- Module      : Network.AWS.Redshift.Types.PauseClusterMessage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.PauseClusterMessage where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Redshift.Internal

-- | Describes a pause cluster operation. For example, a scheduled action to
-- run the @PauseCluster@ API operation.
--
-- /See:/ 'newPauseClusterMessage' smart constructor.
data PauseClusterMessage = PauseClusterMessage'
  { -- | The identifier of the cluster to be paused.
    clusterIdentifier :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PauseClusterMessage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterIdentifier', 'pauseClusterMessage_clusterIdentifier' - The identifier of the cluster to be paused.
newPauseClusterMessage ::
  -- | 'clusterIdentifier'
  Core.Text ->
  PauseClusterMessage
newPauseClusterMessage pClusterIdentifier_ =
  PauseClusterMessage'
    { clusterIdentifier =
        pClusterIdentifier_
    }

-- | The identifier of the cluster to be paused.
pauseClusterMessage_clusterIdentifier :: Lens.Lens' PauseClusterMessage Core.Text
pauseClusterMessage_clusterIdentifier = Lens.lens (\PauseClusterMessage' {clusterIdentifier} -> clusterIdentifier) (\s@PauseClusterMessage' {} a -> s {clusterIdentifier = a} :: PauseClusterMessage)

instance Core.FromXML PauseClusterMessage where
  parseXML x =
    PauseClusterMessage'
      Core.<$> (x Core..@ "ClusterIdentifier")

instance Core.Hashable PauseClusterMessage

instance Core.NFData PauseClusterMessage

instance Core.ToQuery PauseClusterMessage where
  toQuery PauseClusterMessage' {..} =
    Core.mconcat
      ["ClusterIdentifier" Core.=: clusterIdentifier]
