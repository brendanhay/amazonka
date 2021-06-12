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
-- Module      : Network.AWS.EMR.Types.ClusterStateChangeReason
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.ClusterStateChangeReason where

import qualified Network.AWS.Core as Core
import Network.AWS.EMR.Types.ClusterStateChangeReasonCode
import qualified Network.AWS.Lens as Lens

-- | The reason that the cluster changed to its current state.
--
-- /See:/ 'newClusterStateChangeReason' smart constructor.
data ClusterStateChangeReason = ClusterStateChangeReason'
  { -- | The descriptive message for the state change reason.
    message :: Core.Maybe Core.Text,
    -- | The programmatic code for the state change reason.
    code :: Core.Maybe ClusterStateChangeReasonCode
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ClusterStateChangeReason' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'clusterStateChangeReason_message' - The descriptive message for the state change reason.
--
-- 'code', 'clusterStateChangeReason_code' - The programmatic code for the state change reason.
newClusterStateChangeReason ::
  ClusterStateChangeReason
newClusterStateChangeReason =
  ClusterStateChangeReason'
    { message = Core.Nothing,
      code = Core.Nothing
    }

-- | The descriptive message for the state change reason.
clusterStateChangeReason_message :: Lens.Lens' ClusterStateChangeReason (Core.Maybe Core.Text)
clusterStateChangeReason_message = Lens.lens (\ClusterStateChangeReason' {message} -> message) (\s@ClusterStateChangeReason' {} a -> s {message = a} :: ClusterStateChangeReason)

-- | The programmatic code for the state change reason.
clusterStateChangeReason_code :: Lens.Lens' ClusterStateChangeReason (Core.Maybe ClusterStateChangeReasonCode)
clusterStateChangeReason_code = Lens.lens (\ClusterStateChangeReason' {code} -> code) (\s@ClusterStateChangeReason' {} a -> s {code = a} :: ClusterStateChangeReason)

instance Core.FromJSON ClusterStateChangeReason where
  parseJSON =
    Core.withObject
      "ClusterStateChangeReason"
      ( \x ->
          ClusterStateChangeReason'
            Core.<$> (x Core..:? "Message") Core.<*> (x Core..:? "Code")
      )

instance Core.Hashable ClusterStateChangeReason

instance Core.NFData ClusterStateChangeReason
