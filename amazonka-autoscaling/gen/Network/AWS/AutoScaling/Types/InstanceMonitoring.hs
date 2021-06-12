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
-- Module      : Network.AWS.AutoScaling.Types.InstanceMonitoring
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.InstanceMonitoring where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes whether detailed monitoring is enabled for the Auto Scaling
-- instances.
--
-- /See:/ 'newInstanceMonitoring' smart constructor.
data InstanceMonitoring = InstanceMonitoring'
  { -- | If @true@, detailed monitoring is enabled. Otherwise, basic monitoring
    -- is enabled.
    enabled :: Core.Maybe Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'InstanceMonitoring' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'instanceMonitoring_enabled' - If @true@, detailed monitoring is enabled. Otherwise, basic monitoring
-- is enabled.
newInstanceMonitoring ::
  InstanceMonitoring
newInstanceMonitoring =
  InstanceMonitoring' {enabled = Core.Nothing}

-- | If @true@, detailed monitoring is enabled. Otherwise, basic monitoring
-- is enabled.
instanceMonitoring_enabled :: Lens.Lens' InstanceMonitoring (Core.Maybe Core.Bool)
instanceMonitoring_enabled = Lens.lens (\InstanceMonitoring' {enabled} -> enabled) (\s@InstanceMonitoring' {} a -> s {enabled = a} :: InstanceMonitoring)

instance Core.FromXML InstanceMonitoring where
  parseXML x =
    InstanceMonitoring' Core.<$> (x Core..@? "Enabled")

instance Core.Hashable InstanceMonitoring

instance Core.NFData InstanceMonitoring

instance Core.ToQuery InstanceMonitoring where
  toQuery InstanceMonitoring' {..} =
    Core.mconcat ["Enabled" Core.=: enabled]
