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
-- Module      : Network.AWS.EC2.Types.LaunchTemplatesMonitoring
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplatesMonitoring where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens

-- | Describes the monitoring for the instance.
--
-- /See:/ 'newLaunchTemplatesMonitoring' smart constructor.
data LaunchTemplatesMonitoring = LaunchTemplatesMonitoring'
  { -- | Indicates whether detailed monitoring is enabled. Otherwise, basic
    -- monitoring is enabled.
    enabled :: Core.Maybe Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'LaunchTemplatesMonitoring' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'launchTemplatesMonitoring_enabled' - Indicates whether detailed monitoring is enabled. Otherwise, basic
-- monitoring is enabled.
newLaunchTemplatesMonitoring ::
  LaunchTemplatesMonitoring
newLaunchTemplatesMonitoring =
  LaunchTemplatesMonitoring' {enabled = Core.Nothing}

-- | Indicates whether detailed monitoring is enabled. Otherwise, basic
-- monitoring is enabled.
launchTemplatesMonitoring_enabled :: Lens.Lens' LaunchTemplatesMonitoring (Core.Maybe Core.Bool)
launchTemplatesMonitoring_enabled = Lens.lens (\LaunchTemplatesMonitoring' {enabled} -> enabled) (\s@LaunchTemplatesMonitoring' {} a -> s {enabled = a} :: LaunchTemplatesMonitoring)

instance Core.FromXML LaunchTemplatesMonitoring where
  parseXML x =
    LaunchTemplatesMonitoring'
      Core.<$> (x Core..@? "enabled")

instance Core.Hashable LaunchTemplatesMonitoring

instance Core.NFData LaunchTemplatesMonitoring
