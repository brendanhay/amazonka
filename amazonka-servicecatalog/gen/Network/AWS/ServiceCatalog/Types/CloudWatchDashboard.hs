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
-- Module      : Network.AWS.ServiceCatalog.Types.CloudWatchDashboard
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.CloudWatchDashboard where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Information about a CloudWatch dashboard.
--
-- /See:/ 'newCloudWatchDashboard' smart constructor.
data CloudWatchDashboard = CloudWatchDashboard'
  { -- | The name of the CloudWatch dashboard.
    name :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CloudWatchDashboard' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'cloudWatchDashboard_name' - The name of the CloudWatch dashboard.
newCloudWatchDashboard ::
  CloudWatchDashboard
newCloudWatchDashboard =
  CloudWatchDashboard' {name = Core.Nothing}

-- | The name of the CloudWatch dashboard.
cloudWatchDashboard_name :: Lens.Lens' CloudWatchDashboard (Core.Maybe Core.Text)
cloudWatchDashboard_name = Lens.lens (\CloudWatchDashboard' {name} -> name) (\s@CloudWatchDashboard' {} a -> s {name = a} :: CloudWatchDashboard)

instance Core.FromJSON CloudWatchDashboard where
  parseJSON =
    Core.withObject
      "CloudWatchDashboard"
      ( \x ->
          CloudWatchDashboard' Core.<$> (x Core..:? "Name")
      )

instance Core.Hashable CloudWatchDashboard

instance Core.NFData CloudWatchDashboard
