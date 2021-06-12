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
-- Module      : Network.AWS.OpsWorks.Types.ReportedOs
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.ReportedOs where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A registered instance\'s reported operating system.
--
-- /See:/ 'newReportedOs' smart constructor.
data ReportedOs = ReportedOs'
  { -- | The operating system version.
    version :: Core.Maybe Core.Text,
    -- | The operating system name.
    name :: Core.Maybe Core.Text,
    -- | The operating system family.
    family :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ReportedOs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'version', 'reportedOs_version' - The operating system version.
--
-- 'name', 'reportedOs_name' - The operating system name.
--
-- 'family', 'reportedOs_family' - The operating system family.
newReportedOs ::
  ReportedOs
newReportedOs =
  ReportedOs'
    { version = Core.Nothing,
      name = Core.Nothing,
      family = Core.Nothing
    }

-- | The operating system version.
reportedOs_version :: Lens.Lens' ReportedOs (Core.Maybe Core.Text)
reportedOs_version = Lens.lens (\ReportedOs' {version} -> version) (\s@ReportedOs' {} a -> s {version = a} :: ReportedOs)

-- | The operating system name.
reportedOs_name :: Lens.Lens' ReportedOs (Core.Maybe Core.Text)
reportedOs_name = Lens.lens (\ReportedOs' {name} -> name) (\s@ReportedOs' {} a -> s {name = a} :: ReportedOs)

-- | The operating system family.
reportedOs_family :: Lens.Lens' ReportedOs (Core.Maybe Core.Text)
reportedOs_family = Lens.lens (\ReportedOs' {family} -> family) (\s@ReportedOs' {} a -> s {family = a} :: ReportedOs)

instance Core.FromJSON ReportedOs where
  parseJSON =
    Core.withObject
      "ReportedOs"
      ( \x ->
          ReportedOs'
            Core.<$> (x Core..:? "Version")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "Family")
      )

instance Core.Hashable ReportedOs

instance Core.NFData ReportedOs
