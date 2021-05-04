{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A registered instance\'s reported operating system.
--
-- /See:/ 'newReportedOs' smart constructor.
data ReportedOs = ReportedOs'
  { -- | The operating system version.
    version :: Prelude.Maybe Prelude.Text,
    -- | The operating system name.
    name :: Prelude.Maybe Prelude.Text,
    -- | The operating system family.
    family :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { version = Prelude.Nothing,
      name = Prelude.Nothing,
      family = Prelude.Nothing
    }

-- | The operating system version.
reportedOs_version :: Lens.Lens' ReportedOs (Prelude.Maybe Prelude.Text)
reportedOs_version = Lens.lens (\ReportedOs' {version} -> version) (\s@ReportedOs' {} a -> s {version = a} :: ReportedOs)

-- | The operating system name.
reportedOs_name :: Lens.Lens' ReportedOs (Prelude.Maybe Prelude.Text)
reportedOs_name = Lens.lens (\ReportedOs' {name} -> name) (\s@ReportedOs' {} a -> s {name = a} :: ReportedOs)

-- | The operating system family.
reportedOs_family :: Lens.Lens' ReportedOs (Prelude.Maybe Prelude.Text)
reportedOs_family = Lens.lens (\ReportedOs' {family} -> family) (\s@ReportedOs' {} a -> s {family = a} :: ReportedOs)

instance Prelude.FromJSON ReportedOs where
  parseJSON =
    Prelude.withObject
      "ReportedOs"
      ( \x ->
          ReportedOs'
            Prelude.<$> (x Prelude..:? "Version")
            Prelude.<*> (x Prelude..:? "Name")
            Prelude.<*> (x Prelude..:? "Family")
      )

instance Prelude.Hashable ReportedOs

instance Prelude.NFData ReportedOs
