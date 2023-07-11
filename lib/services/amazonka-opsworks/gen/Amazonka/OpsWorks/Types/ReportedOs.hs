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
-- Module      : Amazonka.OpsWorks.Types.ReportedOs
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpsWorks.Types.ReportedOs where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A registered instance\'s reported operating system.
--
-- /See:/ 'newReportedOs' smart constructor.
data ReportedOs = ReportedOs'
  { -- | The operating system family.
    family :: Prelude.Maybe Prelude.Text,
    -- | The operating system name.
    name :: Prelude.Maybe Prelude.Text,
    -- | The operating system version.
    version :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReportedOs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'family', 'reportedOs_family' - The operating system family.
--
-- 'name', 'reportedOs_name' - The operating system name.
--
-- 'version', 'reportedOs_version' - The operating system version.
newReportedOs ::
  ReportedOs
newReportedOs =
  ReportedOs'
    { family = Prelude.Nothing,
      name = Prelude.Nothing,
      version = Prelude.Nothing
    }

-- | The operating system family.
reportedOs_family :: Lens.Lens' ReportedOs (Prelude.Maybe Prelude.Text)
reportedOs_family = Lens.lens (\ReportedOs' {family} -> family) (\s@ReportedOs' {} a -> s {family = a} :: ReportedOs)

-- | The operating system name.
reportedOs_name :: Lens.Lens' ReportedOs (Prelude.Maybe Prelude.Text)
reportedOs_name = Lens.lens (\ReportedOs' {name} -> name) (\s@ReportedOs' {} a -> s {name = a} :: ReportedOs)

-- | The operating system version.
reportedOs_version :: Lens.Lens' ReportedOs (Prelude.Maybe Prelude.Text)
reportedOs_version = Lens.lens (\ReportedOs' {version} -> version) (\s@ReportedOs' {} a -> s {version = a} :: ReportedOs)

instance Data.FromJSON ReportedOs where
  parseJSON =
    Data.withObject
      "ReportedOs"
      ( \x ->
          ReportedOs'
            Prelude.<$> (x Data..:? "Family")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Version")
      )

instance Prelude.Hashable ReportedOs where
  hashWithSalt _salt ReportedOs' {..} =
    _salt
      `Prelude.hashWithSalt` family
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` version

instance Prelude.NFData ReportedOs where
  rnf ReportedOs' {..} =
    Prelude.rnf family
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf version
