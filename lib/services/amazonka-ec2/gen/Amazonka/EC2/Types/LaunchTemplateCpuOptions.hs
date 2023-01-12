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
-- Module      : Amazonka.EC2.Types.LaunchTemplateCpuOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.LaunchTemplateCpuOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | The CPU options for the instance.
--
-- /See:/ 'newLaunchTemplateCpuOptions' smart constructor.
data LaunchTemplateCpuOptions = LaunchTemplateCpuOptions'
  { -- | The number of CPU cores for the instance.
    coreCount :: Prelude.Maybe Prelude.Int,
    -- | The number of threads per CPU core.
    threadsPerCore :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LaunchTemplateCpuOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'coreCount', 'launchTemplateCpuOptions_coreCount' - The number of CPU cores for the instance.
--
-- 'threadsPerCore', 'launchTemplateCpuOptions_threadsPerCore' - The number of threads per CPU core.
newLaunchTemplateCpuOptions ::
  LaunchTemplateCpuOptions
newLaunchTemplateCpuOptions =
  LaunchTemplateCpuOptions'
    { coreCount =
        Prelude.Nothing,
      threadsPerCore = Prelude.Nothing
    }

-- | The number of CPU cores for the instance.
launchTemplateCpuOptions_coreCount :: Lens.Lens' LaunchTemplateCpuOptions (Prelude.Maybe Prelude.Int)
launchTemplateCpuOptions_coreCount = Lens.lens (\LaunchTemplateCpuOptions' {coreCount} -> coreCount) (\s@LaunchTemplateCpuOptions' {} a -> s {coreCount = a} :: LaunchTemplateCpuOptions)

-- | The number of threads per CPU core.
launchTemplateCpuOptions_threadsPerCore :: Lens.Lens' LaunchTemplateCpuOptions (Prelude.Maybe Prelude.Int)
launchTemplateCpuOptions_threadsPerCore = Lens.lens (\LaunchTemplateCpuOptions' {threadsPerCore} -> threadsPerCore) (\s@LaunchTemplateCpuOptions' {} a -> s {threadsPerCore = a} :: LaunchTemplateCpuOptions)

instance Data.FromXML LaunchTemplateCpuOptions where
  parseXML x =
    LaunchTemplateCpuOptions'
      Prelude.<$> (x Data..@? "coreCount")
      Prelude.<*> (x Data..@? "threadsPerCore")

instance Prelude.Hashable LaunchTemplateCpuOptions where
  hashWithSalt _salt LaunchTemplateCpuOptions' {..} =
    _salt `Prelude.hashWithSalt` coreCount
      `Prelude.hashWithSalt` threadsPerCore

instance Prelude.NFData LaunchTemplateCpuOptions where
  rnf LaunchTemplateCpuOptions' {..} =
    Prelude.rnf coreCount
      `Prelude.seq` Prelude.rnf threadsPerCore
