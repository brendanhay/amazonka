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
-- Module      : Network.AWS.EC2.Types.LaunchTemplateCpuOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplateCpuOptions where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens

-- | The CPU options for the instance.
--
-- /See:/ 'newLaunchTemplateCpuOptions' smart constructor.
data LaunchTemplateCpuOptions = LaunchTemplateCpuOptions'
  { -- | The number of threads per CPU core.
    threadsPerCore :: Core.Maybe Core.Int,
    -- | The number of CPU cores for the instance.
    coreCount :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'LaunchTemplateCpuOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'threadsPerCore', 'launchTemplateCpuOptions_threadsPerCore' - The number of threads per CPU core.
--
-- 'coreCount', 'launchTemplateCpuOptions_coreCount' - The number of CPU cores for the instance.
newLaunchTemplateCpuOptions ::
  LaunchTemplateCpuOptions
newLaunchTemplateCpuOptions =
  LaunchTemplateCpuOptions'
    { threadsPerCore =
        Core.Nothing,
      coreCount = Core.Nothing
    }

-- | The number of threads per CPU core.
launchTemplateCpuOptions_threadsPerCore :: Lens.Lens' LaunchTemplateCpuOptions (Core.Maybe Core.Int)
launchTemplateCpuOptions_threadsPerCore = Lens.lens (\LaunchTemplateCpuOptions' {threadsPerCore} -> threadsPerCore) (\s@LaunchTemplateCpuOptions' {} a -> s {threadsPerCore = a} :: LaunchTemplateCpuOptions)

-- | The number of CPU cores for the instance.
launchTemplateCpuOptions_coreCount :: Lens.Lens' LaunchTemplateCpuOptions (Core.Maybe Core.Int)
launchTemplateCpuOptions_coreCount = Lens.lens (\LaunchTemplateCpuOptions' {coreCount} -> coreCount) (\s@LaunchTemplateCpuOptions' {} a -> s {coreCount = a} :: LaunchTemplateCpuOptions)

instance Core.FromXML LaunchTemplateCpuOptions where
  parseXML x =
    LaunchTemplateCpuOptions'
      Core.<$> (x Core..@? "threadsPerCore")
      Core.<*> (x Core..@? "coreCount")

instance Core.Hashable LaunchTemplateCpuOptions

instance Core.NFData LaunchTemplateCpuOptions
