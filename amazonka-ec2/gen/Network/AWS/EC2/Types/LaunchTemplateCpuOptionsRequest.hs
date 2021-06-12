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
-- Module      : Network.AWS.EC2.Types.LaunchTemplateCpuOptionsRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplateCpuOptionsRequest where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens

-- | The CPU options for the instance. Both the core count and threads per
-- core must be specified in the request.
--
-- /See:/ 'newLaunchTemplateCpuOptionsRequest' smart constructor.
data LaunchTemplateCpuOptionsRequest = LaunchTemplateCpuOptionsRequest'
  { -- | The number of threads per CPU core. To disable multithreading for the
    -- instance, specify a value of 1. Otherwise, specify the default value of
    -- 2.
    threadsPerCore :: Core.Maybe Core.Int,
    -- | The number of CPU cores for the instance.
    coreCount :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'LaunchTemplateCpuOptionsRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'threadsPerCore', 'launchTemplateCpuOptionsRequest_threadsPerCore' - The number of threads per CPU core. To disable multithreading for the
-- instance, specify a value of 1. Otherwise, specify the default value of
-- 2.
--
-- 'coreCount', 'launchTemplateCpuOptionsRequest_coreCount' - The number of CPU cores for the instance.
newLaunchTemplateCpuOptionsRequest ::
  LaunchTemplateCpuOptionsRequest
newLaunchTemplateCpuOptionsRequest =
  LaunchTemplateCpuOptionsRequest'
    { threadsPerCore =
        Core.Nothing,
      coreCount = Core.Nothing
    }

-- | The number of threads per CPU core. To disable multithreading for the
-- instance, specify a value of 1. Otherwise, specify the default value of
-- 2.
launchTemplateCpuOptionsRequest_threadsPerCore :: Lens.Lens' LaunchTemplateCpuOptionsRequest (Core.Maybe Core.Int)
launchTemplateCpuOptionsRequest_threadsPerCore = Lens.lens (\LaunchTemplateCpuOptionsRequest' {threadsPerCore} -> threadsPerCore) (\s@LaunchTemplateCpuOptionsRequest' {} a -> s {threadsPerCore = a} :: LaunchTemplateCpuOptionsRequest)

-- | The number of CPU cores for the instance.
launchTemplateCpuOptionsRequest_coreCount :: Lens.Lens' LaunchTemplateCpuOptionsRequest (Core.Maybe Core.Int)
launchTemplateCpuOptionsRequest_coreCount = Lens.lens (\LaunchTemplateCpuOptionsRequest' {coreCount} -> coreCount) (\s@LaunchTemplateCpuOptionsRequest' {} a -> s {coreCount = a} :: LaunchTemplateCpuOptionsRequest)

instance
  Core.Hashable
    LaunchTemplateCpuOptionsRequest

instance Core.NFData LaunchTemplateCpuOptionsRequest

instance Core.ToQuery LaunchTemplateCpuOptionsRequest where
  toQuery LaunchTemplateCpuOptionsRequest' {..} =
    Core.mconcat
      [ "ThreadsPerCore" Core.=: threadsPerCore,
        "CoreCount" Core.=: coreCount
      ]
