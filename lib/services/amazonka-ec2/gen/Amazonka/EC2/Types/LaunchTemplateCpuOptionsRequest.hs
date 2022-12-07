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
-- Module      : Amazonka.EC2.Types.LaunchTemplateCpuOptionsRequest
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.LaunchTemplateCpuOptionsRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | The CPU options for the instance. Both the core count and threads per
-- core must be specified in the request.
--
-- /See:/ 'newLaunchTemplateCpuOptionsRequest' smart constructor.
data LaunchTemplateCpuOptionsRequest = LaunchTemplateCpuOptionsRequest'
  { -- | The number of CPU cores for the instance.
    coreCount :: Prelude.Maybe Prelude.Int,
    -- | The number of threads per CPU core. To disable multithreading for the
    -- instance, specify a value of @1@. Otherwise, specify the default value
    -- of @2@.
    threadsPerCore :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LaunchTemplateCpuOptionsRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'coreCount', 'launchTemplateCpuOptionsRequest_coreCount' - The number of CPU cores for the instance.
--
-- 'threadsPerCore', 'launchTemplateCpuOptionsRequest_threadsPerCore' - The number of threads per CPU core. To disable multithreading for the
-- instance, specify a value of @1@. Otherwise, specify the default value
-- of @2@.
newLaunchTemplateCpuOptionsRequest ::
  LaunchTemplateCpuOptionsRequest
newLaunchTemplateCpuOptionsRequest =
  LaunchTemplateCpuOptionsRequest'
    { coreCount =
        Prelude.Nothing,
      threadsPerCore = Prelude.Nothing
    }

-- | The number of CPU cores for the instance.
launchTemplateCpuOptionsRequest_coreCount :: Lens.Lens' LaunchTemplateCpuOptionsRequest (Prelude.Maybe Prelude.Int)
launchTemplateCpuOptionsRequest_coreCount = Lens.lens (\LaunchTemplateCpuOptionsRequest' {coreCount} -> coreCount) (\s@LaunchTemplateCpuOptionsRequest' {} a -> s {coreCount = a} :: LaunchTemplateCpuOptionsRequest)

-- | The number of threads per CPU core. To disable multithreading for the
-- instance, specify a value of @1@. Otherwise, specify the default value
-- of @2@.
launchTemplateCpuOptionsRequest_threadsPerCore :: Lens.Lens' LaunchTemplateCpuOptionsRequest (Prelude.Maybe Prelude.Int)
launchTemplateCpuOptionsRequest_threadsPerCore = Lens.lens (\LaunchTemplateCpuOptionsRequest' {threadsPerCore} -> threadsPerCore) (\s@LaunchTemplateCpuOptionsRequest' {} a -> s {threadsPerCore = a} :: LaunchTemplateCpuOptionsRequest)

instance
  Prelude.Hashable
    LaunchTemplateCpuOptionsRequest
  where
  hashWithSalt
    _salt
    LaunchTemplateCpuOptionsRequest' {..} =
      _salt `Prelude.hashWithSalt` coreCount
        `Prelude.hashWithSalt` threadsPerCore

instance
  Prelude.NFData
    LaunchTemplateCpuOptionsRequest
  where
  rnf LaunchTemplateCpuOptionsRequest' {..} =
    Prelude.rnf coreCount
      `Prelude.seq` Prelude.rnf threadsPerCore

instance Data.ToQuery LaunchTemplateCpuOptionsRequest where
  toQuery LaunchTemplateCpuOptionsRequest' {..} =
    Prelude.mconcat
      [ "CoreCount" Data.=: coreCount,
        "ThreadsPerCore" Data.=: threadsPerCore
      ]
