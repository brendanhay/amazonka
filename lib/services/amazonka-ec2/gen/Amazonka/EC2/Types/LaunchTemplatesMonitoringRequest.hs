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
-- Module      : Amazonka.EC2.Types.LaunchTemplatesMonitoringRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.LaunchTemplatesMonitoringRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Describes the monitoring for the instance.
--
-- /See:/ 'newLaunchTemplatesMonitoringRequest' smart constructor.
data LaunchTemplatesMonitoringRequest = LaunchTemplatesMonitoringRequest'
  { -- | Specify @true@ to enable detailed monitoring. Otherwise, basic
    -- monitoring is enabled.
    enabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LaunchTemplatesMonitoringRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'launchTemplatesMonitoringRequest_enabled' - Specify @true@ to enable detailed monitoring. Otherwise, basic
-- monitoring is enabled.
newLaunchTemplatesMonitoringRequest ::
  LaunchTemplatesMonitoringRequest
newLaunchTemplatesMonitoringRequest =
  LaunchTemplatesMonitoringRequest'
    { enabled =
        Prelude.Nothing
    }

-- | Specify @true@ to enable detailed monitoring. Otherwise, basic
-- monitoring is enabled.
launchTemplatesMonitoringRequest_enabled :: Lens.Lens' LaunchTemplatesMonitoringRequest (Prelude.Maybe Prelude.Bool)
launchTemplatesMonitoringRequest_enabled = Lens.lens (\LaunchTemplatesMonitoringRequest' {enabled} -> enabled) (\s@LaunchTemplatesMonitoringRequest' {} a -> s {enabled = a} :: LaunchTemplatesMonitoringRequest)

instance
  Prelude.Hashable
    LaunchTemplatesMonitoringRequest
  where
  hashWithSalt
    _salt
    LaunchTemplatesMonitoringRequest' {..} =
      _salt `Prelude.hashWithSalt` enabled

instance
  Prelude.NFData
    LaunchTemplatesMonitoringRequest
  where
  rnf LaunchTemplatesMonitoringRequest' {..} =
    Prelude.rnf enabled

instance
  Data.ToQuery
    LaunchTemplatesMonitoringRequest
  where
  toQuery LaunchTemplatesMonitoringRequest' {..} =
    Prelude.mconcat ["Enabled" Data.=: enabled]
