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
-- Module      : Amazonka.EC2.Types.LaunchTemplateInstanceMaintenanceOptionsRequest
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.LaunchTemplateInstanceMaintenanceOptionsRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.LaunchTemplateAutoRecoveryState
import qualified Amazonka.Prelude as Prelude

-- | The maintenance options of your instance.
--
-- /See:/ 'newLaunchTemplateInstanceMaintenanceOptionsRequest' smart constructor.
data LaunchTemplateInstanceMaintenanceOptionsRequest = LaunchTemplateInstanceMaintenanceOptionsRequest'
  { -- | Disables the automatic recovery behavior of your instance or sets it to
    -- default. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-recover.html#instance-configuration-recovery Simplified automatic recovery>.
    autoRecovery :: Prelude.Maybe LaunchTemplateAutoRecoveryState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LaunchTemplateInstanceMaintenanceOptionsRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoRecovery', 'launchTemplateInstanceMaintenanceOptionsRequest_autoRecovery' - Disables the automatic recovery behavior of your instance or sets it to
-- default. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-recover.html#instance-configuration-recovery Simplified automatic recovery>.
newLaunchTemplateInstanceMaintenanceOptionsRequest ::
  LaunchTemplateInstanceMaintenanceOptionsRequest
newLaunchTemplateInstanceMaintenanceOptionsRequest =
  LaunchTemplateInstanceMaintenanceOptionsRequest'
    { autoRecovery =
        Prelude.Nothing
    }

-- | Disables the automatic recovery behavior of your instance or sets it to
-- default. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-recover.html#instance-configuration-recovery Simplified automatic recovery>.
launchTemplateInstanceMaintenanceOptionsRequest_autoRecovery :: Lens.Lens' LaunchTemplateInstanceMaintenanceOptionsRequest (Prelude.Maybe LaunchTemplateAutoRecoveryState)
launchTemplateInstanceMaintenanceOptionsRequest_autoRecovery = Lens.lens (\LaunchTemplateInstanceMaintenanceOptionsRequest' {autoRecovery} -> autoRecovery) (\s@LaunchTemplateInstanceMaintenanceOptionsRequest' {} a -> s {autoRecovery = a} :: LaunchTemplateInstanceMaintenanceOptionsRequest)

instance
  Prelude.Hashable
    LaunchTemplateInstanceMaintenanceOptionsRequest
  where
  hashWithSalt
    _salt
    LaunchTemplateInstanceMaintenanceOptionsRequest' {..} =
      _salt `Prelude.hashWithSalt` autoRecovery

instance
  Prelude.NFData
    LaunchTemplateInstanceMaintenanceOptionsRequest
  where
  rnf
    LaunchTemplateInstanceMaintenanceOptionsRequest' {..} =
      Prelude.rnf autoRecovery

instance
  Data.ToQuery
    LaunchTemplateInstanceMaintenanceOptionsRequest
  where
  toQuery
    LaunchTemplateInstanceMaintenanceOptionsRequest' {..} =
      Prelude.mconcat
        ["AutoRecovery" Data.=: autoRecovery]
