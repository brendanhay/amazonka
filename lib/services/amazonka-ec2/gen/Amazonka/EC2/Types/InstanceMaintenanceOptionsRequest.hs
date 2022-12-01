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
-- Module      : Amazonka.EC2.Types.InstanceMaintenanceOptionsRequest
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.InstanceMaintenanceOptionsRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.InstanceAutoRecoveryState
import qualified Amazonka.Prelude as Prelude

-- | The maintenance options for the instance.
--
-- /See:/ 'newInstanceMaintenanceOptionsRequest' smart constructor.
data InstanceMaintenanceOptionsRequest = InstanceMaintenanceOptionsRequest'
  { -- | Disables the automatic recovery behavior of your instance or sets it to
    -- default. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-recover.html#instance-configuration-recovery Simplified automatic recovery>.
    autoRecovery :: Prelude.Maybe InstanceAutoRecoveryState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceMaintenanceOptionsRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoRecovery', 'instanceMaintenanceOptionsRequest_autoRecovery' - Disables the automatic recovery behavior of your instance or sets it to
-- default. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-recover.html#instance-configuration-recovery Simplified automatic recovery>.
newInstanceMaintenanceOptionsRequest ::
  InstanceMaintenanceOptionsRequest
newInstanceMaintenanceOptionsRequest =
  InstanceMaintenanceOptionsRequest'
    { autoRecovery =
        Prelude.Nothing
    }

-- | Disables the automatic recovery behavior of your instance or sets it to
-- default. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-recover.html#instance-configuration-recovery Simplified automatic recovery>.
instanceMaintenanceOptionsRequest_autoRecovery :: Lens.Lens' InstanceMaintenanceOptionsRequest (Prelude.Maybe InstanceAutoRecoveryState)
instanceMaintenanceOptionsRequest_autoRecovery = Lens.lens (\InstanceMaintenanceOptionsRequest' {autoRecovery} -> autoRecovery) (\s@InstanceMaintenanceOptionsRequest' {} a -> s {autoRecovery = a} :: InstanceMaintenanceOptionsRequest)

instance
  Prelude.Hashable
    InstanceMaintenanceOptionsRequest
  where
  hashWithSalt
    _salt
    InstanceMaintenanceOptionsRequest' {..} =
      _salt `Prelude.hashWithSalt` autoRecovery

instance
  Prelude.NFData
    InstanceMaintenanceOptionsRequest
  where
  rnf InstanceMaintenanceOptionsRequest' {..} =
    Prelude.rnf autoRecovery

instance
  Core.ToQuery
    InstanceMaintenanceOptionsRequest
  where
  toQuery InstanceMaintenanceOptionsRequest' {..} =
    Prelude.mconcat
      ["AutoRecovery" Core.=: autoRecovery]
