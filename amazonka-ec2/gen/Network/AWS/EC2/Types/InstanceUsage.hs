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
-- Module      : Network.AWS.EC2.Types.InstanceUsage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceUsage where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens

-- | Information about the Capacity Reservation usage.
--
-- /See:/ 'newInstanceUsage' smart constructor.
data InstanceUsage = InstanceUsage'
  { -- | The ID of the AWS account that is making use of the Capacity
    -- Reservation.
    accountId :: Core.Maybe Core.Text,
    -- | The number of instances the AWS account currently has in the Capacity
    -- Reservation.
    usedInstanceCount :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'InstanceUsage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'instanceUsage_accountId' - The ID of the AWS account that is making use of the Capacity
-- Reservation.
--
-- 'usedInstanceCount', 'instanceUsage_usedInstanceCount' - The number of instances the AWS account currently has in the Capacity
-- Reservation.
newInstanceUsage ::
  InstanceUsage
newInstanceUsage =
  InstanceUsage'
    { accountId = Core.Nothing,
      usedInstanceCount = Core.Nothing
    }

-- | The ID of the AWS account that is making use of the Capacity
-- Reservation.
instanceUsage_accountId :: Lens.Lens' InstanceUsage (Core.Maybe Core.Text)
instanceUsage_accountId = Lens.lens (\InstanceUsage' {accountId} -> accountId) (\s@InstanceUsage' {} a -> s {accountId = a} :: InstanceUsage)

-- | The number of instances the AWS account currently has in the Capacity
-- Reservation.
instanceUsage_usedInstanceCount :: Lens.Lens' InstanceUsage (Core.Maybe Core.Int)
instanceUsage_usedInstanceCount = Lens.lens (\InstanceUsage' {usedInstanceCount} -> usedInstanceCount) (\s@InstanceUsage' {} a -> s {usedInstanceCount = a} :: InstanceUsage)

instance Core.FromXML InstanceUsage where
  parseXML x =
    InstanceUsage'
      Core.<$> (x Core..@? "accountId")
      Core.<*> (x Core..@? "usedInstanceCount")

instance Core.Hashable InstanceUsage

instance Core.NFData InstanceUsage
