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
-- Module      : Network.AWS.EC2.Types.ReservedInstancesId
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ReservedInstancesId where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens

-- | Describes the ID of a Reserved Instance.
--
-- /See:/ 'newReservedInstancesId' smart constructor.
data ReservedInstancesId = ReservedInstancesId'
  { -- | The ID of the Reserved Instance.
    reservedInstancesId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ReservedInstancesId' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reservedInstancesId', 'reservedInstancesId_reservedInstancesId' - The ID of the Reserved Instance.
newReservedInstancesId ::
  ReservedInstancesId
newReservedInstancesId =
  ReservedInstancesId'
    { reservedInstancesId =
        Core.Nothing
    }

-- | The ID of the Reserved Instance.
reservedInstancesId_reservedInstancesId :: Lens.Lens' ReservedInstancesId (Core.Maybe Core.Text)
reservedInstancesId_reservedInstancesId = Lens.lens (\ReservedInstancesId' {reservedInstancesId} -> reservedInstancesId) (\s@ReservedInstancesId' {} a -> s {reservedInstancesId = a} :: ReservedInstancesId)

instance Core.FromXML ReservedInstancesId where
  parseXML x =
    ReservedInstancesId'
      Core.<$> (x Core..@? "reservedInstancesId")

instance Core.Hashable ReservedInstancesId

instance Core.NFData ReservedInstancesId
