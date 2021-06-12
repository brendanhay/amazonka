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
-- Module      : Network.AWS.RDS.Types.AvailabilityZone
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.AvailabilityZone where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains Availability Zone information.
--
-- This data type is used as an element in the @OrderableDBInstanceOption@
-- data type.
--
-- /See:/ 'newAvailabilityZone' smart constructor.
data AvailabilityZone = AvailabilityZone'
  { -- | The name of the Availability Zone.
    name :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AvailabilityZone' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'availabilityZone_name' - The name of the Availability Zone.
newAvailabilityZone ::
  AvailabilityZone
newAvailabilityZone =
  AvailabilityZone' {name = Core.Nothing}

-- | The name of the Availability Zone.
availabilityZone_name :: Lens.Lens' AvailabilityZone (Core.Maybe Core.Text)
availabilityZone_name = Lens.lens (\AvailabilityZone' {name} -> name) (\s@AvailabilityZone' {} a -> s {name = a} :: AvailabilityZone)

instance Core.FromXML AvailabilityZone where
  parseXML x =
    AvailabilityZone' Core.<$> (x Core..@? "Name")

instance Core.Hashable AvailabilityZone

instance Core.NFData AvailabilityZone
