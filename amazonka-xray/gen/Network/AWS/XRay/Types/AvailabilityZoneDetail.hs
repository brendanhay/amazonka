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
-- Module      : Network.AWS.XRay.Types.AvailabilityZoneDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.AvailabilityZoneDetail where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A list of Availability Zones corresponding to the segments in a trace.
--
-- /See:/ 'newAvailabilityZoneDetail' smart constructor.
data AvailabilityZoneDetail = AvailabilityZoneDetail'
  { -- | The name of a corresponding Availability Zone.
    name :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AvailabilityZoneDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'availabilityZoneDetail_name' - The name of a corresponding Availability Zone.
newAvailabilityZoneDetail ::
  AvailabilityZoneDetail
newAvailabilityZoneDetail =
  AvailabilityZoneDetail' {name = Core.Nothing}

-- | The name of a corresponding Availability Zone.
availabilityZoneDetail_name :: Lens.Lens' AvailabilityZoneDetail (Core.Maybe Core.Text)
availabilityZoneDetail_name = Lens.lens (\AvailabilityZoneDetail' {name} -> name) (\s@AvailabilityZoneDetail' {} a -> s {name = a} :: AvailabilityZoneDetail)

instance Core.FromJSON AvailabilityZoneDetail where
  parseJSON =
    Core.withObject
      "AvailabilityZoneDetail"
      ( \x ->
          AvailabilityZoneDetail' Core.<$> (x Core..:? "Name")
      )

instance Core.Hashable AvailabilityZoneDetail

instance Core.NFData AvailabilityZoneDetail
