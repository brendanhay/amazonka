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
-- Module      : Amazonka.XRay.Types.AvailabilityZoneDetail
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.XRay.Types.AvailabilityZoneDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A list of Availability Zones corresponding to the segments in a trace.
--
-- /See:/ 'newAvailabilityZoneDetail' smart constructor.
data AvailabilityZoneDetail = AvailabilityZoneDetail'
  { -- | The name of a corresponding Availability Zone.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  AvailabilityZoneDetail' {name = Prelude.Nothing}

-- | The name of a corresponding Availability Zone.
availabilityZoneDetail_name :: Lens.Lens' AvailabilityZoneDetail (Prelude.Maybe Prelude.Text)
availabilityZoneDetail_name = Lens.lens (\AvailabilityZoneDetail' {name} -> name) (\s@AvailabilityZoneDetail' {} a -> s {name = a} :: AvailabilityZoneDetail)

instance Data.FromJSON AvailabilityZoneDetail where
  parseJSON =
    Data.withObject
      "AvailabilityZoneDetail"
      ( \x ->
          AvailabilityZoneDetail'
            Prelude.<$> (x Data..:? "Name")
      )

instance Prelude.Hashable AvailabilityZoneDetail where
  hashWithSalt _salt AvailabilityZoneDetail' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData AvailabilityZoneDetail where
  rnf AvailabilityZoneDetail' {..} = Prelude.rnf name
