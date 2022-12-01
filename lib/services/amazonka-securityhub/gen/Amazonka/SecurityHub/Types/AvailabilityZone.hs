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
-- Module      : Amazonka.SecurityHub.Types.AvailabilityZone
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AvailabilityZone where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about an Availability Zone.
--
-- /See:/ 'newAvailabilityZone' smart constructor.
data AvailabilityZone = AvailabilityZone'
  { -- | The name of the Availability Zone.
    zoneName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the subnet. You can specify one subnet per Availability Zone.
    subnetId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AvailabilityZone' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'zoneName', 'availabilityZone_zoneName' - The name of the Availability Zone.
--
-- 'subnetId', 'availabilityZone_subnetId' - The ID of the subnet. You can specify one subnet per Availability Zone.
newAvailabilityZone ::
  AvailabilityZone
newAvailabilityZone =
  AvailabilityZone'
    { zoneName = Prelude.Nothing,
      subnetId = Prelude.Nothing
    }

-- | The name of the Availability Zone.
availabilityZone_zoneName :: Lens.Lens' AvailabilityZone (Prelude.Maybe Prelude.Text)
availabilityZone_zoneName = Lens.lens (\AvailabilityZone' {zoneName} -> zoneName) (\s@AvailabilityZone' {} a -> s {zoneName = a} :: AvailabilityZone)

-- | The ID of the subnet. You can specify one subnet per Availability Zone.
availabilityZone_subnetId :: Lens.Lens' AvailabilityZone (Prelude.Maybe Prelude.Text)
availabilityZone_subnetId = Lens.lens (\AvailabilityZone' {subnetId} -> subnetId) (\s@AvailabilityZone' {} a -> s {subnetId = a} :: AvailabilityZone)

instance Core.FromJSON AvailabilityZone where
  parseJSON =
    Core.withObject
      "AvailabilityZone"
      ( \x ->
          AvailabilityZone'
            Prelude.<$> (x Core..:? "ZoneName")
            Prelude.<*> (x Core..:? "SubnetId")
      )

instance Prelude.Hashable AvailabilityZone where
  hashWithSalt _salt AvailabilityZone' {..} =
    _salt `Prelude.hashWithSalt` zoneName
      `Prelude.hashWithSalt` subnetId

instance Prelude.NFData AvailabilityZone where
  rnf AvailabilityZone' {..} =
    Prelude.rnf zoneName
      `Prelude.seq` Prelude.rnf subnetId

instance Core.ToJSON AvailabilityZone where
  toJSON AvailabilityZone' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ZoneName" Core..=) Prelude.<$> zoneName,
            ("SubnetId" Core..=) Prelude.<$> subnetId
          ]
      )
