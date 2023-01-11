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
-- Module      : Amazonka.EC2.Types.SpotPlacement
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.SpotPlacement where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.Tenancy
import qualified Amazonka.Prelude as Prelude

-- | Describes Spot Instance placement.
--
-- /See:/ 'newSpotPlacement' smart constructor.
data SpotPlacement = SpotPlacement'
  { -- | The Availability Zone.
    --
    -- [Spot Fleet only] To specify multiple Availability Zones, separate them
    -- using commas; for example, \"us-west-2a, us-west-2b\".
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The name of the placement group.
    groupName :: Prelude.Maybe Prelude.Text,
    -- | The tenancy of the instance (if the instance is running in a VPC). An
    -- instance with a tenancy of @dedicated@ runs on single-tenant hardware.
    -- The @host@ tenancy is not supported for Spot Instances.
    tenancy :: Prelude.Maybe Tenancy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SpotPlacement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availabilityZone', 'spotPlacement_availabilityZone' - The Availability Zone.
--
-- [Spot Fleet only] To specify multiple Availability Zones, separate them
-- using commas; for example, \"us-west-2a, us-west-2b\".
--
-- 'groupName', 'spotPlacement_groupName' - The name of the placement group.
--
-- 'tenancy', 'spotPlacement_tenancy' - The tenancy of the instance (if the instance is running in a VPC). An
-- instance with a tenancy of @dedicated@ runs on single-tenant hardware.
-- The @host@ tenancy is not supported for Spot Instances.
newSpotPlacement ::
  SpotPlacement
newSpotPlacement =
  SpotPlacement'
    { availabilityZone = Prelude.Nothing,
      groupName = Prelude.Nothing,
      tenancy = Prelude.Nothing
    }

-- | The Availability Zone.
--
-- [Spot Fleet only] To specify multiple Availability Zones, separate them
-- using commas; for example, \"us-west-2a, us-west-2b\".
spotPlacement_availabilityZone :: Lens.Lens' SpotPlacement (Prelude.Maybe Prelude.Text)
spotPlacement_availabilityZone = Lens.lens (\SpotPlacement' {availabilityZone} -> availabilityZone) (\s@SpotPlacement' {} a -> s {availabilityZone = a} :: SpotPlacement)

-- | The name of the placement group.
spotPlacement_groupName :: Lens.Lens' SpotPlacement (Prelude.Maybe Prelude.Text)
spotPlacement_groupName = Lens.lens (\SpotPlacement' {groupName} -> groupName) (\s@SpotPlacement' {} a -> s {groupName = a} :: SpotPlacement)

-- | The tenancy of the instance (if the instance is running in a VPC). An
-- instance with a tenancy of @dedicated@ runs on single-tenant hardware.
-- The @host@ tenancy is not supported for Spot Instances.
spotPlacement_tenancy :: Lens.Lens' SpotPlacement (Prelude.Maybe Tenancy)
spotPlacement_tenancy = Lens.lens (\SpotPlacement' {tenancy} -> tenancy) (\s@SpotPlacement' {} a -> s {tenancy = a} :: SpotPlacement)

instance Data.FromXML SpotPlacement where
  parseXML x =
    SpotPlacement'
      Prelude.<$> (x Data..@? "availabilityZone")
      Prelude.<*> (x Data..@? "groupName")
      Prelude.<*> (x Data..@? "tenancy")

instance Prelude.Hashable SpotPlacement where
  hashWithSalt _salt SpotPlacement' {..} =
    _salt `Prelude.hashWithSalt` availabilityZone
      `Prelude.hashWithSalt` groupName
      `Prelude.hashWithSalt` tenancy

instance Prelude.NFData SpotPlacement where
  rnf SpotPlacement' {..} =
    Prelude.rnf availabilityZone
      `Prelude.seq` Prelude.rnf groupName
      `Prelude.seq` Prelude.rnf tenancy

instance Data.ToQuery SpotPlacement where
  toQuery SpotPlacement' {..} =
    Prelude.mconcat
      [ "AvailabilityZone" Data.=: availabilityZone,
        "GroupName" Data.=: groupName,
        "Tenancy" Data.=: tenancy
      ]
