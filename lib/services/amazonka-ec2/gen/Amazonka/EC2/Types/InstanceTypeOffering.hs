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
-- Module      : Amazonka.EC2.Types.InstanceTypeOffering
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.InstanceTypeOffering where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.InstanceType
import Amazonka.EC2.Types.LocationType
import qualified Amazonka.Prelude as Prelude

-- | The instance types offered.
--
-- /See:/ 'newInstanceTypeOffering' smart constructor.
data InstanceTypeOffering = InstanceTypeOffering'
  { -- | The instance type. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance types>
    -- in the /Amazon EC2 User Guide/.
    instanceType :: Prelude.Maybe InstanceType,
    -- | The identifier for the location. This depends on the location type. For
    -- example, if the location type is @region@, the location is the Region
    -- code (for example, @us-east-2@.)
    location :: Prelude.Maybe Prelude.Text,
    -- | The location type.
    locationType :: Prelude.Maybe LocationType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceTypeOffering' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceType', 'instanceTypeOffering_instanceType' - The instance type. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance types>
-- in the /Amazon EC2 User Guide/.
--
-- 'location', 'instanceTypeOffering_location' - The identifier for the location. This depends on the location type. For
-- example, if the location type is @region@, the location is the Region
-- code (for example, @us-east-2@.)
--
-- 'locationType', 'instanceTypeOffering_locationType' - The location type.
newInstanceTypeOffering ::
  InstanceTypeOffering
newInstanceTypeOffering =
  InstanceTypeOffering'
    { instanceType =
        Prelude.Nothing,
      location = Prelude.Nothing,
      locationType = Prelude.Nothing
    }

-- | The instance type. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance types>
-- in the /Amazon EC2 User Guide/.
instanceTypeOffering_instanceType :: Lens.Lens' InstanceTypeOffering (Prelude.Maybe InstanceType)
instanceTypeOffering_instanceType = Lens.lens (\InstanceTypeOffering' {instanceType} -> instanceType) (\s@InstanceTypeOffering' {} a -> s {instanceType = a} :: InstanceTypeOffering)

-- | The identifier for the location. This depends on the location type. For
-- example, if the location type is @region@, the location is the Region
-- code (for example, @us-east-2@.)
instanceTypeOffering_location :: Lens.Lens' InstanceTypeOffering (Prelude.Maybe Prelude.Text)
instanceTypeOffering_location = Lens.lens (\InstanceTypeOffering' {location} -> location) (\s@InstanceTypeOffering' {} a -> s {location = a} :: InstanceTypeOffering)

-- | The location type.
instanceTypeOffering_locationType :: Lens.Lens' InstanceTypeOffering (Prelude.Maybe LocationType)
instanceTypeOffering_locationType = Lens.lens (\InstanceTypeOffering' {locationType} -> locationType) (\s@InstanceTypeOffering' {} a -> s {locationType = a} :: InstanceTypeOffering)

instance Data.FromXML InstanceTypeOffering where
  parseXML x =
    InstanceTypeOffering'
      Prelude.<$> (x Data..@? "instanceType")
      Prelude.<*> (x Data..@? "location")
      Prelude.<*> (x Data..@? "locationType")

instance Prelude.Hashable InstanceTypeOffering where
  hashWithSalt _salt InstanceTypeOffering' {..} =
    _salt
      `Prelude.hashWithSalt` instanceType
      `Prelude.hashWithSalt` location
      `Prelude.hashWithSalt` locationType

instance Prelude.NFData InstanceTypeOffering where
  rnf InstanceTypeOffering' {..} =
    Prelude.rnf instanceType
      `Prelude.seq` Prelude.rnf location
      `Prelude.seq` Prelude.rnf locationType
