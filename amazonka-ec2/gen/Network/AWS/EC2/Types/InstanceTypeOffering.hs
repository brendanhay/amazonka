{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.EC2.Types.InstanceTypeOffering
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceTypeOffering where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.InstanceType
import Network.AWS.EC2.Types.LocationType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The instance types offered.
--
-- /See:/ 'newInstanceTypeOffering' smart constructor.
data InstanceTypeOffering = InstanceTypeOffering'
  { -- | The instance type. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance types>
    -- in the /Amazon EC2 User Guide/.
    instanceType :: Prelude.Maybe InstanceType,
    -- | The location type.
    locationType :: Prelude.Maybe LocationType,
    -- | The identifier for the location. This depends on the location type. For
    -- example, if the location type is @region@, the location is the Region
    -- code (for example, @us-east-2@.)
    location :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- 'locationType', 'instanceTypeOffering_locationType' - The location type.
--
-- 'location', 'instanceTypeOffering_location' - The identifier for the location. This depends on the location type. For
-- example, if the location type is @region@, the location is the Region
-- code (for example, @us-east-2@.)
newInstanceTypeOffering ::
  InstanceTypeOffering
newInstanceTypeOffering =
  InstanceTypeOffering'
    { instanceType =
        Prelude.Nothing,
      locationType = Prelude.Nothing,
      location = Prelude.Nothing
    }

-- | The instance type. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance types>
-- in the /Amazon EC2 User Guide/.
instanceTypeOffering_instanceType :: Lens.Lens' InstanceTypeOffering (Prelude.Maybe InstanceType)
instanceTypeOffering_instanceType = Lens.lens (\InstanceTypeOffering' {instanceType} -> instanceType) (\s@InstanceTypeOffering' {} a -> s {instanceType = a} :: InstanceTypeOffering)

-- | The location type.
instanceTypeOffering_locationType :: Lens.Lens' InstanceTypeOffering (Prelude.Maybe LocationType)
instanceTypeOffering_locationType = Lens.lens (\InstanceTypeOffering' {locationType} -> locationType) (\s@InstanceTypeOffering' {} a -> s {locationType = a} :: InstanceTypeOffering)

-- | The identifier for the location. This depends on the location type. For
-- example, if the location type is @region@, the location is the Region
-- code (for example, @us-east-2@.)
instanceTypeOffering_location :: Lens.Lens' InstanceTypeOffering (Prelude.Maybe Prelude.Text)
instanceTypeOffering_location = Lens.lens (\InstanceTypeOffering' {location} -> location) (\s@InstanceTypeOffering' {} a -> s {location = a} :: InstanceTypeOffering)

instance Prelude.FromXML InstanceTypeOffering where
  parseXML x =
    InstanceTypeOffering'
      Prelude.<$> (x Prelude..@? "instanceType")
      Prelude.<*> (x Prelude..@? "locationType")
      Prelude.<*> (x Prelude..@? "location")

instance Prelude.Hashable InstanceTypeOffering

instance Prelude.NFData InstanceTypeOffering
