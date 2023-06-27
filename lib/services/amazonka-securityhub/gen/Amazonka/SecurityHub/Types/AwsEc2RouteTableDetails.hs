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
-- Module      : Amazonka.SecurityHub.Types.AwsEc2RouteTableDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEc2RouteTableDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AssociationSetDetails
import Amazonka.SecurityHub.Types.PropagatingVgwSetDetails
import Amazonka.SecurityHub.Types.RouteSetDetails

-- | Provides details about a route table for the specified VPC.
--
-- /See:/ 'newAwsEc2RouteTableDetails' smart constructor.
data AwsEc2RouteTableDetails = AwsEc2RouteTableDetails'
  { -- | The associations between a route table and one or more subnets or a
    -- gateway.
    associationSet :: Prelude.Maybe [AssociationSetDetails],
    -- | The ID of the Amazon Web Services account that owns the route table.
    ownerId :: Prelude.Maybe Prelude.Text,
    -- | Describes a virtual private gateway propagating route.
    propagatingVgwSet :: Prelude.Maybe [PropagatingVgwSetDetails],
    -- | The routes in the route table.
    routeSet :: Prelude.Maybe [RouteSetDetails],
    -- | The ID of the route table.
    routeTableId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the virtual private cloud (VPC).
    vpcId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEc2RouteTableDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associationSet', 'awsEc2RouteTableDetails_associationSet' - The associations between a route table and one or more subnets or a
-- gateway.
--
-- 'ownerId', 'awsEc2RouteTableDetails_ownerId' - The ID of the Amazon Web Services account that owns the route table.
--
-- 'propagatingVgwSet', 'awsEc2RouteTableDetails_propagatingVgwSet' - Describes a virtual private gateway propagating route.
--
-- 'routeSet', 'awsEc2RouteTableDetails_routeSet' - The routes in the route table.
--
-- 'routeTableId', 'awsEc2RouteTableDetails_routeTableId' - The ID of the route table.
--
-- 'vpcId', 'awsEc2RouteTableDetails_vpcId' - The ID of the virtual private cloud (VPC).
newAwsEc2RouteTableDetails ::
  AwsEc2RouteTableDetails
newAwsEc2RouteTableDetails =
  AwsEc2RouteTableDetails'
    { associationSet =
        Prelude.Nothing,
      ownerId = Prelude.Nothing,
      propagatingVgwSet = Prelude.Nothing,
      routeSet = Prelude.Nothing,
      routeTableId = Prelude.Nothing,
      vpcId = Prelude.Nothing
    }

-- | The associations between a route table and one or more subnets or a
-- gateway.
awsEc2RouteTableDetails_associationSet :: Lens.Lens' AwsEc2RouteTableDetails (Prelude.Maybe [AssociationSetDetails])
awsEc2RouteTableDetails_associationSet = Lens.lens (\AwsEc2RouteTableDetails' {associationSet} -> associationSet) (\s@AwsEc2RouteTableDetails' {} a -> s {associationSet = a} :: AwsEc2RouteTableDetails) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the Amazon Web Services account that owns the route table.
awsEc2RouteTableDetails_ownerId :: Lens.Lens' AwsEc2RouteTableDetails (Prelude.Maybe Prelude.Text)
awsEc2RouteTableDetails_ownerId = Lens.lens (\AwsEc2RouteTableDetails' {ownerId} -> ownerId) (\s@AwsEc2RouteTableDetails' {} a -> s {ownerId = a} :: AwsEc2RouteTableDetails)

-- | Describes a virtual private gateway propagating route.
awsEc2RouteTableDetails_propagatingVgwSet :: Lens.Lens' AwsEc2RouteTableDetails (Prelude.Maybe [PropagatingVgwSetDetails])
awsEc2RouteTableDetails_propagatingVgwSet = Lens.lens (\AwsEc2RouteTableDetails' {propagatingVgwSet} -> propagatingVgwSet) (\s@AwsEc2RouteTableDetails' {} a -> s {propagatingVgwSet = a} :: AwsEc2RouteTableDetails) Prelude.. Lens.mapping Lens.coerced

-- | The routes in the route table.
awsEc2RouteTableDetails_routeSet :: Lens.Lens' AwsEc2RouteTableDetails (Prelude.Maybe [RouteSetDetails])
awsEc2RouteTableDetails_routeSet = Lens.lens (\AwsEc2RouteTableDetails' {routeSet} -> routeSet) (\s@AwsEc2RouteTableDetails' {} a -> s {routeSet = a} :: AwsEc2RouteTableDetails) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the route table.
awsEc2RouteTableDetails_routeTableId :: Lens.Lens' AwsEc2RouteTableDetails (Prelude.Maybe Prelude.Text)
awsEc2RouteTableDetails_routeTableId = Lens.lens (\AwsEc2RouteTableDetails' {routeTableId} -> routeTableId) (\s@AwsEc2RouteTableDetails' {} a -> s {routeTableId = a} :: AwsEc2RouteTableDetails)

-- | The ID of the virtual private cloud (VPC).
awsEc2RouteTableDetails_vpcId :: Lens.Lens' AwsEc2RouteTableDetails (Prelude.Maybe Prelude.Text)
awsEc2RouteTableDetails_vpcId = Lens.lens (\AwsEc2RouteTableDetails' {vpcId} -> vpcId) (\s@AwsEc2RouteTableDetails' {} a -> s {vpcId = a} :: AwsEc2RouteTableDetails)

instance Data.FromJSON AwsEc2RouteTableDetails where
  parseJSON =
    Data.withObject
      "AwsEc2RouteTableDetails"
      ( \x ->
          AwsEc2RouteTableDetails'
            Prelude.<$> (x Data..:? "AssociationSet" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "OwnerId")
            Prelude.<*> ( x
                            Data..:? "PropagatingVgwSet"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "RouteSet" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "RouteTableId")
            Prelude.<*> (x Data..:? "VpcId")
      )

instance Prelude.Hashable AwsEc2RouteTableDetails where
  hashWithSalt _salt AwsEc2RouteTableDetails' {..} =
    _salt
      `Prelude.hashWithSalt` associationSet
      `Prelude.hashWithSalt` ownerId
      `Prelude.hashWithSalt` propagatingVgwSet
      `Prelude.hashWithSalt` routeSet
      `Prelude.hashWithSalt` routeTableId
      `Prelude.hashWithSalt` vpcId

instance Prelude.NFData AwsEc2RouteTableDetails where
  rnf AwsEc2RouteTableDetails' {..} =
    Prelude.rnf associationSet
      `Prelude.seq` Prelude.rnf ownerId
      `Prelude.seq` Prelude.rnf propagatingVgwSet
      `Prelude.seq` Prelude.rnf routeSet
      `Prelude.seq` Prelude.rnf routeTableId
      `Prelude.seq` Prelude.rnf vpcId

instance Data.ToJSON AwsEc2RouteTableDetails where
  toJSON AwsEc2RouteTableDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AssociationSet" Data..=)
              Prelude.<$> associationSet,
            ("OwnerId" Data..=) Prelude.<$> ownerId,
            ("PropagatingVgwSet" Data..=)
              Prelude.<$> propagatingVgwSet,
            ("RouteSet" Data..=) Prelude.<$> routeSet,
            ("RouteTableId" Data..=) Prelude.<$> routeTableId,
            ("VpcId" Data..=) Prelude.<$> vpcId
          ]
      )
