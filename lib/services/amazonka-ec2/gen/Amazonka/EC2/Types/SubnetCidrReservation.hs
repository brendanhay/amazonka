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
-- Module      : Amazonka.EC2.Types.SubnetCidrReservation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.SubnetCidrReservation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.SubnetCidrReservationType
import Amazonka.EC2.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | Describes a subnet CIDR reservation.
--
-- /See:/ 'newSubnetCidrReservation' smart constructor.
data SubnetCidrReservation = SubnetCidrReservation'
  { -- | The CIDR that has been reserved.
    cidr :: Prelude.Maybe Prelude.Text,
    -- | The description assigned to the subnet CIDR reservation.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ID of the account that owns the subnet CIDR reservation.
    ownerId :: Prelude.Maybe Prelude.Text,
    -- | The type of reservation.
    reservationType :: Prelude.Maybe SubnetCidrReservationType,
    -- | The ID of the subnet CIDR reservation.
    subnetCidrReservationId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the subnet.
    subnetId :: Prelude.Maybe Prelude.Text,
    -- | The tags assigned to the subnet CIDR reservation.
    tags :: Prelude.Maybe [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SubnetCidrReservation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cidr', 'subnetCidrReservation_cidr' - The CIDR that has been reserved.
--
-- 'description', 'subnetCidrReservation_description' - The description assigned to the subnet CIDR reservation.
--
-- 'ownerId', 'subnetCidrReservation_ownerId' - The ID of the account that owns the subnet CIDR reservation.
--
-- 'reservationType', 'subnetCidrReservation_reservationType' - The type of reservation.
--
-- 'subnetCidrReservationId', 'subnetCidrReservation_subnetCidrReservationId' - The ID of the subnet CIDR reservation.
--
-- 'subnetId', 'subnetCidrReservation_subnetId' - The ID of the subnet.
--
-- 'tags', 'subnetCidrReservation_tags' - The tags assigned to the subnet CIDR reservation.
newSubnetCidrReservation ::
  SubnetCidrReservation
newSubnetCidrReservation =
  SubnetCidrReservation'
    { cidr = Prelude.Nothing,
      description = Prelude.Nothing,
      ownerId = Prelude.Nothing,
      reservationType = Prelude.Nothing,
      subnetCidrReservationId = Prelude.Nothing,
      subnetId = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The CIDR that has been reserved.
subnetCidrReservation_cidr :: Lens.Lens' SubnetCidrReservation (Prelude.Maybe Prelude.Text)
subnetCidrReservation_cidr = Lens.lens (\SubnetCidrReservation' {cidr} -> cidr) (\s@SubnetCidrReservation' {} a -> s {cidr = a} :: SubnetCidrReservation)

-- | The description assigned to the subnet CIDR reservation.
subnetCidrReservation_description :: Lens.Lens' SubnetCidrReservation (Prelude.Maybe Prelude.Text)
subnetCidrReservation_description = Lens.lens (\SubnetCidrReservation' {description} -> description) (\s@SubnetCidrReservation' {} a -> s {description = a} :: SubnetCidrReservation)

-- | The ID of the account that owns the subnet CIDR reservation.
subnetCidrReservation_ownerId :: Lens.Lens' SubnetCidrReservation (Prelude.Maybe Prelude.Text)
subnetCidrReservation_ownerId = Lens.lens (\SubnetCidrReservation' {ownerId} -> ownerId) (\s@SubnetCidrReservation' {} a -> s {ownerId = a} :: SubnetCidrReservation)

-- | The type of reservation.
subnetCidrReservation_reservationType :: Lens.Lens' SubnetCidrReservation (Prelude.Maybe SubnetCidrReservationType)
subnetCidrReservation_reservationType = Lens.lens (\SubnetCidrReservation' {reservationType} -> reservationType) (\s@SubnetCidrReservation' {} a -> s {reservationType = a} :: SubnetCidrReservation)

-- | The ID of the subnet CIDR reservation.
subnetCidrReservation_subnetCidrReservationId :: Lens.Lens' SubnetCidrReservation (Prelude.Maybe Prelude.Text)
subnetCidrReservation_subnetCidrReservationId = Lens.lens (\SubnetCidrReservation' {subnetCidrReservationId} -> subnetCidrReservationId) (\s@SubnetCidrReservation' {} a -> s {subnetCidrReservationId = a} :: SubnetCidrReservation)

-- | The ID of the subnet.
subnetCidrReservation_subnetId :: Lens.Lens' SubnetCidrReservation (Prelude.Maybe Prelude.Text)
subnetCidrReservation_subnetId = Lens.lens (\SubnetCidrReservation' {subnetId} -> subnetId) (\s@SubnetCidrReservation' {} a -> s {subnetId = a} :: SubnetCidrReservation)

-- | The tags assigned to the subnet CIDR reservation.
subnetCidrReservation_tags :: Lens.Lens' SubnetCidrReservation (Prelude.Maybe [Tag])
subnetCidrReservation_tags = Lens.lens (\SubnetCidrReservation' {tags} -> tags) (\s@SubnetCidrReservation' {} a -> s {tags = a} :: SubnetCidrReservation) Prelude.. Lens.mapping Lens.coerced

instance Data.FromXML SubnetCidrReservation where
  parseXML x =
    SubnetCidrReservation'
      Prelude.<$> (x Data..@? "cidr")
      Prelude.<*> (x Data..@? "description")
      Prelude.<*> (x Data..@? "ownerId")
      Prelude.<*> (x Data..@? "reservationType")
      Prelude.<*> (x Data..@? "subnetCidrReservationId")
      Prelude.<*> (x Data..@? "subnetId")
      Prelude.<*> ( x Data..@? "tagSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )

instance Prelude.Hashable SubnetCidrReservation where
  hashWithSalt _salt SubnetCidrReservation' {..} =
    _salt
      `Prelude.hashWithSalt` cidr
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` ownerId
      `Prelude.hashWithSalt` reservationType
      `Prelude.hashWithSalt` subnetCidrReservationId
      `Prelude.hashWithSalt` subnetId
      `Prelude.hashWithSalt` tags

instance Prelude.NFData SubnetCidrReservation where
  rnf SubnetCidrReservation' {..} =
    Prelude.rnf cidr `Prelude.seq`
      Prelude.rnf description `Prelude.seq`
        Prelude.rnf ownerId `Prelude.seq`
          Prelude.rnf reservationType `Prelude.seq`
            Prelude.rnf subnetCidrReservationId `Prelude.seq`
              Prelude.rnf subnetId `Prelude.seq`
                Prelude.rnf tags
