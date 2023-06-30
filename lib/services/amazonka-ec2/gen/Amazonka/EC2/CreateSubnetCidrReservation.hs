{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.EC2.CreateSubnetCidrReservation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a subnet CIDR reservation. For information about subnet CIDR
-- reservations, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/subnet-cidr-reservation.html Subnet CIDR reservations>
-- in the /Amazon Virtual Private Cloud User Guide/.
module Amazonka.EC2.CreateSubnetCidrReservation
  ( -- * Creating a Request
    CreateSubnetCidrReservation (..),
    newCreateSubnetCidrReservation,

    -- * Request Lenses
    createSubnetCidrReservation_description,
    createSubnetCidrReservation_dryRun,
    createSubnetCidrReservation_tagSpecifications,
    createSubnetCidrReservation_subnetId,
    createSubnetCidrReservation_cidr,
    createSubnetCidrReservation_reservationType,

    -- * Destructuring the Response
    CreateSubnetCidrReservationResponse (..),
    newCreateSubnetCidrReservationResponse,

    -- * Response Lenses
    createSubnetCidrReservationResponse_subnetCidrReservation,
    createSubnetCidrReservationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateSubnetCidrReservation' smart constructor.
data CreateSubnetCidrReservation = CreateSubnetCidrReservation'
  { -- | The description to assign to the subnet CIDR reservation.
    description :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The tags to assign to the subnet CIDR reservation.
    tagSpecifications :: Prelude.Maybe [TagSpecification],
    -- | The ID of the subnet.
    subnetId :: Prelude.Text,
    -- | The IPv4 or IPV6 CIDR range to reserve.
    cidr :: Prelude.Text,
    -- | The type of reservation.
    --
    -- The following are valid values:
    --
    -- -   @prefix@: The Amazon EC2 Prefix Delegation feature assigns the IP
    --     addresses to network interfaces that are associated with an
    --     instance. For information about Prefix Delegation, see
    --     <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-prefix-delegation.html Prefix Delegation for Amazon EC2 network interfaces>
    --     in the /Amazon Elastic Compute Cloud User Guide/.
    --
    -- -   @explicit@: You manually assign the IP addresses to resources that
    --     reside in your subnet.
    reservationType :: SubnetCidrReservationType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSubnetCidrReservation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'createSubnetCidrReservation_description' - The description to assign to the subnet CIDR reservation.
--
-- 'dryRun', 'createSubnetCidrReservation_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'tagSpecifications', 'createSubnetCidrReservation_tagSpecifications' - The tags to assign to the subnet CIDR reservation.
--
-- 'subnetId', 'createSubnetCidrReservation_subnetId' - The ID of the subnet.
--
-- 'cidr', 'createSubnetCidrReservation_cidr' - The IPv4 or IPV6 CIDR range to reserve.
--
-- 'reservationType', 'createSubnetCidrReservation_reservationType' - The type of reservation.
--
-- The following are valid values:
--
-- -   @prefix@: The Amazon EC2 Prefix Delegation feature assigns the IP
--     addresses to network interfaces that are associated with an
--     instance. For information about Prefix Delegation, see
--     <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-prefix-delegation.html Prefix Delegation for Amazon EC2 network interfaces>
--     in the /Amazon Elastic Compute Cloud User Guide/.
--
-- -   @explicit@: You manually assign the IP addresses to resources that
--     reside in your subnet.
newCreateSubnetCidrReservation ::
  -- | 'subnetId'
  Prelude.Text ->
  -- | 'cidr'
  Prelude.Text ->
  -- | 'reservationType'
  SubnetCidrReservationType ->
  CreateSubnetCidrReservation
newCreateSubnetCidrReservation
  pSubnetId_
  pCidr_
  pReservationType_ =
    CreateSubnetCidrReservation'
      { description =
          Prelude.Nothing,
        dryRun = Prelude.Nothing,
        tagSpecifications = Prelude.Nothing,
        subnetId = pSubnetId_,
        cidr = pCidr_,
        reservationType = pReservationType_
      }

-- | The description to assign to the subnet CIDR reservation.
createSubnetCidrReservation_description :: Lens.Lens' CreateSubnetCidrReservation (Prelude.Maybe Prelude.Text)
createSubnetCidrReservation_description = Lens.lens (\CreateSubnetCidrReservation' {description} -> description) (\s@CreateSubnetCidrReservation' {} a -> s {description = a} :: CreateSubnetCidrReservation)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createSubnetCidrReservation_dryRun :: Lens.Lens' CreateSubnetCidrReservation (Prelude.Maybe Prelude.Bool)
createSubnetCidrReservation_dryRun = Lens.lens (\CreateSubnetCidrReservation' {dryRun} -> dryRun) (\s@CreateSubnetCidrReservation' {} a -> s {dryRun = a} :: CreateSubnetCidrReservation)

-- | The tags to assign to the subnet CIDR reservation.
createSubnetCidrReservation_tagSpecifications :: Lens.Lens' CreateSubnetCidrReservation (Prelude.Maybe [TagSpecification])
createSubnetCidrReservation_tagSpecifications = Lens.lens (\CreateSubnetCidrReservation' {tagSpecifications} -> tagSpecifications) (\s@CreateSubnetCidrReservation' {} a -> s {tagSpecifications = a} :: CreateSubnetCidrReservation) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the subnet.
createSubnetCidrReservation_subnetId :: Lens.Lens' CreateSubnetCidrReservation Prelude.Text
createSubnetCidrReservation_subnetId = Lens.lens (\CreateSubnetCidrReservation' {subnetId} -> subnetId) (\s@CreateSubnetCidrReservation' {} a -> s {subnetId = a} :: CreateSubnetCidrReservation)

-- | The IPv4 or IPV6 CIDR range to reserve.
createSubnetCidrReservation_cidr :: Lens.Lens' CreateSubnetCidrReservation Prelude.Text
createSubnetCidrReservation_cidr = Lens.lens (\CreateSubnetCidrReservation' {cidr} -> cidr) (\s@CreateSubnetCidrReservation' {} a -> s {cidr = a} :: CreateSubnetCidrReservation)

-- | The type of reservation.
--
-- The following are valid values:
--
-- -   @prefix@: The Amazon EC2 Prefix Delegation feature assigns the IP
--     addresses to network interfaces that are associated with an
--     instance. For information about Prefix Delegation, see
--     <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-prefix-delegation.html Prefix Delegation for Amazon EC2 network interfaces>
--     in the /Amazon Elastic Compute Cloud User Guide/.
--
-- -   @explicit@: You manually assign the IP addresses to resources that
--     reside in your subnet.
createSubnetCidrReservation_reservationType :: Lens.Lens' CreateSubnetCidrReservation SubnetCidrReservationType
createSubnetCidrReservation_reservationType = Lens.lens (\CreateSubnetCidrReservation' {reservationType} -> reservationType) (\s@CreateSubnetCidrReservation' {} a -> s {reservationType = a} :: CreateSubnetCidrReservation)

instance Core.AWSRequest CreateSubnetCidrReservation where
  type
    AWSResponse CreateSubnetCidrReservation =
      CreateSubnetCidrReservationResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          CreateSubnetCidrReservationResponse'
            Prelude.<$> (x Data..@? "subnetCidrReservation")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateSubnetCidrReservation where
  hashWithSalt _salt CreateSubnetCidrReservation' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` tagSpecifications
      `Prelude.hashWithSalt` subnetId
      `Prelude.hashWithSalt` cidr
      `Prelude.hashWithSalt` reservationType

instance Prelude.NFData CreateSubnetCidrReservation where
  rnf CreateSubnetCidrReservation' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf tagSpecifications
      `Prelude.seq` Prelude.rnf subnetId
      `Prelude.seq` Prelude.rnf cidr
      `Prelude.seq` Prelude.rnf reservationType

instance Data.ToHeaders CreateSubnetCidrReservation where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CreateSubnetCidrReservation where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateSubnetCidrReservation where
  toQuery CreateSubnetCidrReservation' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "CreateSubnetCidrReservation" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "Description" Data.=: description,
        "DryRun" Data.=: dryRun,
        Data.toQuery
          ( Data.toQueryList "TagSpecification"
              Prelude.<$> tagSpecifications
          ),
        "SubnetId" Data.=: subnetId,
        "Cidr" Data.=: cidr,
        "ReservationType" Data.=: reservationType
      ]

-- | /See:/ 'newCreateSubnetCidrReservationResponse' smart constructor.
data CreateSubnetCidrReservationResponse = CreateSubnetCidrReservationResponse'
  { -- | Information about the created subnet CIDR reservation.
    subnetCidrReservation :: Prelude.Maybe SubnetCidrReservation,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSubnetCidrReservationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'subnetCidrReservation', 'createSubnetCidrReservationResponse_subnetCidrReservation' - Information about the created subnet CIDR reservation.
--
-- 'httpStatus', 'createSubnetCidrReservationResponse_httpStatus' - The response's http status code.
newCreateSubnetCidrReservationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateSubnetCidrReservationResponse
newCreateSubnetCidrReservationResponse pHttpStatus_ =
  CreateSubnetCidrReservationResponse'
    { subnetCidrReservation =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the created subnet CIDR reservation.
createSubnetCidrReservationResponse_subnetCidrReservation :: Lens.Lens' CreateSubnetCidrReservationResponse (Prelude.Maybe SubnetCidrReservation)
createSubnetCidrReservationResponse_subnetCidrReservation = Lens.lens (\CreateSubnetCidrReservationResponse' {subnetCidrReservation} -> subnetCidrReservation) (\s@CreateSubnetCidrReservationResponse' {} a -> s {subnetCidrReservation = a} :: CreateSubnetCidrReservationResponse)

-- | The response's http status code.
createSubnetCidrReservationResponse_httpStatus :: Lens.Lens' CreateSubnetCidrReservationResponse Prelude.Int
createSubnetCidrReservationResponse_httpStatus = Lens.lens (\CreateSubnetCidrReservationResponse' {httpStatus} -> httpStatus) (\s@CreateSubnetCidrReservationResponse' {} a -> s {httpStatus = a} :: CreateSubnetCidrReservationResponse)

instance
  Prelude.NFData
    CreateSubnetCidrReservationResponse
  where
  rnf CreateSubnetCidrReservationResponse' {..} =
    Prelude.rnf subnetCidrReservation
      `Prelude.seq` Prelude.rnf httpStatus
