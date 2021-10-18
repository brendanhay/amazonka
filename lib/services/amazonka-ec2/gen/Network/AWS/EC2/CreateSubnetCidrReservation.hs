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
-- Module      : Network.AWS.EC2.CreateSubnetCidrReservation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a subnet CIDR reservation. For information about subnet CIDR
-- reservations, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/subnet-cidr-reservation.html Subnet CIDR reservations>
-- in the /Amazon Virtual Private Cloud User Guide/.
module Network.AWS.EC2.CreateSubnetCidrReservation
  ( -- * Creating a Request
    CreateSubnetCidrReservation (..),
    newCreateSubnetCidrReservation,

    -- * Request Lenses
    createSubnetCidrReservation_tagSpecifications,
    createSubnetCidrReservation_dryRun,
    createSubnetCidrReservation_description,
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

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateSubnetCidrReservation' smart constructor.
data CreateSubnetCidrReservation = CreateSubnetCidrReservation'
  { -- | The tags to assign to the subnet CIDR reservation.
    tagSpecifications :: Prelude.Maybe [TagSpecification],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The description to assign to the subnet CIDR reservation.
    description :: Prelude.Maybe Prelude.Text,
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
-- 'tagSpecifications', 'createSubnetCidrReservation_tagSpecifications' - The tags to assign to the subnet CIDR reservation.
--
-- 'dryRun', 'createSubnetCidrReservation_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'description', 'createSubnetCidrReservation_description' - The description to assign to the subnet CIDR reservation.
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
      { tagSpecifications =
          Prelude.Nothing,
        dryRun = Prelude.Nothing,
        description = Prelude.Nothing,
        subnetId = pSubnetId_,
        cidr = pCidr_,
        reservationType = pReservationType_
      }

-- | The tags to assign to the subnet CIDR reservation.
createSubnetCidrReservation_tagSpecifications :: Lens.Lens' CreateSubnetCidrReservation (Prelude.Maybe [TagSpecification])
createSubnetCidrReservation_tagSpecifications = Lens.lens (\CreateSubnetCidrReservation' {tagSpecifications} -> tagSpecifications) (\s@CreateSubnetCidrReservation' {} a -> s {tagSpecifications = a} :: CreateSubnetCidrReservation) Prelude.. Lens.mapping Lens._Coerce

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createSubnetCidrReservation_dryRun :: Lens.Lens' CreateSubnetCidrReservation (Prelude.Maybe Prelude.Bool)
createSubnetCidrReservation_dryRun = Lens.lens (\CreateSubnetCidrReservation' {dryRun} -> dryRun) (\s@CreateSubnetCidrReservation' {} a -> s {dryRun = a} :: CreateSubnetCidrReservation)

-- | The description to assign to the subnet CIDR reservation.
createSubnetCidrReservation_description :: Lens.Lens' CreateSubnetCidrReservation (Prelude.Maybe Prelude.Text)
createSubnetCidrReservation_description = Lens.lens (\CreateSubnetCidrReservation' {description} -> description) (\s@CreateSubnetCidrReservation' {} a -> s {description = a} :: CreateSubnetCidrReservation)

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
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          CreateSubnetCidrReservationResponse'
            Prelude.<$> (x Core..@? "subnetCidrReservation")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateSubnetCidrReservation

instance Prelude.NFData CreateSubnetCidrReservation

instance Core.ToHeaders CreateSubnetCidrReservation where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath CreateSubnetCidrReservation where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateSubnetCidrReservation where
  toQuery CreateSubnetCidrReservation' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "CreateSubnetCidrReservation" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        Core.toQuery
          ( Core.toQueryList "TagSpecification"
              Prelude.<$> tagSpecifications
          ),
        "DryRun" Core.=: dryRun,
        "Description" Core.=: description,
        "SubnetId" Core.=: subnetId,
        "Cidr" Core.=: cidr,
        "ReservationType" Core.=: reservationType
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
