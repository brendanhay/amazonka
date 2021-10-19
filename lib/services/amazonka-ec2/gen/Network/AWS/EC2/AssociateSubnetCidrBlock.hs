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
-- Module      : Network.AWS.EC2.AssociateSubnetCidrBlock
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a CIDR block with your subnet. You can only associate a
-- single IPv6 CIDR block with your subnet. An IPv6 CIDR block must have a
-- prefix length of \/64.
module Network.AWS.EC2.AssociateSubnetCidrBlock
  ( -- * Creating a Request
    AssociateSubnetCidrBlock (..),
    newAssociateSubnetCidrBlock,

    -- * Request Lenses
    associateSubnetCidrBlock_ipv6CidrBlock,
    associateSubnetCidrBlock_subnetId,

    -- * Destructuring the Response
    AssociateSubnetCidrBlockResponse (..),
    newAssociateSubnetCidrBlockResponse,

    -- * Response Lenses
    associateSubnetCidrBlockResponse_subnetId,
    associateSubnetCidrBlockResponse_ipv6CidrBlockAssociation,
    associateSubnetCidrBlockResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAssociateSubnetCidrBlock' smart constructor.
data AssociateSubnetCidrBlock = AssociateSubnetCidrBlock'
  { -- | The IPv6 CIDR block for your subnet. The subnet must have a \/64 prefix
    -- length.
    ipv6CidrBlock :: Prelude.Text,
    -- | The ID of your subnet.
    subnetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateSubnetCidrBlock' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipv6CidrBlock', 'associateSubnetCidrBlock_ipv6CidrBlock' - The IPv6 CIDR block for your subnet. The subnet must have a \/64 prefix
-- length.
--
-- 'subnetId', 'associateSubnetCidrBlock_subnetId' - The ID of your subnet.
newAssociateSubnetCidrBlock ::
  -- | 'ipv6CidrBlock'
  Prelude.Text ->
  -- | 'subnetId'
  Prelude.Text ->
  AssociateSubnetCidrBlock
newAssociateSubnetCidrBlock
  pIpv6CidrBlock_
  pSubnetId_ =
    AssociateSubnetCidrBlock'
      { ipv6CidrBlock =
          pIpv6CidrBlock_,
        subnetId = pSubnetId_
      }

-- | The IPv6 CIDR block for your subnet. The subnet must have a \/64 prefix
-- length.
associateSubnetCidrBlock_ipv6CidrBlock :: Lens.Lens' AssociateSubnetCidrBlock Prelude.Text
associateSubnetCidrBlock_ipv6CidrBlock = Lens.lens (\AssociateSubnetCidrBlock' {ipv6CidrBlock} -> ipv6CidrBlock) (\s@AssociateSubnetCidrBlock' {} a -> s {ipv6CidrBlock = a} :: AssociateSubnetCidrBlock)

-- | The ID of your subnet.
associateSubnetCidrBlock_subnetId :: Lens.Lens' AssociateSubnetCidrBlock Prelude.Text
associateSubnetCidrBlock_subnetId = Lens.lens (\AssociateSubnetCidrBlock' {subnetId} -> subnetId) (\s@AssociateSubnetCidrBlock' {} a -> s {subnetId = a} :: AssociateSubnetCidrBlock)

instance Core.AWSRequest AssociateSubnetCidrBlock where
  type
    AWSResponse AssociateSubnetCidrBlock =
      AssociateSubnetCidrBlockResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          AssociateSubnetCidrBlockResponse'
            Prelude.<$> (x Core..@? "subnetId")
            Prelude.<*> (x Core..@? "ipv6CidrBlockAssociation")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AssociateSubnetCidrBlock

instance Prelude.NFData AssociateSubnetCidrBlock

instance Core.ToHeaders AssociateSubnetCidrBlock where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath AssociateSubnetCidrBlock where
  toPath = Prelude.const "/"

instance Core.ToQuery AssociateSubnetCidrBlock where
  toQuery AssociateSubnetCidrBlock' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("AssociateSubnetCidrBlock" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "Ipv6CidrBlock" Core.=: ipv6CidrBlock,
        "SubnetId" Core.=: subnetId
      ]

-- | /See:/ 'newAssociateSubnetCidrBlockResponse' smart constructor.
data AssociateSubnetCidrBlockResponse = AssociateSubnetCidrBlockResponse'
  { -- | The ID of the subnet.
    subnetId :: Prelude.Maybe Prelude.Text,
    -- | Information about the IPv6 CIDR block association.
    ipv6CidrBlockAssociation :: Prelude.Maybe SubnetIpv6CidrBlockAssociation,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateSubnetCidrBlockResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'subnetId', 'associateSubnetCidrBlockResponse_subnetId' - The ID of the subnet.
--
-- 'ipv6CidrBlockAssociation', 'associateSubnetCidrBlockResponse_ipv6CidrBlockAssociation' - Information about the IPv6 CIDR block association.
--
-- 'httpStatus', 'associateSubnetCidrBlockResponse_httpStatus' - The response's http status code.
newAssociateSubnetCidrBlockResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssociateSubnetCidrBlockResponse
newAssociateSubnetCidrBlockResponse pHttpStatus_ =
  AssociateSubnetCidrBlockResponse'
    { subnetId =
        Prelude.Nothing,
      ipv6CidrBlockAssociation =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the subnet.
associateSubnetCidrBlockResponse_subnetId :: Lens.Lens' AssociateSubnetCidrBlockResponse (Prelude.Maybe Prelude.Text)
associateSubnetCidrBlockResponse_subnetId = Lens.lens (\AssociateSubnetCidrBlockResponse' {subnetId} -> subnetId) (\s@AssociateSubnetCidrBlockResponse' {} a -> s {subnetId = a} :: AssociateSubnetCidrBlockResponse)

-- | Information about the IPv6 CIDR block association.
associateSubnetCidrBlockResponse_ipv6CidrBlockAssociation :: Lens.Lens' AssociateSubnetCidrBlockResponse (Prelude.Maybe SubnetIpv6CidrBlockAssociation)
associateSubnetCidrBlockResponse_ipv6CidrBlockAssociation = Lens.lens (\AssociateSubnetCidrBlockResponse' {ipv6CidrBlockAssociation} -> ipv6CidrBlockAssociation) (\s@AssociateSubnetCidrBlockResponse' {} a -> s {ipv6CidrBlockAssociation = a} :: AssociateSubnetCidrBlockResponse)

-- | The response's http status code.
associateSubnetCidrBlockResponse_httpStatus :: Lens.Lens' AssociateSubnetCidrBlockResponse Prelude.Int
associateSubnetCidrBlockResponse_httpStatus = Lens.lens (\AssociateSubnetCidrBlockResponse' {httpStatus} -> httpStatus) (\s@AssociateSubnetCidrBlockResponse' {} a -> s {httpStatus = a} :: AssociateSubnetCidrBlockResponse)

instance
  Prelude.NFData
    AssociateSubnetCidrBlockResponse
