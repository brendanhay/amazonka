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
-- Module      : Amazonka.EC2.AssociateSubnetCidrBlock
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a CIDR block with your subnet. You can only associate a
-- single IPv6 CIDR block with your subnet. An IPv6 CIDR block must have a
-- prefix length of \/64.
module Amazonka.EC2.AssociateSubnetCidrBlock
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
    associateSubnetCidrBlockResponse_ipv6CidrBlockAssociation,
    associateSubnetCidrBlockResponse_subnetId,
    associateSubnetCidrBlockResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          AssociateSubnetCidrBlockResponse'
            Prelude.<$> (x Data..@? "ipv6CidrBlockAssociation")
            Prelude.<*> (x Data..@? "subnetId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AssociateSubnetCidrBlock where
  hashWithSalt _salt AssociateSubnetCidrBlock' {..} =
    _salt `Prelude.hashWithSalt` ipv6CidrBlock
      `Prelude.hashWithSalt` subnetId

instance Prelude.NFData AssociateSubnetCidrBlock where
  rnf AssociateSubnetCidrBlock' {..} =
    Prelude.rnf ipv6CidrBlock
      `Prelude.seq` Prelude.rnf subnetId

instance Data.ToHeaders AssociateSubnetCidrBlock where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath AssociateSubnetCidrBlock where
  toPath = Prelude.const "/"

instance Data.ToQuery AssociateSubnetCidrBlock where
  toQuery AssociateSubnetCidrBlock' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("AssociateSubnetCidrBlock" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "Ipv6CidrBlock" Data.=: ipv6CidrBlock,
        "SubnetId" Data.=: subnetId
      ]

-- | /See:/ 'newAssociateSubnetCidrBlockResponse' smart constructor.
data AssociateSubnetCidrBlockResponse = AssociateSubnetCidrBlockResponse'
  { -- | Information about the IPv6 association.
    ipv6CidrBlockAssociation :: Prelude.Maybe SubnetIpv6CidrBlockAssociation,
    -- | The ID of the subnet.
    subnetId :: Prelude.Maybe Prelude.Text,
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
-- 'ipv6CidrBlockAssociation', 'associateSubnetCidrBlockResponse_ipv6CidrBlockAssociation' - Information about the IPv6 association.
--
-- 'subnetId', 'associateSubnetCidrBlockResponse_subnetId' - The ID of the subnet.
--
-- 'httpStatus', 'associateSubnetCidrBlockResponse_httpStatus' - The response's http status code.
newAssociateSubnetCidrBlockResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssociateSubnetCidrBlockResponse
newAssociateSubnetCidrBlockResponse pHttpStatus_ =
  AssociateSubnetCidrBlockResponse'
    { ipv6CidrBlockAssociation =
        Prelude.Nothing,
      subnetId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the IPv6 association.
associateSubnetCidrBlockResponse_ipv6CidrBlockAssociation :: Lens.Lens' AssociateSubnetCidrBlockResponse (Prelude.Maybe SubnetIpv6CidrBlockAssociation)
associateSubnetCidrBlockResponse_ipv6CidrBlockAssociation = Lens.lens (\AssociateSubnetCidrBlockResponse' {ipv6CidrBlockAssociation} -> ipv6CidrBlockAssociation) (\s@AssociateSubnetCidrBlockResponse' {} a -> s {ipv6CidrBlockAssociation = a} :: AssociateSubnetCidrBlockResponse)

-- | The ID of the subnet.
associateSubnetCidrBlockResponse_subnetId :: Lens.Lens' AssociateSubnetCidrBlockResponse (Prelude.Maybe Prelude.Text)
associateSubnetCidrBlockResponse_subnetId = Lens.lens (\AssociateSubnetCidrBlockResponse' {subnetId} -> subnetId) (\s@AssociateSubnetCidrBlockResponse' {} a -> s {subnetId = a} :: AssociateSubnetCidrBlockResponse)

-- | The response's http status code.
associateSubnetCidrBlockResponse_httpStatus :: Lens.Lens' AssociateSubnetCidrBlockResponse Prelude.Int
associateSubnetCidrBlockResponse_httpStatus = Lens.lens (\AssociateSubnetCidrBlockResponse' {httpStatus} -> httpStatus) (\s@AssociateSubnetCidrBlockResponse' {} a -> s {httpStatus = a} :: AssociateSubnetCidrBlockResponse)

instance
  Prelude.NFData
    AssociateSubnetCidrBlockResponse
  where
  rnf AssociateSubnetCidrBlockResponse' {..} =
    Prelude.rnf ipv6CidrBlockAssociation
      `Prelude.seq` Prelude.rnf subnetId
      `Prelude.seq` Prelude.rnf httpStatus
