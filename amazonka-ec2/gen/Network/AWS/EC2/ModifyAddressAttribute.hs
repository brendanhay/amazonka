{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.EC2.ModifyAddressAttribute
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies an attribute of the specified Elastic IP address. For
-- requirements, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html#Using_Elastic_Addressing_Reverse_DNS Using reverse DNS for email applications>.
module Network.AWS.EC2.ModifyAddressAttribute
  ( -- * Creating a Request
    ModifyAddressAttribute (..),
    newModifyAddressAttribute,

    -- * Request Lenses
    modifyAddressAttribute_dryRun,
    modifyAddressAttribute_domainName,
    modifyAddressAttribute_allocationId,

    -- * Destructuring the Response
    ModifyAddressAttributeResponse (..),
    newModifyAddressAttributeResponse,

    -- * Response Lenses
    modifyAddressAttributeResponse_address,
    modifyAddressAttributeResponse_httpStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newModifyAddressAttribute' smart constructor.
data ModifyAddressAttribute = ModifyAddressAttribute'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The domain name to modify for the IP address.
    domainName :: Prelude.Maybe Prelude.Text,
    -- | [EC2-VPC] The allocation ID.
    allocationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ModifyAddressAttribute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'modifyAddressAttribute_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'domainName', 'modifyAddressAttribute_domainName' - The domain name to modify for the IP address.
--
-- 'allocationId', 'modifyAddressAttribute_allocationId' - [EC2-VPC] The allocation ID.
newModifyAddressAttribute ::
  -- | 'allocationId'
  Prelude.Text ->
  ModifyAddressAttribute
newModifyAddressAttribute pAllocationId_ =
  ModifyAddressAttribute'
    { dryRun = Prelude.Nothing,
      domainName = Prelude.Nothing,
      allocationId = pAllocationId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
modifyAddressAttribute_dryRun :: Lens.Lens' ModifyAddressAttribute (Prelude.Maybe Prelude.Bool)
modifyAddressAttribute_dryRun = Lens.lens (\ModifyAddressAttribute' {dryRun} -> dryRun) (\s@ModifyAddressAttribute' {} a -> s {dryRun = a} :: ModifyAddressAttribute)

-- | The domain name to modify for the IP address.
modifyAddressAttribute_domainName :: Lens.Lens' ModifyAddressAttribute (Prelude.Maybe Prelude.Text)
modifyAddressAttribute_domainName = Lens.lens (\ModifyAddressAttribute' {domainName} -> domainName) (\s@ModifyAddressAttribute' {} a -> s {domainName = a} :: ModifyAddressAttribute)

-- | [EC2-VPC] The allocation ID.
modifyAddressAttribute_allocationId :: Lens.Lens' ModifyAddressAttribute Prelude.Text
modifyAddressAttribute_allocationId = Lens.lens (\ModifyAddressAttribute' {allocationId} -> allocationId) (\s@ModifyAddressAttribute' {} a -> s {allocationId = a} :: ModifyAddressAttribute)

instance Prelude.AWSRequest ModifyAddressAttribute where
  type
    Rs ModifyAddressAttribute =
      ModifyAddressAttributeResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          ModifyAddressAttributeResponse'
            Prelude.<$> (x Prelude..@? "address")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ModifyAddressAttribute

instance Prelude.NFData ModifyAddressAttribute

instance Prelude.ToHeaders ModifyAddressAttribute where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath ModifyAddressAttribute where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ModifyAddressAttribute where
  toQuery ModifyAddressAttribute' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("ModifyAddressAttribute" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Prelude.=: dryRun,
        "DomainName" Prelude.=: domainName,
        "AllocationId" Prelude.=: allocationId
      ]

-- | /See:/ 'newModifyAddressAttributeResponse' smart constructor.
data ModifyAddressAttributeResponse = ModifyAddressAttributeResponse'
  { -- | Information about the Elastic IP address.
    address :: Prelude.Maybe AddressAttribute,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ModifyAddressAttributeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'address', 'modifyAddressAttributeResponse_address' - Information about the Elastic IP address.
--
-- 'httpStatus', 'modifyAddressAttributeResponse_httpStatus' - The response's http status code.
newModifyAddressAttributeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ModifyAddressAttributeResponse
newModifyAddressAttributeResponse pHttpStatus_ =
  ModifyAddressAttributeResponse'
    { address =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the Elastic IP address.
modifyAddressAttributeResponse_address :: Lens.Lens' ModifyAddressAttributeResponse (Prelude.Maybe AddressAttribute)
modifyAddressAttributeResponse_address = Lens.lens (\ModifyAddressAttributeResponse' {address} -> address) (\s@ModifyAddressAttributeResponse' {} a -> s {address = a} :: ModifyAddressAttributeResponse)

-- | The response's http status code.
modifyAddressAttributeResponse_httpStatus :: Lens.Lens' ModifyAddressAttributeResponse Prelude.Int
modifyAddressAttributeResponse_httpStatus = Lens.lens (\ModifyAddressAttributeResponse' {httpStatus} -> httpStatus) (\s@ModifyAddressAttributeResponse' {} a -> s {httpStatus = a} :: ModifyAddressAttributeResponse)

instance
  Prelude.NFData
    ModifyAddressAttributeResponse
