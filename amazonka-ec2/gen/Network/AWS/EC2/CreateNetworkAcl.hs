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
-- Module      : Network.AWS.EC2.CreateNetworkAcl
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a network ACL in a VPC. Network ACLs provide an optional layer
-- of security (in addition to security groups) for the instances in your
-- VPC.
--
-- For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_ACLs.html Network ACLs>
-- in the /Amazon Virtual Private Cloud User Guide/.
module Network.AWS.EC2.CreateNetworkAcl
  ( -- * Creating a Request
    CreateNetworkAcl (..),
    newCreateNetworkAcl,

    -- * Request Lenses
    createNetworkAcl_tagSpecifications,
    createNetworkAcl_dryRun,
    createNetworkAcl_vpcId,

    -- * Destructuring the Response
    CreateNetworkAclResponse (..),
    newCreateNetworkAclResponse,

    -- * Response Lenses
    createNetworkAclResponse_networkAcl,
    createNetworkAclResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateNetworkAcl' smart constructor.
data CreateNetworkAcl = CreateNetworkAcl'
  { -- | The tags to assign to the network ACL.
    tagSpecifications :: Core.Maybe [TagSpecification],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The ID of the VPC.
    vpcId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateNetworkAcl' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tagSpecifications', 'createNetworkAcl_tagSpecifications' - The tags to assign to the network ACL.
--
-- 'dryRun', 'createNetworkAcl_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'vpcId', 'createNetworkAcl_vpcId' - The ID of the VPC.
newCreateNetworkAcl ::
  -- | 'vpcId'
  Core.Text ->
  CreateNetworkAcl
newCreateNetworkAcl pVpcId_ =
  CreateNetworkAcl'
    { tagSpecifications = Core.Nothing,
      dryRun = Core.Nothing,
      vpcId = pVpcId_
    }

-- | The tags to assign to the network ACL.
createNetworkAcl_tagSpecifications :: Lens.Lens' CreateNetworkAcl (Core.Maybe [TagSpecification])
createNetworkAcl_tagSpecifications = Lens.lens (\CreateNetworkAcl' {tagSpecifications} -> tagSpecifications) (\s@CreateNetworkAcl' {} a -> s {tagSpecifications = a} :: CreateNetworkAcl) Core.. Lens.mapping Lens._Coerce

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createNetworkAcl_dryRun :: Lens.Lens' CreateNetworkAcl (Core.Maybe Core.Bool)
createNetworkAcl_dryRun = Lens.lens (\CreateNetworkAcl' {dryRun} -> dryRun) (\s@CreateNetworkAcl' {} a -> s {dryRun = a} :: CreateNetworkAcl)

-- | The ID of the VPC.
createNetworkAcl_vpcId :: Lens.Lens' CreateNetworkAcl Core.Text
createNetworkAcl_vpcId = Lens.lens (\CreateNetworkAcl' {vpcId} -> vpcId) (\s@CreateNetworkAcl' {} a -> s {vpcId = a} :: CreateNetworkAcl)

instance Core.AWSRequest CreateNetworkAcl where
  type
    AWSResponse CreateNetworkAcl =
      CreateNetworkAclResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          CreateNetworkAclResponse'
            Core.<$> (x Core..@? "networkAcl")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateNetworkAcl

instance Core.NFData CreateNetworkAcl

instance Core.ToHeaders CreateNetworkAcl where
  toHeaders = Core.const Core.mempty

instance Core.ToPath CreateNetworkAcl where
  toPath = Core.const "/"

instance Core.ToQuery CreateNetworkAcl where
  toQuery CreateNetworkAcl' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("CreateNetworkAcl" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        Core.toQuery
          ( Core.toQueryList "TagSpecification"
              Core.<$> tagSpecifications
          ),
        "DryRun" Core.=: dryRun,
        "VpcId" Core.=: vpcId
      ]

-- | /See:/ 'newCreateNetworkAclResponse' smart constructor.
data CreateNetworkAclResponse = CreateNetworkAclResponse'
  { -- | Information about the network ACL.
    networkAcl :: Core.Maybe NetworkAcl,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateNetworkAclResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'networkAcl', 'createNetworkAclResponse_networkAcl' - Information about the network ACL.
--
-- 'httpStatus', 'createNetworkAclResponse_httpStatus' - The response's http status code.
newCreateNetworkAclResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateNetworkAclResponse
newCreateNetworkAclResponse pHttpStatus_ =
  CreateNetworkAclResponse'
    { networkAcl =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the network ACL.
createNetworkAclResponse_networkAcl :: Lens.Lens' CreateNetworkAclResponse (Core.Maybe NetworkAcl)
createNetworkAclResponse_networkAcl = Lens.lens (\CreateNetworkAclResponse' {networkAcl} -> networkAcl) (\s@CreateNetworkAclResponse' {} a -> s {networkAcl = a} :: CreateNetworkAclResponse)

-- | The response's http status code.
createNetworkAclResponse_httpStatus :: Lens.Lens' CreateNetworkAclResponse Core.Int
createNetworkAclResponse_httpStatus = Lens.lens (\CreateNetworkAclResponse' {httpStatus} -> httpStatus) (\s@CreateNetworkAclResponse' {} a -> s {httpStatus = a} :: CreateNetworkAclResponse)

instance Core.NFData CreateNetworkAclResponse
