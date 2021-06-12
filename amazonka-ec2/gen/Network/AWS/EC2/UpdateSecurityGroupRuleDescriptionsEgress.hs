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
-- Module      : Network.AWS.EC2.UpdateSecurityGroupRuleDescriptionsEgress
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- [VPC only] Updates the description of an egress (outbound) security
-- group rule. You can replace an existing description, or add a
-- description to a rule that did not have one previously.
--
-- You specify the description as part of the IP permissions structure. You
-- can remove a description for a security group rule by omitting the
-- description parameter in the request.
module Network.AWS.EC2.UpdateSecurityGroupRuleDescriptionsEgress
  ( -- * Creating a Request
    UpdateSecurityGroupRuleDescriptionsEgress (..),
    newUpdateSecurityGroupRuleDescriptionsEgress,

    -- * Request Lenses
    updateSecurityGroupRuleDescriptionsEgress_dryRun,
    updateSecurityGroupRuleDescriptionsEgress_groupName,
    updateSecurityGroupRuleDescriptionsEgress_groupId,
    updateSecurityGroupRuleDescriptionsEgress_ipPermissions,

    -- * Destructuring the Response
    UpdateSecurityGroupRuleDescriptionsEgressResponse (..),
    newUpdateSecurityGroupRuleDescriptionsEgressResponse,

    -- * Response Lenses
    updateSecurityGroupRuleDescriptionsEgressResponse_return,
    updateSecurityGroupRuleDescriptionsEgressResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateSecurityGroupRuleDescriptionsEgress' smart constructor.
data UpdateSecurityGroupRuleDescriptionsEgress = UpdateSecurityGroupRuleDescriptionsEgress'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | [Default VPC] The name of the security group. You must specify either
    -- the security group ID or the security group name in the request.
    groupName :: Core.Maybe Core.Text,
    -- | The ID of the security group. You must specify either the security group
    -- ID or the security group name in the request. For security groups in a
    -- nondefault VPC, you must specify the security group ID.
    groupId :: Core.Maybe Core.Text,
    -- | The IP permissions for the security group rule.
    ipPermissions :: [IpPermission]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateSecurityGroupRuleDescriptionsEgress' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'updateSecurityGroupRuleDescriptionsEgress_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'groupName', 'updateSecurityGroupRuleDescriptionsEgress_groupName' - [Default VPC] The name of the security group. You must specify either
-- the security group ID or the security group name in the request.
--
-- 'groupId', 'updateSecurityGroupRuleDescriptionsEgress_groupId' - The ID of the security group. You must specify either the security group
-- ID or the security group name in the request. For security groups in a
-- nondefault VPC, you must specify the security group ID.
--
-- 'ipPermissions', 'updateSecurityGroupRuleDescriptionsEgress_ipPermissions' - The IP permissions for the security group rule.
newUpdateSecurityGroupRuleDescriptionsEgress ::
  UpdateSecurityGroupRuleDescriptionsEgress
newUpdateSecurityGroupRuleDescriptionsEgress =
  UpdateSecurityGroupRuleDescriptionsEgress'
    { dryRun =
        Core.Nothing,
      groupName = Core.Nothing,
      groupId = Core.Nothing,
      ipPermissions = Core.mempty
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
updateSecurityGroupRuleDescriptionsEgress_dryRun :: Lens.Lens' UpdateSecurityGroupRuleDescriptionsEgress (Core.Maybe Core.Bool)
updateSecurityGroupRuleDescriptionsEgress_dryRun = Lens.lens (\UpdateSecurityGroupRuleDescriptionsEgress' {dryRun} -> dryRun) (\s@UpdateSecurityGroupRuleDescriptionsEgress' {} a -> s {dryRun = a} :: UpdateSecurityGroupRuleDescriptionsEgress)

-- | [Default VPC] The name of the security group. You must specify either
-- the security group ID or the security group name in the request.
updateSecurityGroupRuleDescriptionsEgress_groupName :: Lens.Lens' UpdateSecurityGroupRuleDescriptionsEgress (Core.Maybe Core.Text)
updateSecurityGroupRuleDescriptionsEgress_groupName = Lens.lens (\UpdateSecurityGroupRuleDescriptionsEgress' {groupName} -> groupName) (\s@UpdateSecurityGroupRuleDescriptionsEgress' {} a -> s {groupName = a} :: UpdateSecurityGroupRuleDescriptionsEgress)

-- | The ID of the security group. You must specify either the security group
-- ID or the security group name in the request. For security groups in a
-- nondefault VPC, you must specify the security group ID.
updateSecurityGroupRuleDescriptionsEgress_groupId :: Lens.Lens' UpdateSecurityGroupRuleDescriptionsEgress (Core.Maybe Core.Text)
updateSecurityGroupRuleDescriptionsEgress_groupId = Lens.lens (\UpdateSecurityGroupRuleDescriptionsEgress' {groupId} -> groupId) (\s@UpdateSecurityGroupRuleDescriptionsEgress' {} a -> s {groupId = a} :: UpdateSecurityGroupRuleDescriptionsEgress)

-- | The IP permissions for the security group rule.
updateSecurityGroupRuleDescriptionsEgress_ipPermissions :: Lens.Lens' UpdateSecurityGroupRuleDescriptionsEgress [IpPermission]
updateSecurityGroupRuleDescriptionsEgress_ipPermissions = Lens.lens (\UpdateSecurityGroupRuleDescriptionsEgress' {ipPermissions} -> ipPermissions) (\s@UpdateSecurityGroupRuleDescriptionsEgress' {} a -> s {ipPermissions = a} :: UpdateSecurityGroupRuleDescriptionsEgress) Core.. Lens._Coerce

instance
  Core.AWSRequest
    UpdateSecurityGroupRuleDescriptionsEgress
  where
  type
    AWSResponse
      UpdateSecurityGroupRuleDescriptionsEgress =
      UpdateSecurityGroupRuleDescriptionsEgressResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          UpdateSecurityGroupRuleDescriptionsEgressResponse'
            Core.<$> (x Core..@? "return")
              Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    UpdateSecurityGroupRuleDescriptionsEgress

instance
  Core.NFData
    UpdateSecurityGroupRuleDescriptionsEgress

instance
  Core.ToHeaders
    UpdateSecurityGroupRuleDescriptionsEgress
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToPath
    UpdateSecurityGroupRuleDescriptionsEgress
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    UpdateSecurityGroupRuleDescriptionsEgress
  where
  toQuery
    UpdateSecurityGroupRuleDescriptionsEgress' {..} =
      Core.mconcat
        [ "Action"
            Core.=: ( "UpdateSecurityGroupRuleDescriptionsEgress" ::
                        Core.ByteString
                    ),
          "Version" Core.=: ("2016-11-15" :: Core.ByteString),
          "DryRun" Core.=: dryRun,
          "GroupName" Core.=: groupName,
          "GroupId" Core.=: groupId,
          Core.toQueryList "IpPermissions" ipPermissions
        ]

-- | /See:/ 'newUpdateSecurityGroupRuleDescriptionsEgressResponse' smart constructor.
data UpdateSecurityGroupRuleDescriptionsEgressResponse = UpdateSecurityGroupRuleDescriptionsEgressResponse'
  { -- | Returns @true@ if the request succeeds; otherwise, returns an error.
    return' :: Core.Maybe Core.Bool,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateSecurityGroupRuleDescriptionsEgressResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'return'', 'updateSecurityGroupRuleDescriptionsEgressResponse_return' - Returns @true@ if the request succeeds; otherwise, returns an error.
--
-- 'httpStatus', 'updateSecurityGroupRuleDescriptionsEgressResponse_httpStatus' - The response's http status code.
newUpdateSecurityGroupRuleDescriptionsEgressResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateSecurityGroupRuleDescriptionsEgressResponse
newUpdateSecurityGroupRuleDescriptionsEgressResponse
  pHttpStatus_ =
    UpdateSecurityGroupRuleDescriptionsEgressResponse'
      { return' =
          Core.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | Returns @true@ if the request succeeds; otherwise, returns an error.
updateSecurityGroupRuleDescriptionsEgressResponse_return :: Lens.Lens' UpdateSecurityGroupRuleDescriptionsEgressResponse (Core.Maybe Core.Bool)
updateSecurityGroupRuleDescriptionsEgressResponse_return = Lens.lens (\UpdateSecurityGroupRuleDescriptionsEgressResponse' {return'} -> return') (\s@UpdateSecurityGroupRuleDescriptionsEgressResponse' {} a -> s {return' = a} :: UpdateSecurityGroupRuleDescriptionsEgressResponse)

-- | The response's http status code.
updateSecurityGroupRuleDescriptionsEgressResponse_httpStatus :: Lens.Lens' UpdateSecurityGroupRuleDescriptionsEgressResponse Core.Int
updateSecurityGroupRuleDescriptionsEgressResponse_httpStatus = Lens.lens (\UpdateSecurityGroupRuleDescriptionsEgressResponse' {httpStatus} -> httpStatus) (\s@UpdateSecurityGroupRuleDescriptionsEgressResponse' {} a -> s {httpStatus = a} :: UpdateSecurityGroupRuleDescriptionsEgressResponse)

instance
  Core.NFData
    UpdateSecurityGroupRuleDescriptionsEgressResponse
