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
-- Module      : Amazonka.EC2.UpdateSecurityGroupRuleDescriptionsIngress
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the description of an ingress (inbound) security group rule. You
-- can replace an existing description, or add a description to a rule that
-- did not have one previously. You can remove a description for a security
-- group rule by omitting the description parameter in the request.
module Amazonka.EC2.UpdateSecurityGroupRuleDescriptionsIngress
  ( -- * Creating a Request
    UpdateSecurityGroupRuleDescriptionsIngress (..),
    newUpdateSecurityGroupRuleDescriptionsIngress,

    -- * Request Lenses
    updateSecurityGroupRuleDescriptionsIngress_dryRun,
    updateSecurityGroupRuleDescriptionsIngress_groupId,
    updateSecurityGroupRuleDescriptionsIngress_groupName,
    updateSecurityGroupRuleDescriptionsIngress_ipPermissions,
    updateSecurityGroupRuleDescriptionsIngress_securityGroupRuleDescriptions,

    -- * Destructuring the Response
    UpdateSecurityGroupRuleDescriptionsIngressResponse (..),
    newUpdateSecurityGroupRuleDescriptionsIngressResponse,

    -- * Response Lenses
    updateSecurityGroupRuleDescriptionsIngressResponse_return,
    updateSecurityGroupRuleDescriptionsIngressResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateSecurityGroupRuleDescriptionsIngress' smart constructor.
data UpdateSecurityGroupRuleDescriptionsIngress = UpdateSecurityGroupRuleDescriptionsIngress'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the security group. You must specify either the security group
    -- ID or the security group name in the request. For security groups in a
    -- nondefault VPC, you must specify the security group ID.
    groupId :: Prelude.Maybe Prelude.Text,
    -- | [EC2-Classic, default VPC] The name of the security group. You must
    -- specify either the security group ID or the security group name in the
    -- request. For security groups in a nondefault VPC, you must specify the
    -- security group ID.
    groupName :: Prelude.Maybe Prelude.Text,
    -- | The IP permissions for the security group rule. You must specify either
    -- IP permissions or a description.
    ipPermissions :: Prelude.Maybe [IpPermission],
    -- | [VPC only] The description for the ingress security group rules. You
    -- must specify either a description or IP permissions.
    securityGroupRuleDescriptions :: Prelude.Maybe [SecurityGroupRuleDescription]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSecurityGroupRuleDescriptionsIngress' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'updateSecurityGroupRuleDescriptionsIngress_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'groupId', 'updateSecurityGroupRuleDescriptionsIngress_groupId' - The ID of the security group. You must specify either the security group
-- ID or the security group name in the request. For security groups in a
-- nondefault VPC, you must specify the security group ID.
--
-- 'groupName', 'updateSecurityGroupRuleDescriptionsIngress_groupName' - [EC2-Classic, default VPC] The name of the security group. You must
-- specify either the security group ID or the security group name in the
-- request. For security groups in a nondefault VPC, you must specify the
-- security group ID.
--
-- 'ipPermissions', 'updateSecurityGroupRuleDescriptionsIngress_ipPermissions' - The IP permissions for the security group rule. You must specify either
-- IP permissions or a description.
--
-- 'securityGroupRuleDescriptions', 'updateSecurityGroupRuleDescriptionsIngress_securityGroupRuleDescriptions' - [VPC only] The description for the ingress security group rules. You
-- must specify either a description or IP permissions.
newUpdateSecurityGroupRuleDescriptionsIngress ::
  UpdateSecurityGroupRuleDescriptionsIngress
newUpdateSecurityGroupRuleDescriptionsIngress =
  UpdateSecurityGroupRuleDescriptionsIngress'
    { dryRun =
        Prelude.Nothing,
      groupId = Prelude.Nothing,
      groupName = Prelude.Nothing,
      ipPermissions = Prelude.Nothing,
      securityGroupRuleDescriptions =
        Prelude.Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
updateSecurityGroupRuleDescriptionsIngress_dryRun :: Lens.Lens' UpdateSecurityGroupRuleDescriptionsIngress (Prelude.Maybe Prelude.Bool)
updateSecurityGroupRuleDescriptionsIngress_dryRun = Lens.lens (\UpdateSecurityGroupRuleDescriptionsIngress' {dryRun} -> dryRun) (\s@UpdateSecurityGroupRuleDescriptionsIngress' {} a -> s {dryRun = a} :: UpdateSecurityGroupRuleDescriptionsIngress)

-- | The ID of the security group. You must specify either the security group
-- ID or the security group name in the request. For security groups in a
-- nondefault VPC, you must specify the security group ID.
updateSecurityGroupRuleDescriptionsIngress_groupId :: Lens.Lens' UpdateSecurityGroupRuleDescriptionsIngress (Prelude.Maybe Prelude.Text)
updateSecurityGroupRuleDescriptionsIngress_groupId = Lens.lens (\UpdateSecurityGroupRuleDescriptionsIngress' {groupId} -> groupId) (\s@UpdateSecurityGroupRuleDescriptionsIngress' {} a -> s {groupId = a} :: UpdateSecurityGroupRuleDescriptionsIngress)

-- | [EC2-Classic, default VPC] The name of the security group. You must
-- specify either the security group ID or the security group name in the
-- request. For security groups in a nondefault VPC, you must specify the
-- security group ID.
updateSecurityGroupRuleDescriptionsIngress_groupName :: Lens.Lens' UpdateSecurityGroupRuleDescriptionsIngress (Prelude.Maybe Prelude.Text)
updateSecurityGroupRuleDescriptionsIngress_groupName = Lens.lens (\UpdateSecurityGroupRuleDescriptionsIngress' {groupName} -> groupName) (\s@UpdateSecurityGroupRuleDescriptionsIngress' {} a -> s {groupName = a} :: UpdateSecurityGroupRuleDescriptionsIngress)

-- | The IP permissions for the security group rule. You must specify either
-- IP permissions or a description.
updateSecurityGroupRuleDescriptionsIngress_ipPermissions :: Lens.Lens' UpdateSecurityGroupRuleDescriptionsIngress (Prelude.Maybe [IpPermission])
updateSecurityGroupRuleDescriptionsIngress_ipPermissions = Lens.lens (\UpdateSecurityGroupRuleDescriptionsIngress' {ipPermissions} -> ipPermissions) (\s@UpdateSecurityGroupRuleDescriptionsIngress' {} a -> s {ipPermissions = a} :: UpdateSecurityGroupRuleDescriptionsIngress) Prelude.. Lens.mapping Lens.coerced

-- | [VPC only] The description for the ingress security group rules. You
-- must specify either a description or IP permissions.
updateSecurityGroupRuleDescriptionsIngress_securityGroupRuleDescriptions :: Lens.Lens' UpdateSecurityGroupRuleDescriptionsIngress (Prelude.Maybe [SecurityGroupRuleDescription])
updateSecurityGroupRuleDescriptionsIngress_securityGroupRuleDescriptions = Lens.lens (\UpdateSecurityGroupRuleDescriptionsIngress' {securityGroupRuleDescriptions} -> securityGroupRuleDescriptions) (\s@UpdateSecurityGroupRuleDescriptionsIngress' {} a -> s {securityGroupRuleDescriptions = a} :: UpdateSecurityGroupRuleDescriptionsIngress) Prelude.. Lens.mapping Lens.coerced

instance
  Core.AWSRequest
    UpdateSecurityGroupRuleDescriptionsIngress
  where
  type
    AWSResponse
      UpdateSecurityGroupRuleDescriptionsIngress =
      UpdateSecurityGroupRuleDescriptionsIngressResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          UpdateSecurityGroupRuleDescriptionsIngressResponse'
            Prelude.<$> (x Data..@? "return")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateSecurityGroupRuleDescriptionsIngress
  where
  hashWithSalt
    _salt
    UpdateSecurityGroupRuleDescriptionsIngress' {..} =
      _salt
        `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` groupId
        `Prelude.hashWithSalt` groupName
        `Prelude.hashWithSalt` ipPermissions
        `Prelude.hashWithSalt` securityGroupRuleDescriptions

instance
  Prelude.NFData
    UpdateSecurityGroupRuleDescriptionsIngress
  where
  rnf UpdateSecurityGroupRuleDescriptionsIngress' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf groupId
      `Prelude.seq` Prelude.rnf groupName
      `Prelude.seq` Prelude.rnf ipPermissions
      `Prelude.seq` Prelude.rnf securityGroupRuleDescriptions

instance
  Data.ToHeaders
    UpdateSecurityGroupRuleDescriptionsIngress
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    UpdateSecurityGroupRuleDescriptionsIngress
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    UpdateSecurityGroupRuleDescriptionsIngress
  where
  toQuery
    UpdateSecurityGroupRuleDescriptionsIngress' {..} =
      Prelude.mconcat
        [ "Action"
            Data.=: ( "UpdateSecurityGroupRuleDescriptionsIngress" ::
                        Prelude.ByteString
                    ),
          "Version"
            Data.=: ("2016-11-15" :: Prelude.ByteString),
          "DryRun" Data.=: dryRun,
          "GroupId" Data.=: groupId,
          "GroupName" Data.=: groupName,
          Data.toQuery
            ( Data.toQueryList "IpPermissions"
                Prelude.<$> ipPermissions
            ),
          Data.toQuery
            ( Data.toQueryList "SecurityGroupRuleDescription"
                Prelude.<$> securityGroupRuleDescriptions
            )
        ]

-- | /See:/ 'newUpdateSecurityGroupRuleDescriptionsIngressResponse' smart constructor.
data UpdateSecurityGroupRuleDescriptionsIngressResponse = UpdateSecurityGroupRuleDescriptionsIngressResponse'
  { -- | Returns @true@ if the request succeeds; otherwise, returns an error.
    return' :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSecurityGroupRuleDescriptionsIngressResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'return'', 'updateSecurityGroupRuleDescriptionsIngressResponse_return' - Returns @true@ if the request succeeds; otherwise, returns an error.
--
-- 'httpStatus', 'updateSecurityGroupRuleDescriptionsIngressResponse_httpStatus' - The response's http status code.
newUpdateSecurityGroupRuleDescriptionsIngressResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateSecurityGroupRuleDescriptionsIngressResponse
newUpdateSecurityGroupRuleDescriptionsIngressResponse
  pHttpStatus_ =
    UpdateSecurityGroupRuleDescriptionsIngressResponse'
      { return' =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | Returns @true@ if the request succeeds; otherwise, returns an error.
updateSecurityGroupRuleDescriptionsIngressResponse_return :: Lens.Lens' UpdateSecurityGroupRuleDescriptionsIngressResponse (Prelude.Maybe Prelude.Bool)
updateSecurityGroupRuleDescriptionsIngressResponse_return = Lens.lens (\UpdateSecurityGroupRuleDescriptionsIngressResponse' {return'} -> return') (\s@UpdateSecurityGroupRuleDescriptionsIngressResponse' {} a -> s {return' = a} :: UpdateSecurityGroupRuleDescriptionsIngressResponse)

-- | The response's http status code.
updateSecurityGroupRuleDescriptionsIngressResponse_httpStatus :: Lens.Lens' UpdateSecurityGroupRuleDescriptionsIngressResponse Prelude.Int
updateSecurityGroupRuleDescriptionsIngressResponse_httpStatus = Lens.lens (\UpdateSecurityGroupRuleDescriptionsIngressResponse' {httpStatus} -> httpStatus) (\s@UpdateSecurityGroupRuleDescriptionsIngressResponse' {} a -> s {httpStatus = a} :: UpdateSecurityGroupRuleDescriptionsIngressResponse)

instance
  Prelude.NFData
    UpdateSecurityGroupRuleDescriptionsIngressResponse
  where
  rnf
    UpdateSecurityGroupRuleDescriptionsIngressResponse' {..} =
      Prelude.rnf return'
        `Prelude.seq` Prelude.rnf httpStatus
