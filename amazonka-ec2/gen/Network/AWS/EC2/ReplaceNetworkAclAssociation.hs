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
-- Module      : Network.AWS.EC2.ReplaceNetworkAclAssociation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes which network ACL a subnet is associated with. By default when
-- you create a subnet, it\'s automatically associated with the default
-- network ACL. For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_ACLs.html Network ACLs>
-- in the /Amazon Virtual Private Cloud User Guide/.
--
-- This is an idempotent operation.
module Network.AWS.EC2.ReplaceNetworkAclAssociation
  ( -- * Creating a Request
    ReplaceNetworkAclAssociation (..),
    newReplaceNetworkAclAssociation,

    -- * Request Lenses
    replaceNetworkAclAssociation_dryRun,
    replaceNetworkAclAssociation_associationId,
    replaceNetworkAclAssociation_networkAclId,

    -- * Destructuring the Response
    ReplaceNetworkAclAssociationResponse (..),
    newReplaceNetworkAclAssociationResponse,

    -- * Response Lenses
    replaceNetworkAclAssociationResponse_newAssociationId,
    replaceNetworkAclAssociationResponse_httpStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newReplaceNetworkAclAssociation' smart constructor.
data ReplaceNetworkAclAssociation = ReplaceNetworkAclAssociation'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the current association between the original network ACL and
    -- the subnet.
    associationId :: Prelude.Text,
    -- | The ID of the new network ACL to associate with the subnet.
    networkAclId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ReplaceNetworkAclAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'replaceNetworkAclAssociation_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'associationId', 'replaceNetworkAclAssociation_associationId' - The ID of the current association between the original network ACL and
-- the subnet.
--
-- 'networkAclId', 'replaceNetworkAclAssociation_networkAclId' - The ID of the new network ACL to associate with the subnet.
newReplaceNetworkAclAssociation ::
  -- | 'associationId'
  Prelude.Text ->
  -- | 'networkAclId'
  Prelude.Text ->
  ReplaceNetworkAclAssociation
newReplaceNetworkAclAssociation
  pAssociationId_
  pNetworkAclId_ =
    ReplaceNetworkAclAssociation'
      { dryRun =
          Prelude.Nothing,
        associationId = pAssociationId_,
        networkAclId = pNetworkAclId_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
replaceNetworkAclAssociation_dryRun :: Lens.Lens' ReplaceNetworkAclAssociation (Prelude.Maybe Prelude.Bool)
replaceNetworkAclAssociation_dryRun = Lens.lens (\ReplaceNetworkAclAssociation' {dryRun} -> dryRun) (\s@ReplaceNetworkAclAssociation' {} a -> s {dryRun = a} :: ReplaceNetworkAclAssociation)

-- | The ID of the current association between the original network ACL and
-- the subnet.
replaceNetworkAclAssociation_associationId :: Lens.Lens' ReplaceNetworkAclAssociation Prelude.Text
replaceNetworkAclAssociation_associationId = Lens.lens (\ReplaceNetworkAclAssociation' {associationId} -> associationId) (\s@ReplaceNetworkAclAssociation' {} a -> s {associationId = a} :: ReplaceNetworkAclAssociation)

-- | The ID of the new network ACL to associate with the subnet.
replaceNetworkAclAssociation_networkAclId :: Lens.Lens' ReplaceNetworkAclAssociation Prelude.Text
replaceNetworkAclAssociation_networkAclId = Lens.lens (\ReplaceNetworkAclAssociation' {networkAclId} -> networkAclId) (\s@ReplaceNetworkAclAssociation' {} a -> s {networkAclId = a} :: ReplaceNetworkAclAssociation)

instance
  Prelude.AWSRequest
    ReplaceNetworkAclAssociation
  where
  type
    Rs ReplaceNetworkAclAssociation =
      ReplaceNetworkAclAssociationResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          ReplaceNetworkAclAssociationResponse'
            Prelude.<$> (x Prelude..@? "newAssociationId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ReplaceNetworkAclAssociation

instance Prelude.NFData ReplaceNetworkAclAssociation

instance
  Prelude.ToHeaders
    ReplaceNetworkAclAssociation
  where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath ReplaceNetworkAclAssociation where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ReplaceNetworkAclAssociation where
  toQuery ReplaceNetworkAclAssociation' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ( "ReplaceNetworkAclAssociation" ::
                         Prelude.ByteString
                     ),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Prelude.=: dryRun,
        "AssociationId" Prelude.=: associationId,
        "NetworkAclId" Prelude.=: networkAclId
      ]

-- | /See:/ 'newReplaceNetworkAclAssociationResponse' smart constructor.
data ReplaceNetworkAclAssociationResponse = ReplaceNetworkAclAssociationResponse'
  { -- | The ID of the new association.
    newAssociationId' :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ReplaceNetworkAclAssociationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'newAssociationId'', 'replaceNetworkAclAssociationResponse_newAssociationId' - The ID of the new association.
--
-- 'httpStatus', 'replaceNetworkAclAssociationResponse_httpStatus' - The response's http status code.
newReplaceNetworkAclAssociationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ReplaceNetworkAclAssociationResponse
newReplaceNetworkAclAssociationResponse pHttpStatus_ =
  ReplaceNetworkAclAssociationResponse'
    { newAssociationId' =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the new association.
replaceNetworkAclAssociationResponse_newAssociationId :: Lens.Lens' ReplaceNetworkAclAssociationResponse (Prelude.Maybe Prelude.Text)
replaceNetworkAclAssociationResponse_newAssociationId = Lens.lens (\ReplaceNetworkAclAssociationResponse' {newAssociationId'} -> newAssociationId') (\s@ReplaceNetworkAclAssociationResponse' {} a -> s {newAssociationId' = a} :: ReplaceNetworkAclAssociationResponse)

-- | The response's http status code.
replaceNetworkAclAssociationResponse_httpStatus :: Lens.Lens' ReplaceNetworkAclAssociationResponse Prelude.Int
replaceNetworkAclAssociationResponse_httpStatus = Lens.lens (\ReplaceNetworkAclAssociationResponse' {httpStatus} -> httpStatus) (\s@ReplaceNetworkAclAssociationResponse' {} a -> s {httpStatus = a} :: ReplaceNetworkAclAssociationResponse)

instance
  Prelude.NFData
    ReplaceNetworkAclAssociationResponse
