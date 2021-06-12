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
-- Module      : Network.AWS.EC2.DeleteNetworkAcl
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified network ACL. You can\'t delete the ACL if it\'s
-- associated with any subnets. You can\'t delete the default network ACL.
module Network.AWS.EC2.DeleteNetworkAcl
  ( -- * Creating a Request
    DeleteNetworkAcl (..),
    newDeleteNetworkAcl,

    -- * Request Lenses
    deleteNetworkAcl_dryRun,
    deleteNetworkAcl_networkAclId,

    -- * Destructuring the Response
    DeleteNetworkAclResponse (..),
    newDeleteNetworkAclResponse,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteNetworkAcl' smart constructor.
data DeleteNetworkAcl = DeleteNetworkAcl'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The ID of the network ACL.
    networkAclId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteNetworkAcl' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'deleteNetworkAcl_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'networkAclId', 'deleteNetworkAcl_networkAclId' - The ID of the network ACL.
newDeleteNetworkAcl ::
  -- | 'networkAclId'
  Core.Text ->
  DeleteNetworkAcl
newDeleteNetworkAcl pNetworkAclId_ =
  DeleteNetworkAcl'
    { dryRun = Core.Nothing,
      networkAclId = pNetworkAclId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteNetworkAcl_dryRun :: Lens.Lens' DeleteNetworkAcl (Core.Maybe Core.Bool)
deleteNetworkAcl_dryRun = Lens.lens (\DeleteNetworkAcl' {dryRun} -> dryRun) (\s@DeleteNetworkAcl' {} a -> s {dryRun = a} :: DeleteNetworkAcl)

-- | The ID of the network ACL.
deleteNetworkAcl_networkAclId :: Lens.Lens' DeleteNetworkAcl Core.Text
deleteNetworkAcl_networkAclId = Lens.lens (\DeleteNetworkAcl' {networkAclId} -> networkAclId) (\s@DeleteNetworkAcl' {} a -> s {networkAclId = a} :: DeleteNetworkAcl)

instance Core.AWSRequest DeleteNetworkAcl where
  type
    AWSResponse DeleteNetworkAcl =
      DeleteNetworkAclResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull DeleteNetworkAclResponse'

instance Core.Hashable DeleteNetworkAcl

instance Core.NFData DeleteNetworkAcl

instance Core.ToHeaders DeleteNetworkAcl where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DeleteNetworkAcl where
  toPath = Core.const "/"

instance Core.ToQuery DeleteNetworkAcl where
  toQuery DeleteNetworkAcl' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DeleteNetworkAcl" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "DryRun" Core.=: dryRun,
        "NetworkAclId" Core.=: networkAclId
      ]

-- | /See:/ 'newDeleteNetworkAclResponse' smart constructor.
data DeleteNetworkAclResponse = DeleteNetworkAclResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteNetworkAclResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteNetworkAclResponse ::
  DeleteNetworkAclResponse
newDeleteNetworkAclResponse =
  DeleteNetworkAclResponse'

instance Core.NFData DeleteNetworkAclResponse
