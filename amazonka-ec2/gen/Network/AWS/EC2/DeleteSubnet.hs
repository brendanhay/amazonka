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
-- Module      : Network.AWS.EC2.DeleteSubnet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified subnet. You must terminate all running instances
-- in the subnet before you can delete the subnet.
module Network.AWS.EC2.DeleteSubnet
  ( -- * Creating a Request
    DeleteSubnet (..),
    newDeleteSubnet,

    -- * Request Lenses
    deleteSubnet_dryRun,
    deleteSubnet_subnetId,

    -- * Destructuring the Response
    DeleteSubnetResponse (..),
    newDeleteSubnetResponse,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteSubnet' smart constructor.
data DeleteSubnet = DeleteSubnet'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The ID of the subnet.
    subnetId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteSubnet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'deleteSubnet_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'subnetId', 'deleteSubnet_subnetId' - The ID of the subnet.
newDeleteSubnet ::
  -- | 'subnetId'
  Core.Text ->
  DeleteSubnet
newDeleteSubnet pSubnetId_ =
  DeleteSubnet'
    { dryRun = Core.Nothing,
      subnetId = pSubnetId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteSubnet_dryRun :: Lens.Lens' DeleteSubnet (Core.Maybe Core.Bool)
deleteSubnet_dryRun = Lens.lens (\DeleteSubnet' {dryRun} -> dryRun) (\s@DeleteSubnet' {} a -> s {dryRun = a} :: DeleteSubnet)

-- | The ID of the subnet.
deleteSubnet_subnetId :: Lens.Lens' DeleteSubnet Core.Text
deleteSubnet_subnetId = Lens.lens (\DeleteSubnet' {subnetId} -> subnetId) (\s@DeleteSubnet' {} a -> s {subnetId = a} :: DeleteSubnet)

instance Core.AWSRequest DeleteSubnet where
  type AWSResponse DeleteSubnet = DeleteSubnetResponse
  request = Request.postQuery defaultService
  response = Response.receiveNull DeleteSubnetResponse'

instance Core.Hashable DeleteSubnet

instance Core.NFData DeleteSubnet

instance Core.ToHeaders DeleteSubnet where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DeleteSubnet where
  toPath = Core.const "/"

instance Core.ToQuery DeleteSubnet where
  toQuery DeleteSubnet' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DeleteSubnet" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "DryRun" Core.=: dryRun,
        "SubnetId" Core.=: subnetId
      ]

-- | /See:/ 'newDeleteSubnetResponse' smart constructor.
data DeleteSubnetResponse = DeleteSubnetResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteSubnetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteSubnetResponse ::
  DeleteSubnetResponse
newDeleteSubnetResponse = DeleteSubnetResponse'

instance Core.NFData DeleteSubnetResponse
