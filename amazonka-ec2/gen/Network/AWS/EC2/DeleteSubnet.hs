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

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteSubnet' smart constructor.
data DeleteSubnet = DeleteSubnet'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the subnet.
    subnetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  DeleteSubnet
newDeleteSubnet pSubnetId_ =
  DeleteSubnet'
    { dryRun = Prelude.Nothing,
      subnetId = pSubnetId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteSubnet_dryRun :: Lens.Lens' DeleteSubnet (Prelude.Maybe Prelude.Bool)
deleteSubnet_dryRun = Lens.lens (\DeleteSubnet' {dryRun} -> dryRun) (\s@DeleteSubnet' {} a -> s {dryRun = a} :: DeleteSubnet)

-- | The ID of the subnet.
deleteSubnet_subnetId :: Lens.Lens' DeleteSubnet Prelude.Text
deleteSubnet_subnetId = Lens.lens (\DeleteSubnet' {subnetId} -> subnetId) (\s@DeleteSubnet' {} a -> s {subnetId = a} :: DeleteSubnet)

instance Prelude.AWSRequest DeleteSubnet where
  type Rs DeleteSubnet = DeleteSubnetResponse
  request = Request.postQuery defaultService
  response = Response.receiveNull DeleteSubnetResponse'

instance Prelude.Hashable DeleteSubnet

instance Prelude.NFData DeleteSubnet

instance Prelude.ToHeaders DeleteSubnet where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DeleteSubnet where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteSubnet where
  toQuery DeleteSubnet' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("DeleteSubnet" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Prelude.=: dryRun,
        "SubnetId" Prelude.=: subnetId
      ]

-- | /See:/ 'newDeleteSubnetResponse' smart constructor.
data DeleteSubnetResponse = DeleteSubnetResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteSubnetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteSubnetResponse ::
  DeleteSubnetResponse
newDeleteSubnetResponse = DeleteSubnetResponse'

instance Prelude.NFData DeleteSubnetResponse
