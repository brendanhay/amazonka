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

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteNetworkAcl' smart constructor.
data DeleteNetworkAcl = DeleteNetworkAcl'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the network ACL.
    networkAclId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  DeleteNetworkAcl
newDeleteNetworkAcl pNetworkAclId_ =
  DeleteNetworkAcl'
    { dryRun = Prelude.Nothing,
      networkAclId = pNetworkAclId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteNetworkAcl_dryRun :: Lens.Lens' DeleteNetworkAcl (Prelude.Maybe Prelude.Bool)
deleteNetworkAcl_dryRun = Lens.lens (\DeleteNetworkAcl' {dryRun} -> dryRun) (\s@DeleteNetworkAcl' {} a -> s {dryRun = a} :: DeleteNetworkAcl)

-- | The ID of the network ACL.
deleteNetworkAcl_networkAclId :: Lens.Lens' DeleteNetworkAcl Prelude.Text
deleteNetworkAcl_networkAclId = Lens.lens (\DeleteNetworkAcl' {networkAclId} -> networkAclId) (\s@DeleteNetworkAcl' {} a -> s {networkAclId = a} :: DeleteNetworkAcl)

instance Prelude.AWSRequest DeleteNetworkAcl where
  type Rs DeleteNetworkAcl = DeleteNetworkAclResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull DeleteNetworkAclResponse'

instance Prelude.Hashable DeleteNetworkAcl

instance Prelude.NFData DeleteNetworkAcl

instance Prelude.ToHeaders DeleteNetworkAcl where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DeleteNetworkAcl where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteNetworkAcl where
  toQuery DeleteNetworkAcl' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("DeleteNetworkAcl" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Prelude.=: dryRun,
        "NetworkAclId" Prelude.=: networkAclId
      ]

-- | /See:/ 'newDeleteNetworkAclResponse' smart constructor.
data DeleteNetworkAclResponse = DeleteNetworkAclResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteNetworkAclResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteNetworkAclResponse ::
  DeleteNetworkAclResponse
newDeleteNetworkAclResponse =
  DeleteNetworkAclResponse'

instance Prelude.NFData DeleteNetworkAclResponse
