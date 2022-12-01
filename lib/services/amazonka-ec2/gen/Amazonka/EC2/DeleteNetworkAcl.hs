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
-- Module      : Amazonka.EC2.DeleteNetworkAcl
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified network ACL. You can\'t delete the ACL if it\'s
-- associated with any subnets. You can\'t delete the default network ACL.
module Amazonka.EC2.DeleteNetworkAcl
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest DeleteNetworkAcl where
  type
    AWSResponse DeleteNetworkAcl =
      DeleteNetworkAclResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull DeleteNetworkAclResponse'

instance Prelude.Hashable DeleteNetworkAcl where
  hashWithSalt _salt DeleteNetworkAcl' {..} =
    _salt `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` networkAclId

instance Prelude.NFData DeleteNetworkAcl where
  rnf DeleteNetworkAcl' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf networkAclId

instance Core.ToHeaders DeleteNetworkAcl where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DeleteNetworkAcl where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteNetworkAcl where
  toQuery DeleteNetworkAcl' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DeleteNetworkAcl" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Core.=: dryRun,
        "NetworkAclId" Core.=: networkAclId
      ]

-- | /See:/ 'newDeleteNetworkAclResponse' smart constructor.
data DeleteNetworkAclResponse = DeleteNetworkAclResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteNetworkAclResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteNetworkAclResponse ::
  DeleteNetworkAclResponse
newDeleteNetworkAclResponse =
  DeleteNetworkAclResponse'

instance Prelude.NFData DeleteNetworkAclResponse where
  rnf _ = ()
