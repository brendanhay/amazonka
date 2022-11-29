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
-- Module      : Amazonka.AppStream.DeleteEntitlement
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified entitlement.
module Amazonka.AppStream.DeleteEntitlement
  ( -- * Creating a Request
    DeleteEntitlement (..),
    newDeleteEntitlement,

    -- * Request Lenses
    deleteEntitlement_name,
    deleteEntitlement_stackName,

    -- * Destructuring the Response
    DeleteEntitlementResponse (..),
    newDeleteEntitlementResponse,

    -- * Response Lenses
    deleteEntitlementResponse_httpStatus,
  )
where

import Amazonka.AppStream.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteEntitlement' smart constructor.
data DeleteEntitlement = DeleteEntitlement'
  { -- | The name of the entitlement.
    name :: Prelude.Text,
    -- | The name of the stack with which the entitlement is associated.
    stackName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteEntitlement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'deleteEntitlement_name' - The name of the entitlement.
--
-- 'stackName', 'deleteEntitlement_stackName' - The name of the stack with which the entitlement is associated.
newDeleteEntitlement ::
  -- | 'name'
  Prelude.Text ->
  -- | 'stackName'
  Prelude.Text ->
  DeleteEntitlement
newDeleteEntitlement pName_ pStackName_ =
  DeleteEntitlement'
    { name = pName_,
      stackName = pStackName_
    }

-- | The name of the entitlement.
deleteEntitlement_name :: Lens.Lens' DeleteEntitlement Prelude.Text
deleteEntitlement_name = Lens.lens (\DeleteEntitlement' {name} -> name) (\s@DeleteEntitlement' {} a -> s {name = a} :: DeleteEntitlement)

-- | The name of the stack with which the entitlement is associated.
deleteEntitlement_stackName :: Lens.Lens' DeleteEntitlement Prelude.Text
deleteEntitlement_stackName = Lens.lens (\DeleteEntitlement' {stackName} -> stackName) (\s@DeleteEntitlement' {} a -> s {stackName = a} :: DeleteEntitlement)

instance Core.AWSRequest DeleteEntitlement where
  type
    AWSResponse DeleteEntitlement =
      DeleteEntitlementResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteEntitlementResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteEntitlement where
  hashWithSalt _salt DeleteEntitlement' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` stackName

instance Prelude.NFData DeleteEntitlement where
  rnf DeleteEntitlement' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf stackName

instance Core.ToHeaders DeleteEntitlement where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "PhotonAdminProxyService.DeleteEntitlement" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteEntitlement where
  toJSON DeleteEntitlement' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Core..= name),
            Prelude.Just ("StackName" Core..= stackName)
          ]
      )

instance Core.ToPath DeleteEntitlement where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteEntitlement where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteEntitlementResponse' smart constructor.
data DeleteEntitlementResponse = DeleteEntitlementResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteEntitlementResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteEntitlementResponse_httpStatus' - The response's http status code.
newDeleteEntitlementResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteEntitlementResponse
newDeleteEntitlementResponse pHttpStatus_ =
  DeleteEntitlementResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteEntitlementResponse_httpStatus :: Lens.Lens' DeleteEntitlementResponse Prelude.Int
deleteEntitlementResponse_httpStatus = Lens.lens (\DeleteEntitlementResponse' {httpStatus} -> httpStatus) (\s@DeleteEntitlementResponse' {} a -> s {httpStatus = a} :: DeleteEntitlementResponse)

instance Prelude.NFData DeleteEntitlementResponse where
  rnf DeleteEntitlementResponse' {..} =
    Prelude.rnf httpStatus
