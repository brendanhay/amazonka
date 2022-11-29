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
-- Module      : Amazonka.CognitoIdentity.DeleteIdentityPool
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an identity pool. Once a pool is deleted, users will not be able
-- to authenticate with the pool.
--
-- You must use AWS Developer credentials to call this API.
module Amazonka.CognitoIdentity.DeleteIdentityPool
  ( -- * Creating a Request
    DeleteIdentityPool (..),
    newDeleteIdentityPool,

    -- * Request Lenses
    deleteIdentityPool_identityPoolId,

    -- * Destructuring the Response
    DeleteIdentityPoolResponse (..),
    newDeleteIdentityPoolResponse,
  )
where

import Amazonka.CognitoIdentity.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Input to the DeleteIdentityPool action.
--
-- /See:/ 'newDeleteIdentityPool' smart constructor.
data DeleteIdentityPool = DeleteIdentityPool'
  { -- | An identity pool ID in the format REGION:GUID.
    identityPoolId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteIdentityPool' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identityPoolId', 'deleteIdentityPool_identityPoolId' - An identity pool ID in the format REGION:GUID.
newDeleteIdentityPool ::
  -- | 'identityPoolId'
  Prelude.Text ->
  DeleteIdentityPool
newDeleteIdentityPool pIdentityPoolId_ =
  DeleteIdentityPool'
    { identityPoolId =
        pIdentityPoolId_
    }

-- | An identity pool ID in the format REGION:GUID.
deleteIdentityPool_identityPoolId :: Lens.Lens' DeleteIdentityPool Prelude.Text
deleteIdentityPool_identityPoolId = Lens.lens (\DeleteIdentityPool' {identityPoolId} -> identityPoolId) (\s@DeleteIdentityPool' {} a -> s {identityPoolId = a} :: DeleteIdentityPool)

instance Core.AWSRequest DeleteIdentityPool where
  type
    AWSResponse DeleteIdentityPool =
      DeleteIdentityPoolResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull DeleteIdentityPoolResponse'

instance Prelude.Hashable DeleteIdentityPool where
  hashWithSalt _salt DeleteIdentityPool' {..} =
    _salt `Prelude.hashWithSalt` identityPoolId

instance Prelude.NFData DeleteIdentityPool where
  rnf DeleteIdentityPool' {..} =
    Prelude.rnf identityPoolId

instance Core.ToHeaders DeleteIdentityPool where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityService.DeleteIdentityPool" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteIdentityPool where
  toJSON DeleteIdentityPool' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("IdentityPoolId" Core..= identityPoolId)
          ]
      )

instance Core.ToPath DeleteIdentityPool where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteIdentityPool where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteIdentityPoolResponse' smart constructor.
data DeleteIdentityPoolResponse = DeleteIdentityPoolResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteIdentityPoolResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteIdentityPoolResponse ::
  DeleteIdentityPoolResponse
newDeleteIdentityPoolResponse =
  DeleteIdentityPoolResponse'

instance Prelude.NFData DeleteIdentityPoolResponse where
  rnf _ = ()
