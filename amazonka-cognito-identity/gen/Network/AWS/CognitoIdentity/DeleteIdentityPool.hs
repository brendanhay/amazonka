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
-- Module      : Network.AWS.CognitoIdentity.DeleteIdentityPool
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an identity pool. Once a pool is deleted, users will not be able
-- to authenticate with the pool.
--
-- You must use AWS Developer credentials to call this API.
module Network.AWS.CognitoIdentity.DeleteIdentityPool
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

import Network.AWS.CognitoIdentity.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Input to the DeleteIdentityPool action.
--
-- /See:/ 'newDeleteIdentityPool' smart constructor.
data DeleteIdentityPool = DeleteIdentityPool'
  { -- | An identity pool ID in the format REGION:GUID.
    identityPoolId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  DeleteIdentityPool
newDeleteIdentityPool pIdentityPoolId_ =
  DeleteIdentityPool'
    { identityPoolId =
        pIdentityPoolId_
    }

-- | An identity pool ID in the format REGION:GUID.
deleteIdentityPool_identityPoolId :: Lens.Lens' DeleteIdentityPool Core.Text
deleteIdentityPool_identityPoolId = Lens.lens (\DeleteIdentityPool' {identityPoolId} -> identityPoolId) (\s@DeleteIdentityPool' {} a -> s {identityPoolId = a} :: DeleteIdentityPool)

instance Core.AWSRequest DeleteIdentityPool where
  type
    AWSResponse DeleteIdentityPool =
      DeleteIdentityPoolResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull DeleteIdentityPoolResponse'

instance Core.Hashable DeleteIdentityPool

instance Core.NFData DeleteIdentityPool

instance Core.ToHeaders DeleteIdentityPool where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityService.DeleteIdentityPool" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteIdentityPool where
  toJSON DeleteIdentityPool' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("IdentityPoolId" Core..= identityPoolId)
          ]
      )

instance Core.ToPath DeleteIdentityPool where
  toPath = Core.const "/"

instance Core.ToQuery DeleteIdentityPool where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteIdentityPoolResponse' smart constructor.
data DeleteIdentityPoolResponse = DeleteIdentityPoolResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteIdentityPoolResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteIdentityPoolResponse ::
  DeleteIdentityPoolResponse
newDeleteIdentityPoolResponse =
  DeleteIdentityPoolResponse'

instance Core.NFData DeleteIdentityPoolResponse
