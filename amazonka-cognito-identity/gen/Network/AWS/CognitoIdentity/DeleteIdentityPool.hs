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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Input to the DeleteIdentityPool action.
--
-- /See:/ 'newDeleteIdentityPool' smart constructor.
data DeleteIdentityPool = DeleteIdentityPool'
  { -- | An identity pool ID in the format REGION:GUID.
    identityPoolId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest DeleteIdentityPool where
  type
    Rs DeleteIdentityPool =
      DeleteIdentityPoolResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull DeleteIdentityPoolResponse'

instance Prelude.Hashable DeleteIdentityPool

instance Prelude.NFData DeleteIdentityPool

instance Prelude.ToHeaders DeleteIdentityPool where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSCognitoIdentityService.DeleteIdentityPool" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteIdentityPool where
  toJSON DeleteIdentityPool' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("IdentityPoolId" Prelude..= identityPoolId)
          ]
      )

instance Prelude.ToPath DeleteIdentityPool where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteIdentityPool where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteIdentityPoolResponse' smart constructor.
data DeleteIdentityPoolResponse = DeleteIdentityPoolResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteIdentityPoolResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteIdentityPoolResponse ::
  DeleteIdentityPoolResponse
newDeleteIdentityPoolResponse =
  DeleteIdentityPoolResponse'

instance Prelude.NFData DeleteIdentityPoolResponse
