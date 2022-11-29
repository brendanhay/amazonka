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
-- Module      : Amazonka.CognitoIdentityProvider.DeleteUserPool
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified Amazon Cognito user pool.
module Amazonka.CognitoIdentityProvider.DeleteUserPool
  ( -- * Creating a Request
    DeleteUserPool (..),
    newDeleteUserPool,

    -- * Request Lenses
    deleteUserPool_userPoolId,

    -- * Destructuring the Response
    DeleteUserPoolResponse (..),
    newDeleteUserPoolResponse,
  )
where

import Amazonka.CognitoIdentityProvider.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the request to delete a user pool.
--
-- /See:/ 'newDeleteUserPool' smart constructor.
data DeleteUserPool = DeleteUserPool'
  { -- | The user pool ID for the user pool you want to delete.
    userPoolId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteUserPool' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userPoolId', 'deleteUserPool_userPoolId' - The user pool ID for the user pool you want to delete.
newDeleteUserPool ::
  -- | 'userPoolId'
  Prelude.Text ->
  DeleteUserPool
newDeleteUserPool pUserPoolId_ =
  DeleteUserPool' {userPoolId = pUserPoolId_}

-- | The user pool ID for the user pool you want to delete.
deleteUserPool_userPoolId :: Lens.Lens' DeleteUserPool Prelude.Text
deleteUserPool_userPoolId = Lens.lens (\DeleteUserPool' {userPoolId} -> userPoolId) (\s@DeleteUserPool' {} a -> s {userPoolId = a} :: DeleteUserPool)

instance Core.AWSRequest DeleteUserPool where
  type
    AWSResponse DeleteUserPool =
      DeleteUserPoolResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull DeleteUserPoolResponse'

instance Prelude.Hashable DeleteUserPool where
  hashWithSalt _salt DeleteUserPool' {..} =
    _salt `Prelude.hashWithSalt` userPoolId

instance Prelude.NFData DeleteUserPool where
  rnf DeleteUserPool' {..} = Prelude.rnf userPoolId

instance Core.ToHeaders DeleteUserPool where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityProviderService.DeleteUserPool" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteUserPool where
  toJSON DeleteUserPool' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("UserPoolId" Core..= userPoolId)]
      )

instance Core.ToPath DeleteUserPool where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteUserPool where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteUserPoolResponse' smart constructor.
data DeleteUserPoolResponse = DeleteUserPoolResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteUserPoolResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteUserPoolResponse ::
  DeleteUserPoolResponse
newDeleteUserPoolResponse = DeleteUserPoolResponse'

instance Prelude.NFData DeleteUserPoolResponse where
  rnf _ = ()
