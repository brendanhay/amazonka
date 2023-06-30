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
-- Module      : Amazonka.CognitoIdentityProvider.DeleteIdentityProvider
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an IdP for a user pool.
module Amazonka.CognitoIdentityProvider.DeleteIdentityProvider
  ( -- * Creating a Request
    DeleteIdentityProvider (..),
    newDeleteIdentityProvider,

    -- * Request Lenses
    deleteIdentityProvider_userPoolId,
    deleteIdentityProvider_providerName,

    -- * Destructuring the Response
    DeleteIdentityProviderResponse (..),
    newDeleteIdentityProviderResponse,
  )
where

import Amazonka.CognitoIdentityProvider.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteIdentityProvider' smart constructor.
data DeleteIdentityProvider = DeleteIdentityProvider'
  { -- | The user pool ID.
    userPoolId :: Prelude.Text,
    -- | The IdP name.
    providerName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteIdentityProvider' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userPoolId', 'deleteIdentityProvider_userPoolId' - The user pool ID.
--
-- 'providerName', 'deleteIdentityProvider_providerName' - The IdP name.
newDeleteIdentityProvider ::
  -- | 'userPoolId'
  Prelude.Text ->
  -- | 'providerName'
  Prelude.Text ->
  DeleteIdentityProvider
newDeleteIdentityProvider pUserPoolId_ pProviderName_ =
  DeleteIdentityProvider'
    { userPoolId = pUserPoolId_,
      providerName = pProviderName_
    }

-- | The user pool ID.
deleteIdentityProvider_userPoolId :: Lens.Lens' DeleteIdentityProvider Prelude.Text
deleteIdentityProvider_userPoolId = Lens.lens (\DeleteIdentityProvider' {userPoolId} -> userPoolId) (\s@DeleteIdentityProvider' {} a -> s {userPoolId = a} :: DeleteIdentityProvider)

-- | The IdP name.
deleteIdentityProvider_providerName :: Lens.Lens' DeleteIdentityProvider Prelude.Text
deleteIdentityProvider_providerName = Lens.lens (\DeleteIdentityProvider' {providerName} -> providerName) (\s@DeleteIdentityProvider' {} a -> s {providerName = a} :: DeleteIdentityProvider)

instance Core.AWSRequest DeleteIdentityProvider where
  type
    AWSResponse DeleteIdentityProvider =
      DeleteIdentityProviderResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      DeleteIdentityProviderResponse'

instance Prelude.Hashable DeleteIdentityProvider where
  hashWithSalt _salt DeleteIdentityProvider' {..} =
    _salt
      `Prelude.hashWithSalt` userPoolId
      `Prelude.hashWithSalt` providerName

instance Prelude.NFData DeleteIdentityProvider where
  rnf DeleteIdentityProvider' {..} =
    Prelude.rnf userPoolId
      `Prelude.seq` Prelude.rnf providerName

instance Data.ToHeaders DeleteIdentityProvider where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSCognitoIdentityProviderService.DeleteIdentityProvider" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteIdentityProvider where
  toJSON DeleteIdentityProvider' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("UserPoolId" Data..= userPoolId),
            Prelude.Just ("ProviderName" Data..= providerName)
          ]
      )

instance Data.ToPath DeleteIdentityProvider where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteIdentityProvider where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteIdentityProviderResponse' smart constructor.
data DeleteIdentityProviderResponse = DeleteIdentityProviderResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteIdentityProviderResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteIdentityProviderResponse ::
  DeleteIdentityProviderResponse
newDeleteIdentityProviderResponse =
  DeleteIdentityProviderResponse'

instance
  Prelude.NFData
    DeleteIdentityProviderResponse
  where
  rnf _ = ()
