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
-- Module      : Network.AWS.CognitoIdentityProvider.DeleteIdentityProvider
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an identity provider for a user pool.
module Network.AWS.CognitoIdentityProvider.DeleteIdentityProvider
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

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteIdentityProvider' smart constructor.
data DeleteIdentityProvider = DeleteIdentityProvider'
  { -- | The user pool ID.
    userPoolId :: Core.Text,
    -- | The identity provider name.
    providerName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'providerName', 'deleteIdentityProvider_providerName' - The identity provider name.
newDeleteIdentityProvider ::
  -- | 'userPoolId'
  Core.Text ->
  -- | 'providerName'
  Core.Text ->
  DeleteIdentityProvider
newDeleteIdentityProvider pUserPoolId_ pProviderName_ =
  DeleteIdentityProvider'
    { userPoolId = pUserPoolId_,
      providerName = pProviderName_
    }

-- | The user pool ID.
deleteIdentityProvider_userPoolId :: Lens.Lens' DeleteIdentityProvider Core.Text
deleteIdentityProvider_userPoolId = Lens.lens (\DeleteIdentityProvider' {userPoolId} -> userPoolId) (\s@DeleteIdentityProvider' {} a -> s {userPoolId = a} :: DeleteIdentityProvider)

-- | The identity provider name.
deleteIdentityProvider_providerName :: Lens.Lens' DeleteIdentityProvider Core.Text
deleteIdentityProvider_providerName = Lens.lens (\DeleteIdentityProvider' {providerName} -> providerName) (\s@DeleteIdentityProvider' {} a -> s {providerName = a} :: DeleteIdentityProvider)

instance Core.AWSRequest DeleteIdentityProvider where
  type
    AWSResponse DeleteIdentityProvider =
      DeleteIdentityProviderResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      DeleteIdentityProviderResponse'

instance Core.Hashable DeleteIdentityProvider

instance Core.NFData DeleteIdentityProvider

instance Core.ToHeaders DeleteIdentityProvider where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityProviderService.DeleteIdentityProvider" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteIdentityProvider where
  toJSON DeleteIdentityProvider' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("UserPoolId" Core..= userPoolId),
            Core.Just ("ProviderName" Core..= providerName)
          ]
      )

instance Core.ToPath DeleteIdentityProvider where
  toPath = Core.const "/"

instance Core.ToQuery DeleteIdentityProvider where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteIdentityProviderResponse' smart constructor.
data DeleteIdentityProviderResponse = DeleteIdentityProviderResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteIdentityProviderResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteIdentityProviderResponse ::
  DeleteIdentityProviderResponse
newDeleteIdentityProviderResponse =
  DeleteIdentityProviderResponse'

instance Core.NFData DeleteIdentityProviderResponse
