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
-- Module      : Amazonka.CognitoIdentityProvider.DeleteResourceServer
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a resource server.
module Amazonka.CognitoIdentityProvider.DeleteResourceServer
  ( -- * Creating a Request
    DeleteResourceServer (..),
    newDeleteResourceServer,

    -- * Request Lenses
    deleteResourceServer_userPoolId,
    deleteResourceServer_identifier,

    -- * Destructuring the Response
    DeleteResourceServerResponse (..),
    newDeleteResourceServerResponse,
  )
where

import Amazonka.CognitoIdentityProvider.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteResourceServer' smart constructor.
data DeleteResourceServer = DeleteResourceServer'
  { -- | The user pool ID for the user pool that hosts the resource server.
    userPoolId :: Prelude.Text,
    -- | The identifier for the resource server.
    identifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteResourceServer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userPoolId', 'deleteResourceServer_userPoolId' - The user pool ID for the user pool that hosts the resource server.
--
-- 'identifier', 'deleteResourceServer_identifier' - The identifier for the resource server.
newDeleteResourceServer ::
  -- | 'userPoolId'
  Prelude.Text ->
  -- | 'identifier'
  Prelude.Text ->
  DeleteResourceServer
newDeleteResourceServer pUserPoolId_ pIdentifier_ =
  DeleteResourceServer'
    { userPoolId = pUserPoolId_,
      identifier = pIdentifier_
    }

-- | The user pool ID for the user pool that hosts the resource server.
deleteResourceServer_userPoolId :: Lens.Lens' DeleteResourceServer Prelude.Text
deleteResourceServer_userPoolId = Lens.lens (\DeleteResourceServer' {userPoolId} -> userPoolId) (\s@DeleteResourceServer' {} a -> s {userPoolId = a} :: DeleteResourceServer)

-- | The identifier for the resource server.
deleteResourceServer_identifier :: Lens.Lens' DeleteResourceServer Prelude.Text
deleteResourceServer_identifier = Lens.lens (\DeleteResourceServer' {identifier} -> identifier) (\s@DeleteResourceServer' {} a -> s {identifier = a} :: DeleteResourceServer)

instance Core.AWSRequest DeleteResourceServer where
  type
    AWSResponse DeleteResourceServer =
      DeleteResourceServerResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull DeleteResourceServerResponse'

instance Prelude.Hashable DeleteResourceServer where
  hashWithSalt _salt DeleteResourceServer' {..} =
    _salt
      `Prelude.hashWithSalt` userPoolId
      `Prelude.hashWithSalt` identifier

instance Prelude.NFData DeleteResourceServer where
  rnf DeleteResourceServer' {..} =
    Prelude.rnf userPoolId
      `Prelude.seq` Prelude.rnf identifier

instance Data.ToHeaders DeleteResourceServer where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSCognitoIdentityProviderService.DeleteResourceServer" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteResourceServer where
  toJSON DeleteResourceServer' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("UserPoolId" Data..= userPoolId),
            Prelude.Just ("Identifier" Data..= identifier)
          ]
      )

instance Data.ToPath DeleteResourceServer where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteResourceServer where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteResourceServerResponse' smart constructor.
data DeleteResourceServerResponse = DeleteResourceServerResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteResourceServerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteResourceServerResponse ::
  DeleteResourceServerResponse
newDeleteResourceServerResponse =
  DeleteResourceServerResponse'

instance Prelude.NFData DeleteResourceServerResponse where
  rnf _ = ()
