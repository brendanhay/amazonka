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
-- Module      : Amazonka.WorkSpacesWeb.DeleteIdentityProvider
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the identity provider.
module Amazonka.WorkSpacesWeb.DeleteIdentityProvider
  ( -- * Creating a Request
    DeleteIdentityProvider (..),
    newDeleteIdentityProvider,

    -- * Request Lenses
    deleteIdentityProvider_identityProviderArn,

    -- * Destructuring the Response
    DeleteIdentityProviderResponse (..),
    newDeleteIdentityProviderResponse,

    -- * Response Lenses
    deleteIdentityProviderResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkSpacesWeb.Types

-- | /See:/ 'newDeleteIdentityProvider' smart constructor.
data DeleteIdentityProvider = DeleteIdentityProvider'
  { -- | The ARN of the identity provider.
    identityProviderArn :: Prelude.Text
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
-- 'identityProviderArn', 'deleteIdentityProvider_identityProviderArn' - The ARN of the identity provider.
newDeleteIdentityProvider ::
  -- | 'identityProviderArn'
  Prelude.Text ->
  DeleteIdentityProvider
newDeleteIdentityProvider pIdentityProviderArn_ =
  DeleteIdentityProvider'
    { identityProviderArn =
        pIdentityProviderArn_
    }

-- | The ARN of the identity provider.
deleteIdentityProvider_identityProviderArn :: Lens.Lens' DeleteIdentityProvider Prelude.Text
deleteIdentityProvider_identityProviderArn = Lens.lens (\DeleteIdentityProvider' {identityProviderArn} -> identityProviderArn) (\s@DeleteIdentityProvider' {} a -> s {identityProviderArn = a} :: DeleteIdentityProvider)

instance Core.AWSRequest DeleteIdentityProvider where
  type
    AWSResponse DeleteIdentityProvider =
      DeleteIdentityProviderResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteIdentityProviderResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteIdentityProvider where
  hashWithSalt _salt DeleteIdentityProvider' {..} =
    _salt `Prelude.hashWithSalt` identityProviderArn

instance Prelude.NFData DeleteIdentityProvider where
  rnf DeleteIdentityProvider' {..} =
    Prelude.rnf identityProviderArn

instance Data.ToHeaders DeleteIdentityProvider where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteIdentityProvider where
  toPath DeleteIdentityProvider' {..} =
    Prelude.mconcat
      [ "/identityProviders/",
        Data.toBS identityProviderArn
      ]

instance Data.ToQuery DeleteIdentityProvider where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteIdentityProviderResponse' smart constructor.
data DeleteIdentityProviderResponse = DeleteIdentityProviderResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteIdentityProviderResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteIdentityProviderResponse_httpStatus' - The response's http status code.
newDeleteIdentityProviderResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteIdentityProviderResponse
newDeleteIdentityProviderResponse pHttpStatus_ =
  DeleteIdentityProviderResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteIdentityProviderResponse_httpStatus :: Lens.Lens' DeleteIdentityProviderResponse Prelude.Int
deleteIdentityProviderResponse_httpStatus = Lens.lens (\DeleteIdentityProviderResponse' {httpStatus} -> httpStatus) (\s@DeleteIdentityProviderResponse' {} a -> s {httpStatus = a} :: DeleteIdentityProviderResponse)

instance
  Prelude.NFData
    DeleteIdentityProviderResponse
  where
  rnf DeleteIdentityProviderResponse' {..} =
    Prelude.rnf httpStatus
