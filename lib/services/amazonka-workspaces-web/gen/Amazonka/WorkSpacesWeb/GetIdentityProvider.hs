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
-- Module      : Amazonka.WorkSpacesWeb.GetIdentityProvider
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the identity provider.
module Amazonka.WorkSpacesWeb.GetIdentityProvider
  ( -- * Creating a Request
    GetIdentityProvider (..),
    newGetIdentityProvider,

    -- * Request Lenses
    getIdentityProvider_identityProviderArn,

    -- * Destructuring the Response
    GetIdentityProviderResponse (..),
    newGetIdentityProviderResponse,

    -- * Response Lenses
    getIdentityProviderResponse_identityProvider,
    getIdentityProviderResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkSpacesWeb.Types

-- | /See:/ 'newGetIdentityProvider' smart constructor.
data GetIdentityProvider = GetIdentityProvider'
  { -- | The ARN of the identity provider.
    identityProviderArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetIdentityProvider' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identityProviderArn', 'getIdentityProvider_identityProviderArn' - The ARN of the identity provider.
newGetIdentityProvider ::
  -- | 'identityProviderArn'
  Prelude.Text ->
  GetIdentityProvider
newGetIdentityProvider pIdentityProviderArn_ =
  GetIdentityProvider'
    { identityProviderArn =
        pIdentityProviderArn_
    }

-- | The ARN of the identity provider.
getIdentityProvider_identityProviderArn :: Lens.Lens' GetIdentityProvider Prelude.Text
getIdentityProvider_identityProviderArn = Lens.lens (\GetIdentityProvider' {identityProviderArn} -> identityProviderArn) (\s@GetIdentityProvider' {} a -> s {identityProviderArn = a} :: GetIdentityProvider)

instance Core.AWSRequest GetIdentityProvider where
  type
    AWSResponse GetIdentityProvider =
      GetIdentityProviderResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetIdentityProviderResponse'
            Prelude.<$> (x Core..?> "identityProvider")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetIdentityProvider where
  hashWithSalt _salt GetIdentityProvider' {..} =
    _salt `Prelude.hashWithSalt` identityProviderArn

instance Prelude.NFData GetIdentityProvider where
  rnf GetIdentityProvider' {..} =
    Prelude.rnf identityProviderArn

instance Core.ToHeaders GetIdentityProvider where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetIdentityProvider where
  toPath GetIdentityProvider' {..} =
    Prelude.mconcat
      [ "/identityProviders/",
        Core.toBS identityProviderArn
      ]

instance Core.ToQuery GetIdentityProvider where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetIdentityProviderResponse' smart constructor.
data GetIdentityProviderResponse = GetIdentityProviderResponse'
  { -- | The identity provider.
    identityProvider :: Prelude.Maybe IdentityProvider,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetIdentityProviderResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identityProvider', 'getIdentityProviderResponse_identityProvider' - The identity provider.
--
-- 'httpStatus', 'getIdentityProviderResponse_httpStatus' - The response's http status code.
newGetIdentityProviderResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetIdentityProviderResponse
newGetIdentityProviderResponse pHttpStatus_ =
  GetIdentityProviderResponse'
    { identityProvider =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identity provider.
getIdentityProviderResponse_identityProvider :: Lens.Lens' GetIdentityProviderResponse (Prelude.Maybe IdentityProvider)
getIdentityProviderResponse_identityProvider = Lens.lens (\GetIdentityProviderResponse' {identityProvider} -> identityProvider) (\s@GetIdentityProviderResponse' {} a -> s {identityProvider = a} :: GetIdentityProviderResponse)

-- | The response's http status code.
getIdentityProviderResponse_httpStatus :: Lens.Lens' GetIdentityProviderResponse Prelude.Int
getIdentityProviderResponse_httpStatus = Lens.lens (\GetIdentityProviderResponse' {httpStatus} -> httpStatus) (\s@GetIdentityProviderResponse' {} a -> s {httpStatus = a} :: GetIdentityProviderResponse)

instance Prelude.NFData GetIdentityProviderResponse where
  rnf GetIdentityProviderResponse' {..} =
    Prelude.rnf identityProvider
      `Prelude.seq` Prelude.rnf httpStatus
