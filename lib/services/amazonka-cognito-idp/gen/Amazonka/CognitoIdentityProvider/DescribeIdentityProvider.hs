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
-- Module      : Amazonka.CognitoIdentityProvider.DescribeIdentityProvider
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a specific IdP.
module Amazonka.CognitoIdentityProvider.DescribeIdentityProvider
  ( -- * Creating a Request
    DescribeIdentityProvider (..),
    newDescribeIdentityProvider,

    -- * Request Lenses
    describeIdentityProvider_userPoolId,
    describeIdentityProvider_providerName,

    -- * Destructuring the Response
    DescribeIdentityProviderResponse (..),
    newDescribeIdentityProviderResponse,

    -- * Response Lenses
    describeIdentityProviderResponse_httpStatus,
    describeIdentityProviderResponse_identityProvider,
  )
where

import Amazonka.CognitoIdentityProvider.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeIdentityProvider' smart constructor.
data DescribeIdentityProvider = DescribeIdentityProvider'
  { -- | The user pool ID.
    userPoolId :: Prelude.Text,
    -- | The IdP name.
    providerName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeIdentityProvider' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userPoolId', 'describeIdentityProvider_userPoolId' - The user pool ID.
--
-- 'providerName', 'describeIdentityProvider_providerName' - The IdP name.
newDescribeIdentityProvider ::
  -- | 'userPoolId'
  Prelude.Text ->
  -- | 'providerName'
  Prelude.Text ->
  DescribeIdentityProvider
newDescribeIdentityProvider
  pUserPoolId_
  pProviderName_ =
    DescribeIdentityProvider'
      { userPoolId =
          pUserPoolId_,
        providerName = pProviderName_
      }

-- | The user pool ID.
describeIdentityProvider_userPoolId :: Lens.Lens' DescribeIdentityProvider Prelude.Text
describeIdentityProvider_userPoolId = Lens.lens (\DescribeIdentityProvider' {userPoolId} -> userPoolId) (\s@DescribeIdentityProvider' {} a -> s {userPoolId = a} :: DescribeIdentityProvider)

-- | The IdP name.
describeIdentityProvider_providerName :: Lens.Lens' DescribeIdentityProvider Prelude.Text
describeIdentityProvider_providerName = Lens.lens (\DescribeIdentityProvider' {providerName} -> providerName) (\s@DescribeIdentityProvider' {} a -> s {providerName = a} :: DescribeIdentityProvider)

instance Core.AWSRequest DescribeIdentityProvider where
  type
    AWSResponse DescribeIdentityProvider =
      DescribeIdentityProviderResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeIdentityProviderResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "IdentityProvider")
      )

instance Prelude.Hashable DescribeIdentityProvider where
  hashWithSalt _salt DescribeIdentityProvider' {..} =
    _salt `Prelude.hashWithSalt` userPoolId
      `Prelude.hashWithSalt` providerName

instance Prelude.NFData DescribeIdentityProvider where
  rnf DescribeIdentityProvider' {..} =
    Prelude.rnf userPoolId
      `Prelude.seq` Prelude.rnf providerName

instance Data.ToHeaders DescribeIdentityProvider where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSCognitoIdentityProviderService.DescribeIdentityProvider" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeIdentityProvider where
  toJSON DescribeIdentityProvider' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("UserPoolId" Data..= userPoolId),
            Prelude.Just ("ProviderName" Data..= providerName)
          ]
      )

instance Data.ToPath DescribeIdentityProvider where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeIdentityProvider where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeIdentityProviderResponse' smart constructor.
data DescribeIdentityProviderResponse = DescribeIdentityProviderResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The identity provider details.
    identityProvider :: IdentityProviderType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeIdentityProviderResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'describeIdentityProviderResponse_httpStatus' - The response's http status code.
--
-- 'identityProvider', 'describeIdentityProviderResponse_identityProvider' - The identity provider details.
newDescribeIdentityProviderResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'identityProvider'
  IdentityProviderType ->
  DescribeIdentityProviderResponse
newDescribeIdentityProviderResponse
  pHttpStatus_
  pIdentityProvider_ =
    DescribeIdentityProviderResponse'
      { httpStatus =
          pHttpStatus_,
        identityProvider = pIdentityProvider_
      }

-- | The response's http status code.
describeIdentityProviderResponse_httpStatus :: Lens.Lens' DescribeIdentityProviderResponse Prelude.Int
describeIdentityProviderResponse_httpStatus = Lens.lens (\DescribeIdentityProviderResponse' {httpStatus} -> httpStatus) (\s@DescribeIdentityProviderResponse' {} a -> s {httpStatus = a} :: DescribeIdentityProviderResponse)

-- | The identity provider details.
describeIdentityProviderResponse_identityProvider :: Lens.Lens' DescribeIdentityProviderResponse IdentityProviderType
describeIdentityProviderResponse_identityProvider = Lens.lens (\DescribeIdentityProviderResponse' {identityProvider} -> identityProvider) (\s@DescribeIdentityProviderResponse' {} a -> s {identityProvider = a} :: DescribeIdentityProviderResponse)

instance
  Prelude.NFData
    DescribeIdentityProviderResponse
  where
  rnf DescribeIdentityProviderResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf identityProvider
