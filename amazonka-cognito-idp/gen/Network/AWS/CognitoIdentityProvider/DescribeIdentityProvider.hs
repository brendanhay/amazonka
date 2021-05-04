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
-- Module      : Network.AWS.CognitoIdentityProvider.DescribeIdentityProvider
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a specific identity provider.
module Network.AWS.CognitoIdentityProvider.DescribeIdentityProvider
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

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeIdentityProvider' smart constructor.
data DescribeIdentityProvider = DescribeIdentityProvider'
  { -- | The user pool ID.
    userPoolId :: Prelude.Text,
    -- | The identity provider name.
    providerName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- 'providerName', 'describeIdentityProvider_providerName' - The identity provider name.
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

-- | The identity provider name.
describeIdentityProvider_providerName :: Lens.Lens' DescribeIdentityProvider Prelude.Text
describeIdentityProvider_providerName = Lens.lens (\DescribeIdentityProvider' {providerName} -> providerName) (\s@DescribeIdentityProvider' {} a -> s {providerName = a} :: DescribeIdentityProvider)

instance Prelude.AWSRequest DescribeIdentityProvider where
  type
    Rs DescribeIdentityProvider =
      DescribeIdentityProviderResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeIdentityProviderResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Prelude..:> "IdentityProvider")
      )

instance Prelude.Hashable DescribeIdentityProvider

instance Prelude.NFData DescribeIdentityProvider

instance Prelude.ToHeaders DescribeIdentityProvider where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSCognitoIdentityProviderService.DescribeIdentityProvider" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DescribeIdentityProvider where
  toJSON DescribeIdentityProvider' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("UserPoolId" Prelude..= userPoolId),
            Prelude.Just
              ("ProviderName" Prelude..= providerName)
          ]
      )

instance Prelude.ToPath DescribeIdentityProvider where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeIdentityProvider where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeIdentityProviderResponse' smart constructor.
data DescribeIdentityProviderResponse = DescribeIdentityProviderResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The identity provider that was deleted.
    identityProvider :: IdentityProviderType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- 'identityProvider', 'describeIdentityProviderResponse_identityProvider' - The identity provider that was deleted.
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

-- | The identity provider that was deleted.
describeIdentityProviderResponse_identityProvider :: Lens.Lens' DescribeIdentityProviderResponse IdentityProviderType
describeIdentityProviderResponse_identityProvider = Lens.lens (\DescribeIdentityProviderResponse' {identityProvider} -> identityProvider) (\s@DescribeIdentityProviderResponse' {} a -> s {identityProvider = a} :: DescribeIdentityProviderResponse)

instance
  Prelude.NFData
    DescribeIdentityProviderResponse
