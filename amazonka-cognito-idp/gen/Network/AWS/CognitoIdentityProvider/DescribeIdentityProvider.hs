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
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeIdentityProvider' smart constructor.
data DescribeIdentityProvider = DescribeIdentityProvider'
  { -- | The user pool ID.
    userPoolId :: Core.Text,
    -- | The identity provider name.
    providerName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'providerName'
  Core.Text ->
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
describeIdentityProvider_userPoolId :: Lens.Lens' DescribeIdentityProvider Core.Text
describeIdentityProvider_userPoolId = Lens.lens (\DescribeIdentityProvider' {userPoolId} -> userPoolId) (\s@DescribeIdentityProvider' {} a -> s {userPoolId = a} :: DescribeIdentityProvider)

-- | The identity provider name.
describeIdentityProvider_providerName :: Lens.Lens' DescribeIdentityProvider Core.Text
describeIdentityProvider_providerName = Lens.lens (\DescribeIdentityProvider' {providerName} -> providerName) (\s@DescribeIdentityProvider' {} a -> s {providerName = a} :: DescribeIdentityProvider)

instance Core.AWSRequest DescribeIdentityProvider where
  type
    AWSResponse DescribeIdentityProvider =
      DescribeIdentityProviderResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeIdentityProviderResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "IdentityProvider")
      )

instance Core.Hashable DescribeIdentityProvider

instance Core.NFData DescribeIdentityProvider

instance Core.ToHeaders DescribeIdentityProvider where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityProviderService.DescribeIdentityProvider" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeIdentityProvider where
  toJSON DescribeIdentityProvider' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("UserPoolId" Core..= userPoolId),
            Core.Just ("ProviderName" Core..= providerName)
          ]
      )

instance Core.ToPath DescribeIdentityProvider where
  toPath = Core.const "/"

instance Core.ToQuery DescribeIdentityProvider where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeIdentityProviderResponse' smart constructor.
data DescribeIdentityProviderResponse = DescribeIdentityProviderResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The identity provider that was deleted.
    identityProvider :: IdentityProviderType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
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
describeIdentityProviderResponse_httpStatus :: Lens.Lens' DescribeIdentityProviderResponse Core.Int
describeIdentityProviderResponse_httpStatus = Lens.lens (\DescribeIdentityProviderResponse' {httpStatus} -> httpStatus) (\s@DescribeIdentityProviderResponse' {} a -> s {httpStatus = a} :: DescribeIdentityProviderResponse)

-- | The identity provider that was deleted.
describeIdentityProviderResponse_identityProvider :: Lens.Lens' DescribeIdentityProviderResponse IdentityProviderType
describeIdentityProviderResponse_identityProvider = Lens.lens (\DescribeIdentityProviderResponse' {identityProvider} -> identityProvider) (\s@DescribeIdentityProviderResponse' {} a -> s {identityProvider = a} :: DescribeIdentityProviderResponse)

instance Core.NFData DescribeIdentityProviderResponse
