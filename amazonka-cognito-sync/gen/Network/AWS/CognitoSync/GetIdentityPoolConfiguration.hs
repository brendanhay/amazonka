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
-- Module      : Network.AWS.CognitoSync.GetIdentityPoolConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the configuration settings of an identity pool.
--
-- This API can only be called with developer credentials. You cannot call
-- this API with the temporary user credentials provided by Cognito
-- Identity.
module Network.AWS.CognitoSync.GetIdentityPoolConfiguration
  ( -- * Creating a Request
    GetIdentityPoolConfiguration (..),
    newGetIdentityPoolConfiguration,

    -- * Request Lenses
    getIdentityPoolConfiguration_identityPoolId,

    -- * Destructuring the Response
    GetIdentityPoolConfigurationResponse (..),
    newGetIdentityPoolConfigurationResponse,

    -- * Response Lenses
    getIdentityPoolConfigurationResponse_identityPoolId,
    getIdentityPoolConfigurationResponse_pushSync,
    getIdentityPoolConfigurationResponse_cognitoStreams,
    getIdentityPoolConfigurationResponse_httpStatus,
  )
where

import Network.AWS.CognitoSync.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the GetIdentityPoolConfiguration operation.
--
-- /See:/ 'newGetIdentityPoolConfiguration' smart constructor.
data GetIdentityPoolConfiguration = GetIdentityPoolConfiguration'
  { -- | A name-spaced GUID (for example,
    -- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
    -- Cognito. This is the ID of the pool for which to return a configuration.
    identityPoolId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetIdentityPoolConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identityPoolId', 'getIdentityPoolConfiguration_identityPoolId' - A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. This is the ID of the pool for which to return a configuration.
newGetIdentityPoolConfiguration ::
  -- | 'identityPoolId'
  Prelude.Text ->
  GetIdentityPoolConfiguration
newGetIdentityPoolConfiguration pIdentityPoolId_ =
  GetIdentityPoolConfiguration'
    { identityPoolId =
        pIdentityPoolId_
    }

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. This is the ID of the pool for which to return a configuration.
getIdentityPoolConfiguration_identityPoolId :: Lens.Lens' GetIdentityPoolConfiguration Prelude.Text
getIdentityPoolConfiguration_identityPoolId = Lens.lens (\GetIdentityPoolConfiguration' {identityPoolId} -> identityPoolId) (\s@GetIdentityPoolConfiguration' {} a -> s {identityPoolId = a} :: GetIdentityPoolConfiguration)

instance Core.AWSRequest GetIdentityPoolConfiguration where
  type
    AWSResponse GetIdentityPoolConfiguration =
      GetIdentityPoolConfigurationResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetIdentityPoolConfigurationResponse'
            Prelude.<$> (x Core..?> "IdentityPoolId")
            Prelude.<*> (x Core..?> "PushSync")
            Prelude.<*> (x Core..?> "CognitoStreams")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetIdentityPoolConfiguration

instance Prelude.NFData GetIdentityPoolConfiguration

instance Core.ToHeaders GetIdentityPoolConfiguration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetIdentityPoolConfiguration where
  toPath GetIdentityPoolConfiguration' {..} =
    Prelude.mconcat
      [ "/identitypools/",
        Core.toBS identityPoolId,
        "/configuration"
      ]

instance Core.ToQuery GetIdentityPoolConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | The output for the GetIdentityPoolConfiguration operation.
--
-- /See:/ 'newGetIdentityPoolConfigurationResponse' smart constructor.
data GetIdentityPoolConfigurationResponse = GetIdentityPoolConfigurationResponse'
  { -- | A name-spaced GUID (for example,
    -- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
    -- Cognito.
    identityPoolId :: Prelude.Maybe Prelude.Text,
    -- | Options to apply to this identity pool for push synchronization.
    pushSync :: Prelude.Maybe PushSync,
    -- | Options to apply to this identity pool for Amazon Cognito streams.
    cognitoStreams :: Prelude.Maybe CognitoStreams,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetIdentityPoolConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identityPoolId', 'getIdentityPoolConfigurationResponse_identityPoolId' - A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito.
--
-- 'pushSync', 'getIdentityPoolConfigurationResponse_pushSync' - Options to apply to this identity pool for push synchronization.
--
-- 'cognitoStreams', 'getIdentityPoolConfigurationResponse_cognitoStreams' - Options to apply to this identity pool for Amazon Cognito streams.
--
-- 'httpStatus', 'getIdentityPoolConfigurationResponse_httpStatus' - The response's http status code.
newGetIdentityPoolConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetIdentityPoolConfigurationResponse
newGetIdentityPoolConfigurationResponse pHttpStatus_ =
  GetIdentityPoolConfigurationResponse'
    { identityPoolId =
        Prelude.Nothing,
      pushSync = Prelude.Nothing,
      cognitoStreams = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito.
getIdentityPoolConfigurationResponse_identityPoolId :: Lens.Lens' GetIdentityPoolConfigurationResponse (Prelude.Maybe Prelude.Text)
getIdentityPoolConfigurationResponse_identityPoolId = Lens.lens (\GetIdentityPoolConfigurationResponse' {identityPoolId} -> identityPoolId) (\s@GetIdentityPoolConfigurationResponse' {} a -> s {identityPoolId = a} :: GetIdentityPoolConfigurationResponse)

-- | Options to apply to this identity pool for push synchronization.
getIdentityPoolConfigurationResponse_pushSync :: Lens.Lens' GetIdentityPoolConfigurationResponse (Prelude.Maybe PushSync)
getIdentityPoolConfigurationResponse_pushSync = Lens.lens (\GetIdentityPoolConfigurationResponse' {pushSync} -> pushSync) (\s@GetIdentityPoolConfigurationResponse' {} a -> s {pushSync = a} :: GetIdentityPoolConfigurationResponse)

-- | Options to apply to this identity pool for Amazon Cognito streams.
getIdentityPoolConfigurationResponse_cognitoStreams :: Lens.Lens' GetIdentityPoolConfigurationResponse (Prelude.Maybe CognitoStreams)
getIdentityPoolConfigurationResponse_cognitoStreams = Lens.lens (\GetIdentityPoolConfigurationResponse' {cognitoStreams} -> cognitoStreams) (\s@GetIdentityPoolConfigurationResponse' {} a -> s {cognitoStreams = a} :: GetIdentityPoolConfigurationResponse)

-- | The response's http status code.
getIdentityPoolConfigurationResponse_httpStatus :: Lens.Lens' GetIdentityPoolConfigurationResponse Prelude.Int
getIdentityPoolConfigurationResponse_httpStatus = Lens.lens (\GetIdentityPoolConfigurationResponse' {httpStatus} -> httpStatus) (\s@GetIdentityPoolConfigurationResponse' {} a -> s {httpStatus = a} :: GetIdentityPoolConfigurationResponse)

instance
  Prelude.NFData
    GetIdentityPoolConfigurationResponse
