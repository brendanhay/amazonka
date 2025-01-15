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
-- Module      : Amazonka.CognitoSync.GetIdentityPoolConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the configuration settings of an identity pool.
--
-- This API can only be called with developer credentials. You cannot call
-- this API with the temporary user credentials provided by Cognito
-- Identity.
module Amazonka.CognitoSync.GetIdentityPoolConfiguration
  ( -- * Creating a Request
    GetIdentityPoolConfiguration (..),
    newGetIdentityPoolConfiguration,

    -- * Request Lenses
    getIdentityPoolConfiguration_identityPoolId,

    -- * Destructuring the Response
    GetIdentityPoolConfigurationResponse (..),
    newGetIdentityPoolConfigurationResponse,

    -- * Response Lenses
    getIdentityPoolConfigurationResponse_cognitoStreams,
    getIdentityPoolConfigurationResponse_identityPoolId,
    getIdentityPoolConfigurationResponse_pushSync,
    getIdentityPoolConfigurationResponse_httpStatus,
  )
where

import Amazonka.CognitoSync.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetIdentityPoolConfigurationResponse'
            Prelude.<$> (x Data..?> "CognitoStreams")
            Prelude.<*> (x Data..?> "IdentityPoolId")
            Prelude.<*> (x Data..?> "PushSync")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetIdentityPoolConfiguration
  where
  hashWithSalt _salt GetIdentityPoolConfiguration' {..} =
    _salt `Prelude.hashWithSalt` identityPoolId

instance Prelude.NFData GetIdentityPoolConfiguration where
  rnf GetIdentityPoolConfiguration' {..} =
    Prelude.rnf identityPoolId

instance Data.ToHeaders GetIdentityPoolConfiguration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetIdentityPoolConfiguration where
  toPath GetIdentityPoolConfiguration' {..} =
    Prelude.mconcat
      [ "/identitypools/",
        Data.toBS identityPoolId,
        "/configuration"
      ]

instance Data.ToQuery GetIdentityPoolConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | The output for the GetIdentityPoolConfiguration operation.
--
-- /See:/ 'newGetIdentityPoolConfigurationResponse' smart constructor.
data GetIdentityPoolConfigurationResponse = GetIdentityPoolConfigurationResponse'
  { -- | Options to apply to this identity pool for Amazon Cognito streams.
    cognitoStreams :: Prelude.Maybe CognitoStreams,
    -- | A name-spaced GUID (for example,
    -- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
    -- Cognito.
    identityPoolId :: Prelude.Maybe Prelude.Text,
    -- | Options to apply to this identity pool for push synchronization.
    pushSync :: Prelude.Maybe PushSync,
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
-- 'cognitoStreams', 'getIdentityPoolConfigurationResponse_cognitoStreams' - Options to apply to this identity pool for Amazon Cognito streams.
--
-- 'identityPoolId', 'getIdentityPoolConfigurationResponse_identityPoolId' - A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito.
--
-- 'pushSync', 'getIdentityPoolConfigurationResponse_pushSync' - Options to apply to this identity pool for push synchronization.
--
-- 'httpStatus', 'getIdentityPoolConfigurationResponse_httpStatus' - The response's http status code.
newGetIdentityPoolConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetIdentityPoolConfigurationResponse
newGetIdentityPoolConfigurationResponse pHttpStatus_ =
  GetIdentityPoolConfigurationResponse'
    { cognitoStreams =
        Prelude.Nothing,
      identityPoolId = Prelude.Nothing,
      pushSync = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Options to apply to this identity pool for Amazon Cognito streams.
getIdentityPoolConfigurationResponse_cognitoStreams :: Lens.Lens' GetIdentityPoolConfigurationResponse (Prelude.Maybe CognitoStreams)
getIdentityPoolConfigurationResponse_cognitoStreams = Lens.lens (\GetIdentityPoolConfigurationResponse' {cognitoStreams} -> cognitoStreams) (\s@GetIdentityPoolConfigurationResponse' {} a -> s {cognitoStreams = a} :: GetIdentityPoolConfigurationResponse)

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito.
getIdentityPoolConfigurationResponse_identityPoolId :: Lens.Lens' GetIdentityPoolConfigurationResponse (Prelude.Maybe Prelude.Text)
getIdentityPoolConfigurationResponse_identityPoolId = Lens.lens (\GetIdentityPoolConfigurationResponse' {identityPoolId} -> identityPoolId) (\s@GetIdentityPoolConfigurationResponse' {} a -> s {identityPoolId = a} :: GetIdentityPoolConfigurationResponse)

-- | Options to apply to this identity pool for push synchronization.
getIdentityPoolConfigurationResponse_pushSync :: Lens.Lens' GetIdentityPoolConfigurationResponse (Prelude.Maybe PushSync)
getIdentityPoolConfigurationResponse_pushSync = Lens.lens (\GetIdentityPoolConfigurationResponse' {pushSync} -> pushSync) (\s@GetIdentityPoolConfigurationResponse' {} a -> s {pushSync = a} :: GetIdentityPoolConfigurationResponse)

-- | The response's http status code.
getIdentityPoolConfigurationResponse_httpStatus :: Lens.Lens' GetIdentityPoolConfigurationResponse Prelude.Int
getIdentityPoolConfigurationResponse_httpStatus = Lens.lens (\GetIdentityPoolConfigurationResponse' {httpStatus} -> httpStatus) (\s@GetIdentityPoolConfigurationResponse' {} a -> s {httpStatus = a} :: GetIdentityPoolConfigurationResponse)

instance
  Prelude.NFData
    GetIdentityPoolConfigurationResponse
  where
  rnf GetIdentityPoolConfigurationResponse' {..} =
    Prelude.rnf cognitoStreams `Prelude.seq`
      Prelude.rnf identityPoolId `Prelude.seq`
        Prelude.rnf pushSync `Prelude.seq`
          Prelude.rnf httpStatus
