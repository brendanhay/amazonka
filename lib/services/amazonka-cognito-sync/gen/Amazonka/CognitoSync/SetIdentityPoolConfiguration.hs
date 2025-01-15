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
-- Module      : Amazonka.CognitoSync.SetIdentityPoolConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the necessary configuration for push sync.
--
-- This API can only be called with developer credentials. You cannot call
-- this API with the temporary user credentials provided by Cognito
-- Identity.
module Amazonka.CognitoSync.SetIdentityPoolConfiguration
  ( -- * Creating a Request
    SetIdentityPoolConfiguration (..),
    newSetIdentityPoolConfiguration,

    -- * Request Lenses
    setIdentityPoolConfiguration_cognitoStreams,
    setIdentityPoolConfiguration_pushSync,
    setIdentityPoolConfiguration_identityPoolId,

    -- * Destructuring the Response
    SetIdentityPoolConfigurationResponse (..),
    newSetIdentityPoolConfigurationResponse,

    -- * Response Lenses
    setIdentityPoolConfigurationResponse_cognitoStreams,
    setIdentityPoolConfigurationResponse_identityPoolId,
    setIdentityPoolConfigurationResponse_pushSync,
    setIdentityPoolConfigurationResponse_httpStatus,
  )
where

import Amazonka.CognitoSync.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The input for the SetIdentityPoolConfiguration operation.
--
-- /See:/ 'newSetIdentityPoolConfiguration' smart constructor.
data SetIdentityPoolConfiguration = SetIdentityPoolConfiguration'
  { -- | Options to apply to this identity pool for Amazon Cognito streams.
    cognitoStreams :: Prelude.Maybe CognitoStreams,
    -- | Options to apply to this identity pool for push synchronization.
    pushSync :: Prelude.Maybe PushSync,
    -- | A name-spaced GUID (for example,
    -- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
    -- Cognito. This is the ID of the pool to modify.
    identityPoolId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SetIdentityPoolConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cognitoStreams', 'setIdentityPoolConfiguration_cognitoStreams' - Options to apply to this identity pool for Amazon Cognito streams.
--
-- 'pushSync', 'setIdentityPoolConfiguration_pushSync' - Options to apply to this identity pool for push synchronization.
--
-- 'identityPoolId', 'setIdentityPoolConfiguration_identityPoolId' - A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. This is the ID of the pool to modify.
newSetIdentityPoolConfiguration ::
  -- | 'identityPoolId'
  Prelude.Text ->
  SetIdentityPoolConfiguration
newSetIdentityPoolConfiguration pIdentityPoolId_ =
  SetIdentityPoolConfiguration'
    { cognitoStreams =
        Prelude.Nothing,
      pushSync = Prelude.Nothing,
      identityPoolId = pIdentityPoolId_
    }

-- | Options to apply to this identity pool for Amazon Cognito streams.
setIdentityPoolConfiguration_cognitoStreams :: Lens.Lens' SetIdentityPoolConfiguration (Prelude.Maybe CognitoStreams)
setIdentityPoolConfiguration_cognitoStreams = Lens.lens (\SetIdentityPoolConfiguration' {cognitoStreams} -> cognitoStreams) (\s@SetIdentityPoolConfiguration' {} a -> s {cognitoStreams = a} :: SetIdentityPoolConfiguration)

-- | Options to apply to this identity pool for push synchronization.
setIdentityPoolConfiguration_pushSync :: Lens.Lens' SetIdentityPoolConfiguration (Prelude.Maybe PushSync)
setIdentityPoolConfiguration_pushSync = Lens.lens (\SetIdentityPoolConfiguration' {pushSync} -> pushSync) (\s@SetIdentityPoolConfiguration' {} a -> s {pushSync = a} :: SetIdentityPoolConfiguration)

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. This is the ID of the pool to modify.
setIdentityPoolConfiguration_identityPoolId :: Lens.Lens' SetIdentityPoolConfiguration Prelude.Text
setIdentityPoolConfiguration_identityPoolId = Lens.lens (\SetIdentityPoolConfiguration' {identityPoolId} -> identityPoolId) (\s@SetIdentityPoolConfiguration' {} a -> s {identityPoolId = a} :: SetIdentityPoolConfiguration)

instance Core.AWSRequest SetIdentityPoolConfiguration where
  type
    AWSResponse SetIdentityPoolConfiguration =
      SetIdentityPoolConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          SetIdentityPoolConfigurationResponse'
            Prelude.<$> (x Data..?> "CognitoStreams")
            Prelude.<*> (x Data..?> "IdentityPoolId")
            Prelude.<*> (x Data..?> "PushSync")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    SetIdentityPoolConfiguration
  where
  hashWithSalt _salt SetIdentityPoolConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` cognitoStreams
      `Prelude.hashWithSalt` pushSync
      `Prelude.hashWithSalt` identityPoolId

instance Prelude.NFData SetIdentityPoolConfiguration where
  rnf SetIdentityPoolConfiguration' {..} =
    Prelude.rnf cognitoStreams `Prelude.seq`
      Prelude.rnf pushSync `Prelude.seq`
        Prelude.rnf identityPoolId

instance Data.ToHeaders SetIdentityPoolConfiguration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON SetIdentityPoolConfiguration where
  toJSON SetIdentityPoolConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CognitoStreams" Data..=)
              Prelude.<$> cognitoStreams,
            ("PushSync" Data..=) Prelude.<$> pushSync
          ]
      )

instance Data.ToPath SetIdentityPoolConfiguration where
  toPath SetIdentityPoolConfiguration' {..} =
    Prelude.mconcat
      [ "/identitypools/",
        Data.toBS identityPoolId,
        "/configuration"
      ]

instance Data.ToQuery SetIdentityPoolConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | The output for the SetIdentityPoolConfiguration operation
--
-- /See:/ 'newSetIdentityPoolConfigurationResponse' smart constructor.
data SetIdentityPoolConfigurationResponse = SetIdentityPoolConfigurationResponse'
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
-- Create a value of 'SetIdentityPoolConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cognitoStreams', 'setIdentityPoolConfigurationResponse_cognitoStreams' - Options to apply to this identity pool for Amazon Cognito streams.
--
-- 'identityPoolId', 'setIdentityPoolConfigurationResponse_identityPoolId' - A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito.
--
-- 'pushSync', 'setIdentityPoolConfigurationResponse_pushSync' - Options to apply to this identity pool for push synchronization.
--
-- 'httpStatus', 'setIdentityPoolConfigurationResponse_httpStatus' - The response's http status code.
newSetIdentityPoolConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SetIdentityPoolConfigurationResponse
newSetIdentityPoolConfigurationResponse pHttpStatus_ =
  SetIdentityPoolConfigurationResponse'
    { cognitoStreams =
        Prelude.Nothing,
      identityPoolId = Prelude.Nothing,
      pushSync = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Options to apply to this identity pool for Amazon Cognito streams.
setIdentityPoolConfigurationResponse_cognitoStreams :: Lens.Lens' SetIdentityPoolConfigurationResponse (Prelude.Maybe CognitoStreams)
setIdentityPoolConfigurationResponse_cognitoStreams = Lens.lens (\SetIdentityPoolConfigurationResponse' {cognitoStreams} -> cognitoStreams) (\s@SetIdentityPoolConfigurationResponse' {} a -> s {cognitoStreams = a} :: SetIdentityPoolConfigurationResponse)

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito.
setIdentityPoolConfigurationResponse_identityPoolId :: Lens.Lens' SetIdentityPoolConfigurationResponse (Prelude.Maybe Prelude.Text)
setIdentityPoolConfigurationResponse_identityPoolId = Lens.lens (\SetIdentityPoolConfigurationResponse' {identityPoolId} -> identityPoolId) (\s@SetIdentityPoolConfigurationResponse' {} a -> s {identityPoolId = a} :: SetIdentityPoolConfigurationResponse)

-- | Options to apply to this identity pool for push synchronization.
setIdentityPoolConfigurationResponse_pushSync :: Lens.Lens' SetIdentityPoolConfigurationResponse (Prelude.Maybe PushSync)
setIdentityPoolConfigurationResponse_pushSync = Lens.lens (\SetIdentityPoolConfigurationResponse' {pushSync} -> pushSync) (\s@SetIdentityPoolConfigurationResponse' {} a -> s {pushSync = a} :: SetIdentityPoolConfigurationResponse)

-- | The response's http status code.
setIdentityPoolConfigurationResponse_httpStatus :: Lens.Lens' SetIdentityPoolConfigurationResponse Prelude.Int
setIdentityPoolConfigurationResponse_httpStatus = Lens.lens (\SetIdentityPoolConfigurationResponse' {httpStatus} -> httpStatus) (\s@SetIdentityPoolConfigurationResponse' {} a -> s {httpStatus = a} :: SetIdentityPoolConfigurationResponse)

instance
  Prelude.NFData
    SetIdentityPoolConfigurationResponse
  where
  rnf SetIdentityPoolConfigurationResponse' {..} =
    Prelude.rnf cognitoStreams `Prelude.seq`
      Prelude.rnf identityPoolId `Prelude.seq`
        Prelude.rnf pushSync `Prelude.seq`
          Prelude.rnf httpStatus
