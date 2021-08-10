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
-- Module      : Network.AWS.CognitoSync.SetIdentityPoolConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the necessary configuration for push sync.
--
-- This API can only be called with developer credentials. You cannot call
-- this API with the temporary user credentials provided by Cognito
-- Identity.
module Network.AWS.CognitoSync.SetIdentityPoolConfiguration
  ( -- * Creating a Request
    SetIdentityPoolConfiguration (..),
    newSetIdentityPoolConfiguration,

    -- * Request Lenses
    setIdentityPoolConfiguration_pushSync,
    setIdentityPoolConfiguration_cognitoStreams,
    setIdentityPoolConfiguration_identityPoolId,

    -- * Destructuring the Response
    SetIdentityPoolConfigurationResponse (..),
    newSetIdentityPoolConfigurationResponse,

    -- * Response Lenses
    setIdentityPoolConfigurationResponse_identityPoolId,
    setIdentityPoolConfigurationResponse_pushSync,
    setIdentityPoolConfigurationResponse_cognitoStreams,
    setIdentityPoolConfigurationResponse_httpStatus,
  )
where

import Network.AWS.CognitoSync.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the SetIdentityPoolConfiguration operation.
--
-- /See:/ 'newSetIdentityPoolConfiguration' smart constructor.
data SetIdentityPoolConfiguration = SetIdentityPoolConfiguration'
  { -- | Options to apply to this identity pool for push synchronization.
    pushSync :: Prelude.Maybe PushSync,
    -- | Options to apply to this identity pool for Amazon Cognito streams.
    cognitoStreams :: Prelude.Maybe CognitoStreams,
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
-- 'pushSync', 'setIdentityPoolConfiguration_pushSync' - Options to apply to this identity pool for push synchronization.
--
-- 'cognitoStreams', 'setIdentityPoolConfiguration_cognitoStreams' - Options to apply to this identity pool for Amazon Cognito streams.
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
    { pushSync =
        Prelude.Nothing,
      cognitoStreams = Prelude.Nothing,
      identityPoolId = pIdentityPoolId_
    }

-- | Options to apply to this identity pool for push synchronization.
setIdentityPoolConfiguration_pushSync :: Lens.Lens' SetIdentityPoolConfiguration (Prelude.Maybe PushSync)
setIdentityPoolConfiguration_pushSync = Lens.lens (\SetIdentityPoolConfiguration' {pushSync} -> pushSync) (\s@SetIdentityPoolConfiguration' {} a -> s {pushSync = a} :: SetIdentityPoolConfiguration)

-- | Options to apply to this identity pool for Amazon Cognito streams.
setIdentityPoolConfiguration_cognitoStreams :: Lens.Lens' SetIdentityPoolConfiguration (Prelude.Maybe CognitoStreams)
setIdentityPoolConfiguration_cognitoStreams = Lens.lens (\SetIdentityPoolConfiguration' {cognitoStreams} -> cognitoStreams) (\s@SetIdentityPoolConfiguration' {} a -> s {cognitoStreams = a} :: SetIdentityPoolConfiguration)

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. This is the ID of the pool to modify.
setIdentityPoolConfiguration_identityPoolId :: Lens.Lens' SetIdentityPoolConfiguration Prelude.Text
setIdentityPoolConfiguration_identityPoolId = Lens.lens (\SetIdentityPoolConfiguration' {identityPoolId} -> identityPoolId) (\s@SetIdentityPoolConfiguration' {} a -> s {identityPoolId = a} :: SetIdentityPoolConfiguration)

instance Core.AWSRequest SetIdentityPoolConfiguration where
  type
    AWSResponse SetIdentityPoolConfiguration =
      SetIdentityPoolConfigurationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          SetIdentityPoolConfigurationResponse'
            Prelude.<$> (x Core..?> "IdentityPoolId")
            Prelude.<*> (x Core..?> "PushSync")
            Prelude.<*> (x Core..?> "CognitoStreams")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    SetIdentityPoolConfiguration

instance Prelude.NFData SetIdentityPoolConfiguration

instance Core.ToHeaders SetIdentityPoolConfiguration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON SetIdentityPoolConfiguration where
  toJSON SetIdentityPoolConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("PushSync" Core..=) Prelude.<$> pushSync,
            ("CognitoStreams" Core..=)
              Prelude.<$> cognitoStreams
          ]
      )

instance Core.ToPath SetIdentityPoolConfiguration where
  toPath SetIdentityPoolConfiguration' {..} =
    Prelude.mconcat
      [ "/identitypools/",
        Core.toBS identityPoolId,
        "/configuration"
      ]

instance Core.ToQuery SetIdentityPoolConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | The output for the SetIdentityPoolConfiguration operation
--
-- /See:/ 'newSetIdentityPoolConfigurationResponse' smart constructor.
data SetIdentityPoolConfigurationResponse = SetIdentityPoolConfigurationResponse'
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
-- Create a value of 'SetIdentityPoolConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identityPoolId', 'setIdentityPoolConfigurationResponse_identityPoolId' - A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito.
--
-- 'pushSync', 'setIdentityPoolConfigurationResponse_pushSync' - Options to apply to this identity pool for push synchronization.
--
-- 'cognitoStreams', 'setIdentityPoolConfigurationResponse_cognitoStreams' - Options to apply to this identity pool for Amazon Cognito streams.
--
-- 'httpStatus', 'setIdentityPoolConfigurationResponse_httpStatus' - The response's http status code.
newSetIdentityPoolConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SetIdentityPoolConfigurationResponse
newSetIdentityPoolConfigurationResponse pHttpStatus_ =
  SetIdentityPoolConfigurationResponse'
    { identityPoolId =
        Prelude.Nothing,
      pushSync = Prelude.Nothing,
      cognitoStreams = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito.
setIdentityPoolConfigurationResponse_identityPoolId :: Lens.Lens' SetIdentityPoolConfigurationResponse (Prelude.Maybe Prelude.Text)
setIdentityPoolConfigurationResponse_identityPoolId = Lens.lens (\SetIdentityPoolConfigurationResponse' {identityPoolId} -> identityPoolId) (\s@SetIdentityPoolConfigurationResponse' {} a -> s {identityPoolId = a} :: SetIdentityPoolConfigurationResponse)

-- | Options to apply to this identity pool for push synchronization.
setIdentityPoolConfigurationResponse_pushSync :: Lens.Lens' SetIdentityPoolConfigurationResponse (Prelude.Maybe PushSync)
setIdentityPoolConfigurationResponse_pushSync = Lens.lens (\SetIdentityPoolConfigurationResponse' {pushSync} -> pushSync) (\s@SetIdentityPoolConfigurationResponse' {} a -> s {pushSync = a} :: SetIdentityPoolConfigurationResponse)

-- | Options to apply to this identity pool for Amazon Cognito streams.
setIdentityPoolConfigurationResponse_cognitoStreams :: Lens.Lens' SetIdentityPoolConfigurationResponse (Prelude.Maybe CognitoStreams)
setIdentityPoolConfigurationResponse_cognitoStreams = Lens.lens (\SetIdentityPoolConfigurationResponse' {cognitoStreams} -> cognitoStreams) (\s@SetIdentityPoolConfigurationResponse' {} a -> s {cognitoStreams = a} :: SetIdentityPoolConfigurationResponse)

-- | The response's http status code.
setIdentityPoolConfigurationResponse_httpStatus :: Lens.Lens' SetIdentityPoolConfigurationResponse Prelude.Int
setIdentityPoolConfigurationResponse_httpStatus = Lens.lens (\SetIdentityPoolConfigurationResponse' {httpStatus} -> httpStatus) (\s@SetIdentityPoolConfigurationResponse' {} a -> s {httpStatus = a} :: SetIdentityPoolConfigurationResponse)

instance
  Prelude.NFData
    SetIdentityPoolConfigurationResponse
