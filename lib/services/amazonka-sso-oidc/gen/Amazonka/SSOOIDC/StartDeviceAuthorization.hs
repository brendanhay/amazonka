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
-- Module      : Amazonka.SSOOIDC.StartDeviceAuthorization
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Initiates device authorization by requesting a pair of verification
-- codes from the authorization service.
module Amazonka.SSOOIDC.StartDeviceAuthorization
  ( -- * Creating a Request
    StartDeviceAuthorization (..),
    newStartDeviceAuthorization,

    -- * Request Lenses
    startDeviceAuthorization_clientId,
    startDeviceAuthorization_clientSecret,
    startDeviceAuthorization_startUrl,

    -- * Destructuring the Response
    StartDeviceAuthorizationResponse (..),
    newStartDeviceAuthorizationResponse,

    -- * Response Lenses
    startDeviceAuthorizationResponse_deviceCode,
    startDeviceAuthorizationResponse_expiresIn,
    startDeviceAuthorizationResponse_interval,
    startDeviceAuthorizationResponse_userCode,
    startDeviceAuthorizationResponse_verificationUri,
    startDeviceAuthorizationResponse_verificationUriComplete,
    startDeviceAuthorizationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSOOIDC.Types

-- | /See:/ 'newStartDeviceAuthorization' smart constructor.
data StartDeviceAuthorization = StartDeviceAuthorization'
  { -- | The unique identifier string for the client that is registered with IAM
    -- Identity Center. This value should come from the persisted result of the
    -- RegisterClient API operation.
    clientId :: Prelude.Text,
    -- | A secret string that is generated for the client. This value should come
    -- from the persisted result of the RegisterClient API operation.
    clientSecret :: Prelude.Text,
    -- | The URL for the AWS access portal. For more information, see
    -- <https://docs.aws.amazon.com/singlesignon/latest/userguide/using-the-portal.html Using the AWS access portal>
    -- in the /IAM Identity Center User Guide/.
    startUrl :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartDeviceAuthorization' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientId', 'startDeviceAuthorization_clientId' - The unique identifier string for the client that is registered with IAM
-- Identity Center. This value should come from the persisted result of the
-- RegisterClient API operation.
--
-- 'clientSecret', 'startDeviceAuthorization_clientSecret' - A secret string that is generated for the client. This value should come
-- from the persisted result of the RegisterClient API operation.
--
-- 'startUrl', 'startDeviceAuthorization_startUrl' - The URL for the AWS access portal. For more information, see
-- <https://docs.aws.amazon.com/singlesignon/latest/userguide/using-the-portal.html Using the AWS access portal>
-- in the /IAM Identity Center User Guide/.
newStartDeviceAuthorization ::
  -- | 'clientId'
  Prelude.Text ->
  -- | 'clientSecret'
  Prelude.Text ->
  -- | 'startUrl'
  Prelude.Text ->
  StartDeviceAuthorization
newStartDeviceAuthorization
  pClientId_
  pClientSecret_
  pStartUrl_ =
    StartDeviceAuthorization'
      { clientId = pClientId_,
        clientSecret = pClientSecret_,
        startUrl = pStartUrl_
      }

-- | The unique identifier string for the client that is registered with IAM
-- Identity Center. This value should come from the persisted result of the
-- RegisterClient API operation.
startDeviceAuthorization_clientId :: Lens.Lens' StartDeviceAuthorization Prelude.Text
startDeviceAuthorization_clientId = Lens.lens (\StartDeviceAuthorization' {clientId} -> clientId) (\s@StartDeviceAuthorization' {} a -> s {clientId = a} :: StartDeviceAuthorization)

-- | A secret string that is generated for the client. This value should come
-- from the persisted result of the RegisterClient API operation.
startDeviceAuthorization_clientSecret :: Lens.Lens' StartDeviceAuthorization Prelude.Text
startDeviceAuthorization_clientSecret = Lens.lens (\StartDeviceAuthorization' {clientSecret} -> clientSecret) (\s@StartDeviceAuthorization' {} a -> s {clientSecret = a} :: StartDeviceAuthorization)

-- | The URL for the AWS access portal. For more information, see
-- <https://docs.aws.amazon.com/singlesignon/latest/userguide/using-the-portal.html Using the AWS access portal>
-- in the /IAM Identity Center User Guide/.
startDeviceAuthorization_startUrl :: Lens.Lens' StartDeviceAuthorization Prelude.Text
startDeviceAuthorization_startUrl = Lens.lens (\StartDeviceAuthorization' {startUrl} -> startUrl) (\s@StartDeviceAuthorization' {} a -> s {startUrl = a} :: StartDeviceAuthorization)

instance Core.AWSRequest StartDeviceAuthorization where
  type
    AWSResponse StartDeviceAuthorization =
      StartDeviceAuthorizationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartDeviceAuthorizationResponse'
            Prelude.<$> (x Data..?> "deviceCode")
            Prelude.<*> (x Data..?> "expiresIn")
            Prelude.<*> (x Data..?> "interval")
            Prelude.<*> (x Data..?> "userCode")
            Prelude.<*> (x Data..?> "verificationUri")
            Prelude.<*> (x Data..?> "verificationUriComplete")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartDeviceAuthorization where
  hashWithSalt _salt StartDeviceAuthorization' {..} =
    _salt `Prelude.hashWithSalt` clientId
      `Prelude.hashWithSalt` clientSecret
      `Prelude.hashWithSalt` startUrl

instance Prelude.NFData StartDeviceAuthorization where
  rnf StartDeviceAuthorization' {..} =
    Prelude.rnf clientId
      `Prelude.seq` Prelude.rnf clientSecret
      `Prelude.seq` Prelude.rnf startUrl

instance Data.ToHeaders StartDeviceAuthorization where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartDeviceAuthorization where
  toJSON StartDeviceAuthorization' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("clientId" Data..= clientId),
            Prelude.Just ("clientSecret" Data..= clientSecret),
            Prelude.Just ("startUrl" Data..= startUrl)
          ]
      )

instance Data.ToPath StartDeviceAuthorization where
  toPath = Prelude.const "/device_authorization"

instance Data.ToQuery StartDeviceAuthorization where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartDeviceAuthorizationResponse' smart constructor.
data StartDeviceAuthorizationResponse = StartDeviceAuthorizationResponse'
  { -- | The short-lived code that is used by the device when polling for a
    -- session token.
    deviceCode :: Prelude.Maybe Prelude.Text,
    -- | Indicates the number of seconds in which the verification code will
    -- become invalid.
    expiresIn :: Prelude.Maybe Prelude.Int,
    -- | Indicates the number of seconds the client must wait between attempts
    -- when polling for a session.
    interval :: Prelude.Maybe Prelude.Int,
    -- | A one-time user verification code. This is needed to authorize an in-use
    -- device.
    userCode :: Prelude.Maybe Prelude.Text,
    -- | The URI of the verification page that takes the @userCode@ to authorize
    -- the device.
    verificationUri :: Prelude.Maybe Prelude.Text,
    -- | An alternate URL that the client can use to automatically launch a
    -- browser. This process skips the manual step in which the user visits the
    -- verification page and enters their code.
    verificationUriComplete :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartDeviceAuthorizationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deviceCode', 'startDeviceAuthorizationResponse_deviceCode' - The short-lived code that is used by the device when polling for a
-- session token.
--
-- 'expiresIn', 'startDeviceAuthorizationResponse_expiresIn' - Indicates the number of seconds in which the verification code will
-- become invalid.
--
-- 'interval', 'startDeviceAuthorizationResponse_interval' - Indicates the number of seconds the client must wait between attempts
-- when polling for a session.
--
-- 'userCode', 'startDeviceAuthorizationResponse_userCode' - A one-time user verification code. This is needed to authorize an in-use
-- device.
--
-- 'verificationUri', 'startDeviceAuthorizationResponse_verificationUri' - The URI of the verification page that takes the @userCode@ to authorize
-- the device.
--
-- 'verificationUriComplete', 'startDeviceAuthorizationResponse_verificationUriComplete' - An alternate URL that the client can use to automatically launch a
-- browser. This process skips the manual step in which the user visits the
-- verification page and enters their code.
--
-- 'httpStatus', 'startDeviceAuthorizationResponse_httpStatus' - The response's http status code.
newStartDeviceAuthorizationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartDeviceAuthorizationResponse
newStartDeviceAuthorizationResponse pHttpStatus_ =
  StartDeviceAuthorizationResponse'
    { deviceCode =
        Prelude.Nothing,
      expiresIn = Prelude.Nothing,
      interval = Prelude.Nothing,
      userCode = Prelude.Nothing,
      verificationUri = Prelude.Nothing,
      verificationUriComplete = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The short-lived code that is used by the device when polling for a
-- session token.
startDeviceAuthorizationResponse_deviceCode :: Lens.Lens' StartDeviceAuthorizationResponse (Prelude.Maybe Prelude.Text)
startDeviceAuthorizationResponse_deviceCode = Lens.lens (\StartDeviceAuthorizationResponse' {deviceCode} -> deviceCode) (\s@StartDeviceAuthorizationResponse' {} a -> s {deviceCode = a} :: StartDeviceAuthorizationResponse)

-- | Indicates the number of seconds in which the verification code will
-- become invalid.
startDeviceAuthorizationResponse_expiresIn :: Lens.Lens' StartDeviceAuthorizationResponse (Prelude.Maybe Prelude.Int)
startDeviceAuthorizationResponse_expiresIn = Lens.lens (\StartDeviceAuthorizationResponse' {expiresIn} -> expiresIn) (\s@StartDeviceAuthorizationResponse' {} a -> s {expiresIn = a} :: StartDeviceAuthorizationResponse)

-- | Indicates the number of seconds the client must wait between attempts
-- when polling for a session.
startDeviceAuthorizationResponse_interval :: Lens.Lens' StartDeviceAuthorizationResponse (Prelude.Maybe Prelude.Int)
startDeviceAuthorizationResponse_interval = Lens.lens (\StartDeviceAuthorizationResponse' {interval} -> interval) (\s@StartDeviceAuthorizationResponse' {} a -> s {interval = a} :: StartDeviceAuthorizationResponse)

-- | A one-time user verification code. This is needed to authorize an in-use
-- device.
startDeviceAuthorizationResponse_userCode :: Lens.Lens' StartDeviceAuthorizationResponse (Prelude.Maybe Prelude.Text)
startDeviceAuthorizationResponse_userCode = Lens.lens (\StartDeviceAuthorizationResponse' {userCode} -> userCode) (\s@StartDeviceAuthorizationResponse' {} a -> s {userCode = a} :: StartDeviceAuthorizationResponse)

-- | The URI of the verification page that takes the @userCode@ to authorize
-- the device.
startDeviceAuthorizationResponse_verificationUri :: Lens.Lens' StartDeviceAuthorizationResponse (Prelude.Maybe Prelude.Text)
startDeviceAuthorizationResponse_verificationUri = Lens.lens (\StartDeviceAuthorizationResponse' {verificationUri} -> verificationUri) (\s@StartDeviceAuthorizationResponse' {} a -> s {verificationUri = a} :: StartDeviceAuthorizationResponse)

-- | An alternate URL that the client can use to automatically launch a
-- browser. This process skips the manual step in which the user visits the
-- verification page and enters their code.
startDeviceAuthorizationResponse_verificationUriComplete :: Lens.Lens' StartDeviceAuthorizationResponse (Prelude.Maybe Prelude.Text)
startDeviceAuthorizationResponse_verificationUriComplete = Lens.lens (\StartDeviceAuthorizationResponse' {verificationUriComplete} -> verificationUriComplete) (\s@StartDeviceAuthorizationResponse' {} a -> s {verificationUriComplete = a} :: StartDeviceAuthorizationResponse)

-- | The response's http status code.
startDeviceAuthorizationResponse_httpStatus :: Lens.Lens' StartDeviceAuthorizationResponse Prelude.Int
startDeviceAuthorizationResponse_httpStatus = Lens.lens (\StartDeviceAuthorizationResponse' {httpStatus} -> httpStatus) (\s@StartDeviceAuthorizationResponse' {} a -> s {httpStatus = a} :: StartDeviceAuthorizationResponse)

instance
  Prelude.NFData
    StartDeviceAuthorizationResponse
  where
  rnf StartDeviceAuthorizationResponse' {..} =
    Prelude.rnf deviceCode
      `Prelude.seq` Prelude.rnf expiresIn
      `Prelude.seq` Prelude.rnf interval
      `Prelude.seq` Prelude.rnf userCode
      `Prelude.seq` Prelude.rnf verificationUri
      `Prelude.seq` Prelude.rnf verificationUriComplete
      `Prelude.seq` Prelude.rnf httpStatus
