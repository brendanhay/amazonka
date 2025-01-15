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
-- Module      : Amazonka.GameLift.GetComputeAuthToken
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Requests an authorization token from GameLift. The authorization token
-- is used by your game server to authenticate with GameLift. Each
-- authentication token has an expiration token. To continue using the
-- compute resource to host your game server, regularly retrieve a new
-- authorization token.
module Amazonka.GameLift.GetComputeAuthToken
  ( -- * Creating a Request
    GetComputeAuthToken (..),
    newGetComputeAuthToken,

    -- * Request Lenses
    getComputeAuthToken_fleetId,
    getComputeAuthToken_computeName,

    -- * Destructuring the Response
    GetComputeAuthTokenResponse (..),
    newGetComputeAuthTokenResponse,

    -- * Response Lenses
    getComputeAuthTokenResponse_authToken,
    getComputeAuthTokenResponse_computeArn,
    getComputeAuthTokenResponse_computeName,
    getComputeAuthTokenResponse_expirationTimestamp,
    getComputeAuthTokenResponse_fleetArn,
    getComputeAuthTokenResponse_fleetId,
    getComputeAuthTokenResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GameLift.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetComputeAuthToken' smart constructor.
data GetComputeAuthToken = GetComputeAuthToken'
  { -- | A unique identifier for the fleet that the compute is registered to.
    fleetId :: Prelude.Text,
    -- | The name of the compute resource you are requesting the authorization
    -- token for.
    computeName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetComputeAuthToken' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fleetId', 'getComputeAuthToken_fleetId' - A unique identifier for the fleet that the compute is registered to.
--
-- 'computeName', 'getComputeAuthToken_computeName' - The name of the compute resource you are requesting the authorization
-- token for.
newGetComputeAuthToken ::
  -- | 'fleetId'
  Prelude.Text ->
  -- | 'computeName'
  Prelude.Text ->
  GetComputeAuthToken
newGetComputeAuthToken pFleetId_ pComputeName_ =
  GetComputeAuthToken'
    { fleetId = pFleetId_,
      computeName = pComputeName_
    }

-- | A unique identifier for the fleet that the compute is registered to.
getComputeAuthToken_fleetId :: Lens.Lens' GetComputeAuthToken Prelude.Text
getComputeAuthToken_fleetId = Lens.lens (\GetComputeAuthToken' {fleetId} -> fleetId) (\s@GetComputeAuthToken' {} a -> s {fleetId = a} :: GetComputeAuthToken)

-- | The name of the compute resource you are requesting the authorization
-- token for.
getComputeAuthToken_computeName :: Lens.Lens' GetComputeAuthToken Prelude.Text
getComputeAuthToken_computeName = Lens.lens (\GetComputeAuthToken' {computeName} -> computeName) (\s@GetComputeAuthToken' {} a -> s {computeName = a} :: GetComputeAuthToken)

instance Core.AWSRequest GetComputeAuthToken where
  type
    AWSResponse GetComputeAuthToken =
      GetComputeAuthTokenResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetComputeAuthTokenResponse'
            Prelude.<$> (x Data..?> "AuthToken")
            Prelude.<*> (x Data..?> "ComputeArn")
            Prelude.<*> (x Data..?> "ComputeName")
            Prelude.<*> (x Data..?> "ExpirationTimestamp")
            Prelude.<*> (x Data..?> "FleetArn")
            Prelude.<*> (x Data..?> "FleetId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetComputeAuthToken where
  hashWithSalt _salt GetComputeAuthToken' {..} =
    _salt
      `Prelude.hashWithSalt` fleetId
      `Prelude.hashWithSalt` computeName

instance Prelude.NFData GetComputeAuthToken where
  rnf GetComputeAuthToken' {..} =
    Prelude.rnf fleetId `Prelude.seq`
      Prelude.rnf computeName

instance Data.ToHeaders GetComputeAuthToken where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "GameLift.GetComputeAuthToken" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetComputeAuthToken where
  toJSON GetComputeAuthToken' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("FleetId" Data..= fleetId),
            Prelude.Just ("ComputeName" Data..= computeName)
          ]
      )

instance Data.ToPath GetComputeAuthToken where
  toPath = Prelude.const "/"

instance Data.ToQuery GetComputeAuthToken where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetComputeAuthTokenResponse' smart constructor.
data GetComputeAuthTokenResponse = GetComputeAuthTokenResponse'
  { -- | The authorization token that your game server uses to authenticate with
    -- GameLift.
    authToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name
    -- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
    -- that is assigned to a GameLift compute resource and uniquely identifies
    -- it. ARNs are unique across all Regions. Format is
    -- @arn:aws:gamelift:\<region>::compute\/compute-a1234567-b8c9-0d1e-2fa3-b45c6d7e8912@
    computeArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the compute resource you are requesting the authorization
    -- token for.
    computeName :: Prelude.Maybe Prelude.Text,
    -- | The amount of time until the authorization token is no longer valid. To
    -- continue using the compute resource for game server hosting, renew the
    -- authorization token by using this operation again.
    expirationTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The Amazon Resource Name
    -- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
    -- that is assigned to a GameLift fleet resource and uniquely identifies
    -- it. ARNs are unique across all Regions. Format is
    -- @arn:aws:gamelift:\<region>::fleet\/fleet-a1234567-b8c9-0d1e-2fa3-b45c6d7e8912@.
    fleetArn :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the fleet that the compute is registered to.
    fleetId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetComputeAuthTokenResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authToken', 'getComputeAuthTokenResponse_authToken' - The authorization token that your game server uses to authenticate with
-- GameLift.
--
-- 'computeArn', 'getComputeAuthTokenResponse_computeArn' - The Amazon Resource Name
-- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
-- that is assigned to a GameLift compute resource and uniquely identifies
-- it. ARNs are unique across all Regions. Format is
-- @arn:aws:gamelift:\<region>::compute\/compute-a1234567-b8c9-0d1e-2fa3-b45c6d7e8912@
--
-- 'computeName', 'getComputeAuthTokenResponse_computeName' - The name of the compute resource you are requesting the authorization
-- token for.
--
-- 'expirationTimestamp', 'getComputeAuthTokenResponse_expirationTimestamp' - The amount of time until the authorization token is no longer valid. To
-- continue using the compute resource for game server hosting, renew the
-- authorization token by using this operation again.
--
-- 'fleetArn', 'getComputeAuthTokenResponse_fleetArn' - The Amazon Resource Name
-- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
-- that is assigned to a GameLift fleet resource and uniquely identifies
-- it. ARNs are unique across all Regions. Format is
-- @arn:aws:gamelift:\<region>::fleet\/fleet-a1234567-b8c9-0d1e-2fa3-b45c6d7e8912@.
--
-- 'fleetId', 'getComputeAuthTokenResponse_fleetId' - A unique identifier for the fleet that the compute is registered to.
--
-- 'httpStatus', 'getComputeAuthTokenResponse_httpStatus' - The response's http status code.
newGetComputeAuthTokenResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetComputeAuthTokenResponse
newGetComputeAuthTokenResponse pHttpStatus_ =
  GetComputeAuthTokenResponse'
    { authToken =
        Prelude.Nothing,
      computeArn = Prelude.Nothing,
      computeName = Prelude.Nothing,
      expirationTimestamp = Prelude.Nothing,
      fleetArn = Prelude.Nothing,
      fleetId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The authorization token that your game server uses to authenticate with
-- GameLift.
getComputeAuthTokenResponse_authToken :: Lens.Lens' GetComputeAuthTokenResponse (Prelude.Maybe Prelude.Text)
getComputeAuthTokenResponse_authToken = Lens.lens (\GetComputeAuthTokenResponse' {authToken} -> authToken) (\s@GetComputeAuthTokenResponse' {} a -> s {authToken = a} :: GetComputeAuthTokenResponse)

-- | The Amazon Resource Name
-- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
-- that is assigned to a GameLift compute resource and uniquely identifies
-- it. ARNs are unique across all Regions. Format is
-- @arn:aws:gamelift:\<region>::compute\/compute-a1234567-b8c9-0d1e-2fa3-b45c6d7e8912@
getComputeAuthTokenResponse_computeArn :: Lens.Lens' GetComputeAuthTokenResponse (Prelude.Maybe Prelude.Text)
getComputeAuthTokenResponse_computeArn = Lens.lens (\GetComputeAuthTokenResponse' {computeArn} -> computeArn) (\s@GetComputeAuthTokenResponse' {} a -> s {computeArn = a} :: GetComputeAuthTokenResponse)

-- | The name of the compute resource you are requesting the authorization
-- token for.
getComputeAuthTokenResponse_computeName :: Lens.Lens' GetComputeAuthTokenResponse (Prelude.Maybe Prelude.Text)
getComputeAuthTokenResponse_computeName = Lens.lens (\GetComputeAuthTokenResponse' {computeName} -> computeName) (\s@GetComputeAuthTokenResponse' {} a -> s {computeName = a} :: GetComputeAuthTokenResponse)

-- | The amount of time until the authorization token is no longer valid. To
-- continue using the compute resource for game server hosting, renew the
-- authorization token by using this operation again.
getComputeAuthTokenResponse_expirationTimestamp :: Lens.Lens' GetComputeAuthTokenResponse (Prelude.Maybe Prelude.UTCTime)
getComputeAuthTokenResponse_expirationTimestamp = Lens.lens (\GetComputeAuthTokenResponse' {expirationTimestamp} -> expirationTimestamp) (\s@GetComputeAuthTokenResponse' {} a -> s {expirationTimestamp = a} :: GetComputeAuthTokenResponse) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Name
-- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
-- that is assigned to a GameLift fleet resource and uniquely identifies
-- it. ARNs are unique across all Regions. Format is
-- @arn:aws:gamelift:\<region>::fleet\/fleet-a1234567-b8c9-0d1e-2fa3-b45c6d7e8912@.
getComputeAuthTokenResponse_fleetArn :: Lens.Lens' GetComputeAuthTokenResponse (Prelude.Maybe Prelude.Text)
getComputeAuthTokenResponse_fleetArn = Lens.lens (\GetComputeAuthTokenResponse' {fleetArn} -> fleetArn) (\s@GetComputeAuthTokenResponse' {} a -> s {fleetArn = a} :: GetComputeAuthTokenResponse)

-- | A unique identifier for the fleet that the compute is registered to.
getComputeAuthTokenResponse_fleetId :: Lens.Lens' GetComputeAuthTokenResponse (Prelude.Maybe Prelude.Text)
getComputeAuthTokenResponse_fleetId = Lens.lens (\GetComputeAuthTokenResponse' {fleetId} -> fleetId) (\s@GetComputeAuthTokenResponse' {} a -> s {fleetId = a} :: GetComputeAuthTokenResponse)

-- | The response's http status code.
getComputeAuthTokenResponse_httpStatus :: Lens.Lens' GetComputeAuthTokenResponse Prelude.Int
getComputeAuthTokenResponse_httpStatus = Lens.lens (\GetComputeAuthTokenResponse' {httpStatus} -> httpStatus) (\s@GetComputeAuthTokenResponse' {} a -> s {httpStatus = a} :: GetComputeAuthTokenResponse)

instance Prelude.NFData GetComputeAuthTokenResponse where
  rnf GetComputeAuthTokenResponse' {..} =
    Prelude.rnf authToken `Prelude.seq`
      Prelude.rnf computeArn `Prelude.seq`
        Prelude.rnf computeName `Prelude.seq`
          Prelude.rnf expirationTimestamp `Prelude.seq`
            Prelude.rnf fleetArn `Prelude.seq`
              Prelude.rnf fleetId `Prelude.seq`
                Prelude.rnf httpStatus
