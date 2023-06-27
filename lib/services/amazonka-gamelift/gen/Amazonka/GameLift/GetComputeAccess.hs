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
-- Module      : Amazonka.GameLift.GetComputeAccess
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Requests remote access to a fleet instance. Remote access is useful for
-- debugging, gathering benchmarking data, or observing activity in real
-- time.
--
-- To remotely access an instance, you need credentials that match the
-- operating system of the instance. For a Windows instance, Amazon
-- GameLift returns a user name and password as strings for use with a
-- Windows Remote Desktop client. For a Linux instance, Amazon GameLift
-- returns a user name and RSA private key, also as strings, for use with
-- an SSH client. The private key must be saved in the proper format to a
-- @.pem@ file before using. If you\'re making this request using the CLI,
-- saving the secret can be handled as part of the @GetInstanceAccess@
-- request, as shown in one of the examples for this operation.
--
-- To request access to a specific instance, specify the IDs of both the
-- instance and the fleet it belongs to.
--
-- __Learn more__
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/fleets-remote-access.html Remotely Access Fleet Instances>
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/fleets-creating-debug.html Debug Fleet Issues>
module Amazonka.GameLift.GetComputeAccess
  ( -- * Creating a Request
    GetComputeAccess (..),
    newGetComputeAccess,

    -- * Request Lenses
    getComputeAccess_fleetId,
    getComputeAccess_computeName,

    -- * Destructuring the Response
    GetComputeAccessResponse (..),
    newGetComputeAccessResponse,

    -- * Response Lenses
    getComputeAccessResponse_computeArn,
    getComputeAccessResponse_computeName,
    getComputeAccessResponse_credentials,
    getComputeAccessResponse_fleetArn,
    getComputeAccessResponse_fleetId,
    getComputeAccessResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GameLift.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetComputeAccess' smart constructor.
data GetComputeAccess = GetComputeAccess'
  { -- | A unique identifier for the fleet that the compute resource is
    -- registered to.
    fleetId :: Prelude.Text,
    -- | The name of the compute resource you are requesting credentials for.
    computeName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetComputeAccess' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fleetId', 'getComputeAccess_fleetId' - A unique identifier for the fleet that the compute resource is
-- registered to.
--
-- 'computeName', 'getComputeAccess_computeName' - The name of the compute resource you are requesting credentials for.
newGetComputeAccess ::
  -- | 'fleetId'
  Prelude.Text ->
  -- | 'computeName'
  Prelude.Text ->
  GetComputeAccess
newGetComputeAccess pFleetId_ pComputeName_ =
  GetComputeAccess'
    { fleetId = pFleetId_,
      computeName = pComputeName_
    }

-- | A unique identifier for the fleet that the compute resource is
-- registered to.
getComputeAccess_fleetId :: Lens.Lens' GetComputeAccess Prelude.Text
getComputeAccess_fleetId = Lens.lens (\GetComputeAccess' {fleetId} -> fleetId) (\s@GetComputeAccess' {} a -> s {fleetId = a} :: GetComputeAccess)

-- | The name of the compute resource you are requesting credentials for.
getComputeAccess_computeName :: Lens.Lens' GetComputeAccess Prelude.Text
getComputeAccess_computeName = Lens.lens (\GetComputeAccess' {computeName} -> computeName) (\s@GetComputeAccess' {} a -> s {computeName = a} :: GetComputeAccess)

instance Core.AWSRequest GetComputeAccess where
  type
    AWSResponse GetComputeAccess =
      GetComputeAccessResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetComputeAccessResponse'
            Prelude.<$> (x Data..?> "ComputeArn")
            Prelude.<*> (x Data..?> "ComputeName")
            Prelude.<*> (x Data..?> "Credentials")
            Prelude.<*> (x Data..?> "FleetArn")
            Prelude.<*> (x Data..?> "FleetId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetComputeAccess where
  hashWithSalt _salt GetComputeAccess' {..} =
    _salt
      `Prelude.hashWithSalt` fleetId
      `Prelude.hashWithSalt` computeName

instance Prelude.NFData GetComputeAccess where
  rnf GetComputeAccess' {..} =
    Prelude.rnf fleetId
      `Prelude.seq` Prelude.rnf computeName

instance Data.ToHeaders GetComputeAccess where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("GameLift.GetComputeAccess" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetComputeAccess where
  toJSON GetComputeAccess' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("FleetId" Data..= fleetId),
            Prelude.Just ("ComputeName" Data..= computeName)
          ]
      )

instance Data.ToPath GetComputeAccess where
  toPath = Prelude.const "/"

instance Data.ToQuery GetComputeAccess where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetComputeAccessResponse' smart constructor.
data GetComputeAccessResponse = GetComputeAccessResponse'
  { -- | The Amazon Resource Name
    -- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
    -- that is assigned to a Amazon GameLift compute resource and uniquely
    -- identifies it. ARNs are unique across all Regions. Format is
    -- @arn:aws:gamelift:\<region>::compute\/compute-a1234567-b8c9-0d1e-2fa3-b45c6d7e8912@.
    computeArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the compute resource you requested credentials for.
    computeName :: Prelude.Maybe Prelude.Text,
    -- | The access credentials for the compute resource.
    credentials :: Prelude.Maybe (Data.Sensitive AwsCredentials),
    -- | The Amazon Resource Name
    -- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
    -- that is assigned to a Amazon GameLift fleet resource and uniquely
    -- identifies it. ARNs are unique across all Regions. Format is
    -- @arn:aws:gamelift:\<region>::fleet\/fleet-a1234567-b8c9-0d1e-2fa3-b45c6d7e8912@.
    fleetArn :: Prelude.Maybe Prelude.Text,
    -- | The fleet ID of compute resource.
    fleetId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetComputeAccessResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'computeArn', 'getComputeAccessResponse_computeArn' - The Amazon Resource Name
-- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
-- that is assigned to a Amazon GameLift compute resource and uniquely
-- identifies it. ARNs are unique across all Regions. Format is
-- @arn:aws:gamelift:\<region>::compute\/compute-a1234567-b8c9-0d1e-2fa3-b45c6d7e8912@.
--
-- 'computeName', 'getComputeAccessResponse_computeName' - The name of the compute resource you requested credentials for.
--
-- 'credentials', 'getComputeAccessResponse_credentials' - The access credentials for the compute resource.
--
-- 'fleetArn', 'getComputeAccessResponse_fleetArn' - The Amazon Resource Name
-- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
-- that is assigned to a Amazon GameLift fleet resource and uniquely
-- identifies it. ARNs are unique across all Regions. Format is
-- @arn:aws:gamelift:\<region>::fleet\/fleet-a1234567-b8c9-0d1e-2fa3-b45c6d7e8912@.
--
-- 'fleetId', 'getComputeAccessResponse_fleetId' - The fleet ID of compute resource.
--
-- 'httpStatus', 'getComputeAccessResponse_httpStatus' - The response's http status code.
newGetComputeAccessResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetComputeAccessResponse
newGetComputeAccessResponse pHttpStatus_ =
  GetComputeAccessResponse'
    { computeArn =
        Prelude.Nothing,
      computeName = Prelude.Nothing,
      credentials = Prelude.Nothing,
      fleetArn = Prelude.Nothing,
      fleetId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name
-- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
-- that is assigned to a Amazon GameLift compute resource and uniquely
-- identifies it. ARNs are unique across all Regions. Format is
-- @arn:aws:gamelift:\<region>::compute\/compute-a1234567-b8c9-0d1e-2fa3-b45c6d7e8912@.
getComputeAccessResponse_computeArn :: Lens.Lens' GetComputeAccessResponse (Prelude.Maybe Prelude.Text)
getComputeAccessResponse_computeArn = Lens.lens (\GetComputeAccessResponse' {computeArn} -> computeArn) (\s@GetComputeAccessResponse' {} a -> s {computeArn = a} :: GetComputeAccessResponse)

-- | The name of the compute resource you requested credentials for.
getComputeAccessResponse_computeName :: Lens.Lens' GetComputeAccessResponse (Prelude.Maybe Prelude.Text)
getComputeAccessResponse_computeName = Lens.lens (\GetComputeAccessResponse' {computeName} -> computeName) (\s@GetComputeAccessResponse' {} a -> s {computeName = a} :: GetComputeAccessResponse)

-- | The access credentials for the compute resource.
getComputeAccessResponse_credentials :: Lens.Lens' GetComputeAccessResponse (Prelude.Maybe AwsCredentials)
getComputeAccessResponse_credentials = Lens.lens (\GetComputeAccessResponse' {credentials} -> credentials) (\s@GetComputeAccessResponse' {} a -> s {credentials = a} :: GetComputeAccessResponse) Prelude.. Lens.mapping Data._Sensitive

-- | The Amazon Resource Name
-- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
-- that is assigned to a Amazon GameLift fleet resource and uniquely
-- identifies it. ARNs are unique across all Regions. Format is
-- @arn:aws:gamelift:\<region>::fleet\/fleet-a1234567-b8c9-0d1e-2fa3-b45c6d7e8912@.
getComputeAccessResponse_fleetArn :: Lens.Lens' GetComputeAccessResponse (Prelude.Maybe Prelude.Text)
getComputeAccessResponse_fleetArn = Lens.lens (\GetComputeAccessResponse' {fleetArn} -> fleetArn) (\s@GetComputeAccessResponse' {} a -> s {fleetArn = a} :: GetComputeAccessResponse)

-- | The fleet ID of compute resource.
getComputeAccessResponse_fleetId :: Lens.Lens' GetComputeAccessResponse (Prelude.Maybe Prelude.Text)
getComputeAccessResponse_fleetId = Lens.lens (\GetComputeAccessResponse' {fleetId} -> fleetId) (\s@GetComputeAccessResponse' {} a -> s {fleetId = a} :: GetComputeAccessResponse)

-- | The response's http status code.
getComputeAccessResponse_httpStatus :: Lens.Lens' GetComputeAccessResponse Prelude.Int
getComputeAccessResponse_httpStatus = Lens.lens (\GetComputeAccessResponse' {httpStatus} -> httpStatus) (\s@GetComputeAccessResponse' {} a -> s {httpStatus = a} :: GetComputeAccessResponse)

instance Prelude.NFData GetComputeAccessResponse where
  rnf GetComputeAccessResponse' {..} =
    Prelude.rnf computeArn
      `Prelude.seq` Prelude.rnf computeName
      `Prelude.seq` Prelude.rnf credentials
      `Prelude.seq` Prelude.rnf fleetArn
      `Prelude.seq` Prelude.rnf fleetId
      `Prelude.seq` Prelude.rnf httpStatus
