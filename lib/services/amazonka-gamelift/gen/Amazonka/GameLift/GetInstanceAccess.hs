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
-- Module      : Amazonka.GameLift.GetInstanceAccess
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
-- instance and the fleet it belongs to. You can retrieve a fleet\'s
-- instance IDs by calling
-- <https://docs.aws.amazon.com/gamelift/latest/apireference/API_DescribeInstances.html DescribeInstances>.
--
-- __Learn more__
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/fleets-remote-access.html Remotely Access Fleet Instances>
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/fleets-creating-debug.html Debug Fleet Issues>
--
-- __Related actions__
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/reference-awssdk.html#reference-awssdk-resources-fleets All APIs by task>
module Amazonka.GameLift.GetInstanceAccess
  ( -- * Creating a Request
    GetInstanceAccess (..),
    newGetInstanceAccess,

    -- * Request Lenses
    getInstanceAccess_fleetId,
    getInstanceAccess_instanceId,

    -- * Destructuring the Response
    GetInstanceAccessResponse (..),
    newGetInstanceAccessResponse,

    -- * Response Lenses
    getInstanceAccessResponse_instanceAccess,
    getInstanceAccessResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GameLift.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetInstanceAccess' smart constructor.
data GetInstanceAccess = GetInstanceAccess'
  { -- | A unique identifier for the fleet that contains the instance you want
    -- access to. You can use either the fleet ID or ARN value. The fleet can
    -- be in any of the following statuses: @ACTIVATING@, @ACTIVE@, or @ERROR@.
    -- Fleets with an @ERROR@ status may be accessible for a short time before
    -- they are deleted.
    fleetId :: Prelude.Text,
    -- | A unique identifier for the instance you want to get access to. You can
    -- access an instance in any status.
    instanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetInstanceAccess' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fleetId', 'getInstanceAccess_fleetId' - A unique identifier for the fleet that contains the instance you want
-- access to. You can use either the fleet ID or ARN value. The fleet can
-- be in any of the following statuses: @ACTIVATING@, @ACTIVE@, or @ERROR@.
-- Fleets with an @ERROR@ status may be accessible for a short time before
-- they are deleted.
--
-- 'instanceId', 'getInstanceAccess_instanceId' - A unique identifier for the instance you want to get access to. You can
-- access an instance in any status.
newGetInstanceAccess ::
  -- | 'fleetId'
  Prelude.Text ->
  -- | 'instanceId'
  Prelude.Text ->
  GetInstanceAccess
newGetInstanceAccess pFleetId_ pInstanceId_ =
  GetInstanceAccess'
    { fleetId = pFleetId_,
      instanceId = pInstanceId_
    }

-- | A unique identifier for the fleet that contains the instance you want
-- access to. You can use either the fleet ID or ARN value. The fleet can
-- be in any of the following statuses: @ACTIVATING@, @ACTIVE@, or @ERROR@.
-- Fleets with an @ERROR@ status may be accessible for a short time before
-- they are deleted.
getInstanceAccess_fleetId :: Lens.Lens' GetInstanceAccess Prelude.Text
getInstanceAccess_fleetId = Lens.lens (\GetInstanceAccess' {fleetId} -> fleetId) (\s@GetInstanceAccess' {} a -> s {fleetId = a} :: GetInstanceAccess)

-- | A unique identifier for the instance you want to get access to. You can
-- access an instance in any status.
getInstanceAccess_instanceId :: Lens.Lens' GetInstanceAccess Prelude.Text
getInstanceAccess_instanceId = Lens.lens (\GetInstanceAccess' {instanceId} -> instanceId) (\s@GetInstanceAccess' {} a -> s {instanceId = a} :: GetInstanceAccess)

instance Core.AWSRequest GetInstanceAccess where
  type
    AWSResponse GetInstanceAccess =
      GetInstanceAccessResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetInstanceAccessResponse'
            Prelude.<$> (x Data..?> "InstanceAccess")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetInstanceAccess where
  hashWithSalt _salt GetInstanceAccess' {..} =
    _salt
      `Prelude.hashWithSalt` fleetId
      `Prelude.hashWithSalt` instanceId

instance Prelude.NFData GetInstanceAccess where
  rnf GetInstanceAccess' {..} =
    Prelude.rnf fleetId
      `Prelude.seq` Prelude.rnf instanceId

instance Data.ToHeaders GetInstanceAccess where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("GameLift.GetInstanceAccess" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetInstanceAccess where
  toJSON GetInstanceAccess' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("FleetId" Data..= fleetId),
            Prelude.Just ("InstanceId" Data..= instanceId)
          ]
      )

instance Data.ToPath GetInstanceAccess where
  toPath = Prelude.const "/"

instance Data.ToQuery GetInstanceAccess where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetInstanceAccessResponse' smart constructor.
data GetInstanceAccessResponse = GetInstanceAccessResponse'
  { -- | The connection information for a fleet instance, including IP address
    -- and access credentials.
    instanceAccess :: Prelude.Maybe InstanceAccess,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetInstanceAccessResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceAccess', 'getInstanceAccessResponse_instanceAccess' - The connection information for a fleet instance, including IP address
-- and access credentials.
--
-- 'httpStatus', 'getInstanceAccessResponse_httpStatus' - The response's http status code.
newGetInstanceAccessResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetInstanceAccessResponse
newGetInstanceAccessResponse pHttpStatus_ =
  GetInstanceAccessResponse'
    { instanceAccess =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The connection information for a fleet instance, including IP address
-- and access credentials.
getInstanceAccessResponse_instanceAccess :: Lens.Lens' GetInstanceAccessResponse (Prelude.Maybe InstanceAccess)
getInstanceAccessResponse_instanceAccess = Lens.lens (\GetInstanceAccessResponse' {instanceAccess} -> instanceAccess) (\s@GetInstanceAccessResponse' {} a -> s {instanceAccess = a} :: GetInstanceAccessResponse)

-- | The response's http status code.
getInstanceAccessResponse_httpStatus :: Lens.Lens' GetInstanceAccessResponse Prelude.Int
getInstanceAccessResponse_httpStatus = Lens.lens (\GetInstanceAccessResponse' {httpStatus} -> httpStatus) (\s@GetInstanceAccessResponse' {} a -> s {httpStatus = a} :: GetInstanceAccessResponse)

instance Prelude.NFData GetInstanceAccessResponse where
  rnf GetInstanceAccessResponse' {..} =
    Prelude.rnf instanceAccess
      `Prelude.seq` Prelude.rnf httpStatus
