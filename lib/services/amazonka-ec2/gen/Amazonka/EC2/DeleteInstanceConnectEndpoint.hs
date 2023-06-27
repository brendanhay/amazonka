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
-- Module      : Amazonka.EC2.DeleteInstanceConnectEndpoint
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified EC2 Instance Connect Endpoint.
module Amazonka.EC2.DeleteInstanceConnectEndpoint
  ( -- * Creating a Request
    DeleteInstanceConnectEndpoint (..),
    newDeleteInstanceConnectEndpoint,

    -- * Request Lenses
    deleteInstanceConnectEndpoint_dryRun,
    deleteInstanceConnectEndpoint_instanceConnectEndpointId,

    -- * Destructuring the Response
    DeleteInstanceConnectEndpointResponse (..),
    newDeleteInstanceConnectEndpointResponse,

    -- * Response Lenses
    deleteInstanceConnectEndpointResponse_instanceConnectEndpoint,
    deleteInstanceConnectEndpointResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteInstanceConnectEndpoint' smart constructor.
data DeleteInstanceConnectEndpoint = DeleteInstanceConnectEndpoint'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the EC2 Instance Connect Endpoint to delete.
    instanceConnectEndpointId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteInstanceConnectEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'deleteInstanceConnectEndpoint_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'instanceConnectEndpointId', 'deleteInstanceConnectEndpoint_instanceConnectEndpointId' - The ID of the EC2 Instance Connect Endpoint to delete.
newDeleteInstanceConnectEndpoint ::
  -- | 'instanceConnectEndpointId'
  Prelude.Text ->
  DeleteInstanceConnectEndpoint
newDeleteInstanceConnectEndpoint
  pInstanceConnectEndpointId_ =
    DeleteInstanceConnectEndpoint'
      { dryRun =
          Prelude.Nothing,
        instanceConnectEndpointId =
          pInstanceConnectEndpointId_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteInstanceConnectEndpoint_dryRun :: Lens.Lens' DeleteInstanceConnectEndpoint (Prelude.Maybe Prelude.Bool)
deleteInstanceConnectEndpoint_dryRun = Lens.lens (\DeleteInstanceConnectEndpoint' {dryRun} -> dryRun) (\s@DeleteInstanceConnectEndpoint' {} a -> s {dryRun = a} :: DeleteInstanceConnectEndpoint)

-- | The ID of the EC2 Instance Connect Endpoint to delete.
deleteInstanceConnectEndpoint_instanceConnectEndpointId :: Lens.Lens' DeleteInstanceConnectEndpoint Prelude.Text
deleteInstanceConnectEndpoint_instanceConnectEndpointId = Lens.lens (\DeleteInstanceConnectEndpoint' {instanceConnectEndpointId} -> instanceConnectEndpointId) (\s@DeleteInstanceConnectEndpoint' {} a -> s {instanceConnectEndpointId = a} :: DeleteInstanceConnectEndpoint)

instance
  Core.AWSRequest
    DeleteInstanceConnectEndpoint
  where
  type
    AWSResponse DeleteInstanceConnectEndpoint =
      DeleteInstanceConnectEndpointResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteInstanceConnectEndpointResponse'
            Prelude.<$> (x Data..@? "instanceConnectEndpoint")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteInstanceConnectEndpoint
  where
  hashWithSalt _salt DeleteInstanceConnectEndpoint' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` instanceConnectEndpointId

instance Prelude.NFData DeleteInstanceConnectEndpoint where
  rnf DeleteInstanceConnectEndpoint' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf instanceConnectEndpointId

instance Data.ToHeaders DeleteInstanceConnectEndpoint where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteInstanceConnectEndpoint where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteInstanceConnectEndpoint where
  toQuery DeleteInstanceConnectEndpoint' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DeleteInstanceConnectEndpoint" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "InstanceConnectEndpointId"
          Data.=: instanceConnectEndpointId
      ]

-- | /See:/ 'newDeleteInstanceConnectEndpointResponse' smart constructor.
data DeleteInstanceConnectEndpointResponse = DeleteInstanceConnectEndpointResponse'
  { -- | Information about the EC2 Instance Connect Endpoint.
    instanceConnectEndpoint :: Prelude.Maybe Ec2InstanceConnectEndpoint,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteInstanceConnectEndpointResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceConnectEndpoint', 'deleteInstanceConnectEndpointResponse_instanceConnectEndpoint' - Information about the EC2 Instance Connect Endpoint.
--
-- 'httpStatus', 'deleteInstanceConnectEndpointResponse_httpStatus' - The response's http status code.
newDeleteInstanceConnectEndpointResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteInstanceConnectEndpointResponse
newDeleteInstanceConnectEndpointResponse pHttpStatus_ =
  DeleteInstanceConnectEndpointResponse'
    { instanceConnectEndpoint =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the EC2 Instance Connect Endpoint.
deleteInstanceConnectEndpointResponse_instanceConnectEndpoint :: Lens.Lens' DeleteInstanceConnectEndpointResponse (Prelude.Maybe Ec2InstanceConnectEndpoint)
deleteInstanceConnectEndpointResponse_instanceConnectEndpoint = Lens.lens (\DeleteInstanceConnectEndpointResponse' {instanceConnectEndpoint} -> instanceConnectEndpoint) (\s@DeleteInstanceConnectEndpointResponse' {} a -> s {instanceConnectEndpoint = a} :: DeleteInstanceConnectEndpointResponse)

-- | The response's http status code.
deleteInstanceConnectEndpointResponse_httpStatus :: Lens.Lens' DeleteInstanceConnectEndpointResponse Prelude.Int
deleteInstanceConnectEndpointResponse_httpStatus = Lens.lens (\DeleteInstanceConnectEndpointResponse' {httpStatus} -> httpStatus) (\s@DeleteInstanceConnectEndpointResponse' {} a -> s {httpStatus = a} :: DeleteInstanceConnectEndpointResponse)

instance
  Prelude.NFData
    DeleteInstanceConnectEndpointResponse
  where
  rnf DeleteInstanceConnectEndpointResponse' {..} =
    Prelude.rnf instanceConnectEndpoint
      `Prelude.seq` Prelude.rnf httpStatus
