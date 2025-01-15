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
-- Module      : Amazonka.EC2.DeleteVpcEndpointServiceConfigurations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes one or more VPC endpoint service configurations in your account.
-- Before you delete the endpoint service configuration, you must reject
-- any @Available@ or @PendingAcceptance@ interface endpoint connections
-- that are attached to the service.
module Amazonka.EC2.DeleteVpcEndpointServiceConfigurations
  ( -- * Creating a Request
    DeleteVpcEndpointServiceConfigurations (..),
    newDeleteVpcEndpointServiceConfigurations,

    -- * Request Lenses
    deleteVpcEndpointServiceConfigurations_dryRun,
    deleteVpcEndpointServiceConfigurations_serviceIds,

    -- * Destructuring the Response
    DeleteVpcEndpointServiceConfigurationsResponse (..),
    newDeleteVpcEndpointServiceConfigurationsResponse,

    -- * Response Lenses
    deleteVpcEndpointServiceConfigurationsResponse_unsuccessful,
    deleteVpcEndpointServiceConfigurationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteVpcEndpointServiceConfigurations' smart constructor.
data DeleteVpcEndpointServiceConfigurations = DeleteVpcEndpointServiceConfigurations'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The IDs of one or more services.
    serviceIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteVpcEndpointServiceConfigurations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'deleteVpcEndpointServiceConfigurations_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'serviceIds', 'deleteVpcEndpointServiceConfigurations_serviceIds' - The IDs of one or more services.
newDeleteVpcEndpointServiceConfigurations ::
  DeleteVpcEndpointServiceConfigurations
newDeleteVpcEndpointServiceConfigurations =
  DeleteVpcEndpointServiceConfigurations'
    { dryRun =
        Prelude.Nothing,
      serviceIds = Prelude.mempty
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteVpcEndpointServiceConfigurations_dryRun :: Lens.Lens' DeleteVpcEndpointServiceConfigurations (Prelude.Maybe Prelude.Bool)
deleteVpcEndpointServiceConfigurations_dryRun = Lens.lens (\DeleteVpcEndpointServiceConfigurations' {dryRun} -> dryRun) (\s@DeleteVpcEndpointServiceConfigurations' {} a -> s {dryRun = a} :: DeleteVpcEndpointServiceConfigurations)

-- | The IDs of one or more services.
deleteVpcEndpointServiceConfigurations_serviceIds :: Lens.Lens' DeleteVpcEndpointServiceConfigurations [Prelude.Text]
deleteVpcEndpointServiceConfigurations_serviceIds = Lens.lens (\DeleteVpcEndpointServiceConfigurations' {serviceIds} -> serviceIds) (\s@DeleteVpcEndpointServiceConfigurations' {} a -> s {serviceIds = a} :: DeleteVpcEndpointServiceConfigurations) Prelude.. Lens.coerced

instance
  Core.AWSRequest
    DeleteVpcEndpointServiceConfigurations
  where
  type
    AWSResponse
      DeleteVpcEndpointServiceConfigurations =
      DeleteVpcEndpointServiceConfigurationsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteVpcEndpointServiceConfigurationsResponse'
            Prelude.<$> ( x Data..@? "unsuccessful" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteVpcEndpointServiceConfigurations
  where
  hashWithSalt
    _salt
    DeleteVpcEndpointServiceConfigurations' {..} =
      _salt
        `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` serviceIds

instance
  Prelude.NFData
    DeleteVpcEndpointServiceConfigurations
  where
  rnf DeleteVpcEndpointServiceConfigurations' {..} =
    Prelude.rnf dryRun `Prelude.seq`
      Prelude.rnf serviceIds

instance
  Data.ToHeaders
    DeleteVpcEndpointServiceConfigurations
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    DeleteVpcEndpointServiceConfigurations
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DeleteVpcEndpointServiceConfigurations
  where
  toQuery DeleteVpcEndpointServiceConfigurations' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DeleteVpcEndpointServiceConfigurations" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        Data.toQueryList "ServiceId" serviceIds
      ]

-- | /See:/ 'newDeleteVpcEndpointServiceConfigurationsResponse' smart constructor.
data DeleteVpcEndpointServiceConfigurationsResponse = DeleteVpcEndpointServiceConfigurationsResponse'
  { -- | Information about the service configurations that were not deleted, if
    -- applicable.
    unsuccessful :: Prelude.Maybe [UnsuccessfulItem],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteVpcEndpointServiceConfigurationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'unsuccessful', 'deleteVpcEndpointServiceConfigurationsResponse_unsuccessful' - Information about the service configurations that were not deleted, if
-- applicable.
--
-- 'httpStatus', 'deleteVpcEndpointServiceConfigurationsResponse_httpStatus' - The response's http status code.
newDeleteVpcEndpointServiceConfigurationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteVpcEndpointServiceConfigurationsResponse
newDeleteVpcEndpointServiceConfigurationsResponse
  pHttpStatus_ =
    DeleteVpcEndpointServiceConfigurationsResponse'
      { unsuccessful =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Information about the service configurations that were not deleted, if
-- applicable.
deleteVpcEndpointServiceConfigurationsResponse_unsuccessful :: Lens.Lens' DeleteVpcEndpointServiceConfigurationsResponse (Prelude.Maybe [UnsuccessfulItem])
deleteVpcEndpointServiceConfigurationsResponse_unsuccessful = Lens.lens (\DeleteVpcEndpointServiceConfigurationsResponse' {unsuccessful} -> unsuccessful) (\s@DeleteVpcEndpointServiceConfigurationsResponse' {} a -> s {unsuccessful = a} :: DeleteVpcEndpointServiceConfigurationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
deleteVpcEndpointServiceConfigurationsResponse_httpStatus :: Lens.Lens' DeleteVpcEndpointServiceConfigurationsResponse Prelude.Int
deleteVpcEndpointServiceConfigurationsResponse_httpStatus = Lens.lens (\DeleteVpcEndpointServiceConfigurationsResponse' {httpStatus} -> httpStatus) (\s@DeleteVpcEndpointServiceConfigurationsResponse' {} a -> s {httpStatus = a} :: DeleteVpcEndpointServiceConfigurationsResponse)

instance
  Prelude.NFData
    DeleteVpcEndpointServiceConfigurationsResponse
  where
  rnf
    DeleteVpcEndpointServiceConfigurationsResponse' {..} =
      Prelude.rnf unsuccessful `Prelude.seq`
        Prelude.rnf httpStatus
