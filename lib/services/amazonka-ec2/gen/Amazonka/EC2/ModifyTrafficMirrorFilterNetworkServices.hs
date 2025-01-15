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
-- Module      : Amazonka.EC2.ModifyTrafficMirrorFilterNetworkServices
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows or restricts mirroring network services.
--
-- By default, Amazon DNS network services are not eligible for Traffic
-- Mirror. Use @AddNetworkServices@ to add network services to a Traffic
-- Mirror filter. When a network service is added to the Traffic Mirror
-- filter, all traffic related to that network service will be mirrored.
-- When you no longer want to mirror network services, use
-- @RemoveNetworkServices@ to remove the network services from the Traffic
-- Mirror filter.
module Amazonka.EC2.ModifyTrafficMirrorFilterNetworkServices
  ( -- * Creating a Request
    ModifyTrafficMirrorFilterNetworkServices (..),
    newModifyTrafficMirrorFilterNetworkServices,

    -- * Request Lenses
    modifyTrafficMirrorFilterNetworkServices_addNetworkServices,
    modifyTrafficMirrorFilterNetworkServices_dryRun,
    modifyTrafficMirrorFilterNetworkServices_removeNetworkServices,
    modifyTrafficMirrorFilterNetworkServices_trafficMirrorFilterId,

    -- * Destructuring the Response
    ModifyTrafficMirrorFilterNetworkServicesResponse (..),
    newModifyTrafficMirrorFilterNetworkServicesResponse,

    -- * Response Lenses
    modifyTrafficMirrorFilterNetworkServicesResponse_trafficMirrorFilter,
    modifyTrafficMirrorFilterNetworkServicesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newModifyTrafficMirrorFilterNetworkServices' smart constructor.
data ModifyTrafficMirrorFilterNetworkServices = ModifyTrafficMirrorFilterNetworkServices'
  { -- | The network service, for example Amazon DNS, that you want to mirror.
    addNetworkServices :: Prelude.Maybe [TrafficMirrorNetworkService],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The network service, for example Amazon DNS, that you no longer want to
    -- mirror.
    removeNetworkServices :: Prelude.Maybe [TrafficMirrorNetworkService],
    -- | The ID of the Traffic Mirror filter.
    trafficMirrorFilterId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyTrafficMirrorFilterNetworkServices' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'addNetworkServices', 'modifyTrafficMirrorFilterNetworkServices_addNetworkServices' - The network service, for example Amazon DNS, that you want to mirror.
--
-- 'dryRun', 'modifyTrafficMirrorFilterNetworkServices_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'removeNetworkServices', 'modifyTrafficMirrorFilterNetworkServices_removeNetworkServices' - The network service, for example Amazon DNS, that you no longer want to
-- mirror.
--
-- 'trafficMirrorFilterId', 'modifyTrafficMirrorFilterNetworkServices_trafficMirrorFilterId' - The ID of the Traffic Mirror filter.
newModifyTrafficMirrorFilterNetworkServices ::
  -- | 'trafficMirrorFilterId'
  Prelude.Text ->
  ModifyTrafficMirrorFilterNetworkServices
newModifyTrafficMirrorFilterNetworkServices
  pTrafficMirrorFilterId_ =
    ModifyTrafficMirrorFilterNetworkServices'
      { addNetworkServices =
          Prelude.Nothing,
        dryRun = Prelude.Nothing,
        removeNetworkServices =
          Prelude.Nothing,
        trafficMirrorFilterId =
          pTrafficMirrorFilterId_
      }

-- | The network service, for example Amazon DNS, that you want to mirror.
modifyTrafficMirrorFilterNetworkServices_addNetworkServices :: Lens.Lens' ModifyTrafficMirrorFilterNetworkServices (Prelude.Maybe [TrafficMirrorNetworkService])
modifyTrafficMirrorFilterNetworkServices_addNetworkServices = Lens.lens (\ModifyTrafficMirrorFilterNetworkServices' {addNetworkServices} -> addNetworkServices) (\s@ModifyTrafficMirrorFilterNetworkServices' {} a -> s {addNetworkServices = a} :: ModifyTrafficMirrorFilterNetworkServices) Prelude.. Lens.mapping Lens.coerced

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
modifyTrafficMirrorFilterNetworkServices_dryRun :: Lens.Lens' ModifyTrafficMirrorFilterNetworkServices (Prelude.Maybe Prelude.Bool)
modifyTrafficMirrorFilterNetworkServices_dryRun = Lens.lens (\ModifyTrafficMirrorFilterNetworkServices' {dryRun} -> dryRun) (\s@ModifyTrafficMirrorFilterNetworkServices' {} a -> s {dryRun = a} :: ModifyTrafficMirrorFilterNetworkServices)

-- | The network service, for example Amazon DNS, that you no longer want to
-- mirror.
modifyTrafficMirrorFilterNetworkServices_removeNetworkServices :: Lens.Lens' ModifyTrafficMirrorFilterNetworkServices (Prelude.Maybe [TrafficMirrorNetworkService])
modifyTrafficMirrorFilterNetworkServices_removeNetworkServices = Lens.lens (\ModifyTrafficMirrorFilterNetworkServices' {removeNetworkServices} -> removeNetworkServices) (\s@ModifyTrafficMirrorFilterNetworkServices' {} a -> s {removeNetworkServices = a} :: ModifyTrafficMirrorFilterNetworkServices) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the Traffic Mirror filter.
modifyTrafficMirrorFilterNetworkServices_trafficMirrorFilterId :: Lens.Lens' ModifyTrafficMirrorFilterNetworkServices Prelude.Text
modifyTrafficMirrorFilterNetworkServices_trafficMirrorFilterId = Lens.lens (\ModifyTrafficMirrorFilterNetworkServices' {trafficMirrorFilterId} -> trafficMirrorFilterId) (\s@ModifyTrafficMirrorFilterNetworkServices' {} a -> s {trafficMirrorFilterId = a} :: ModifyTrafficMirrorFilterNetworkServices)

instance
  Core.AWSRequest
    ModifyTrafficMirrorFilterNetworkServices
  where
  type
    AWSResponse
      ModifyTrafficMirrorFilterNetworkServices =
      ModifyTrafficMirrorFilterNetworkServicesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ModifyTrafficMirrorFilterNetworkServicesResponse'
            Prelude.<$> (x Data..@? "trafficMirrorFilter")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ModifyTrafficMirrorFilterNetworkServices
  where
  hashWithSalt
    _salt
    ModifyTrafficMirrorFilterNetworkServices' {..} =
      _salt
        `Prelude.hashWithSalt` addNetworkServices
        `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` removeNetworkServices
        `Prelude.hashWithSalt` trafficMirrorFilterId

instance
  Prelude.NFData
    ModifyTrafficMirrorFilterNetworkServices
  where
  rnf ModifyTrafficMirrorFilterNetworkServices' {..} =
    Prelude.rnf addNetworkServices `Prelude.seq`
      Prelude.rnf dryRun `Prelude.seq`
        Prelude.rnf removeNetworkServices `Prelude.seq`
          Prelude.rnf trafficMirrorFilterId

instance
  Data.ToHeaders
    ModifyTrafficMirrorFilterNetworkServices
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    ModifyTrafficMirrorFilterNetworkServices
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    ModifyTrafficMirrorFilterNetworkServices
  where
  toQuery ModifyTrafficMirrorFilterNetworkServices' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "ModifyTrafficMirrorFilterNetworkServices" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        Data.toQuery
          ( Data.toQueryList "AddNetworkService"
              Prelude.<$> addNetworkServices
          ),
        "DryRun" Data.=: dryRun,
        Data.toQuery
          ( Data.toQueryList "RemoveNetworkService"
              Prelude.<$> removeNetworkServices
          ),
        "TrafficMirrorFilterId"
          Data.=: trafficMirrorFilterId
      ]

-- | /See:/ 'newModifyTrafficMirrorFilterNetworkServicesResponse' smart constructor.
data ModifyTrafficMirrorFilterNetworkServicesResponse = ModifyTrafficMirrorFilterNetworkServicesResponse'
  { -- | The Traffic Mirror filter that the network service is associated with.
    trafficMirrorFilter :: Prelude.Maybe TrafficMirrorFilter,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyTrafficMirrorFilterNetworkServicesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trafficMirrorFilter', 'modifyTrafficMirrorFilterNetworkServicesResponse_trafficMirrorFilter' - The Traffic Mirror filter that the network service is associated with.
--
-- 'httpStatus', 'modifyTrafficMirrorFilterNetworkServicesResponse_httpStatus' - The response's http status code.
newModifyTrafficMirrorFilterNetworkServicesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ModifyTrafficMirrorFilterNetworkServicesResponse
newModifyTrafficMirrorFilterNetworkServicesResponse
  pHttpStatus_ =
    ModifyTrafficMirrorFilterNetworkServicesResponse'
      { trafficMirrorFilter =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The Traffic Mirror filter that the network service is associated with.
modifyTrafficMirrorFilterNetworkServicesResponse_trafficMirrorFilter :: Lens.Lens' ModifyTrafficMirrorFilterNetworkServicesResponse (Prelude.Maybe TrafficMirrorFilter)
modifyTrafficMirrorFilterNetworkServicesResponse_trafficMirrorFilter = Lens.lens (\ModifyTrafficMirrorFilterNetworkServicesResponse' {trafficMirrorFilter} -> trafficMirrorFilter) (\s@ModifyTrafficMirrorFilterNetworkServicesResponse' {} a -> s {trafficMirrorFilter = a} :: ModifyTrafficMirrorFilterNetworkServicesResponse)

-- | The response's http status code.
modifyTrafficMirrorFilterNetworkServicesResponse_httpStatus :: Lens.Lens' ModifyTrafficMirrorFilterNetworkServicesResponse Prelude.Int
modifyTrafficMirrorFilterNetworkServicesResponse_httpStatus = Lens.lens (\ModifyTrafficMirrorFilterNetworkServicesResponse' {httpStatus} -> httpStatus) (\s@ModifyTrafficMirrorFilterNetworkServicesResponse' {} a -> s {httpStatus = a} :: ModifyTrafficMirrorFilterNetworkServicesResponse)

instance
  Prelude.NFData
    ModifyTrafficMirrorFilterNetworkServicesResponse
  where
  rnf
    ModifyTrafficMirrorFilterNetworkServicesResponse' {..} =
      Prelude.rnf trafficMirrorFilter `Prelude.seq`
        Prelude.rnf httpStatus
