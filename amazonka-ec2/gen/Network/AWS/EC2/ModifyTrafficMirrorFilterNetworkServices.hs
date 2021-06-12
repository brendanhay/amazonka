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
-- Module      : Network.AWS.EC2.ModifyTrafficMirrorFilterNetworkServices
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
--
-- For information about filter rule properties, see
-- <https://docs.aws.amazon.com/vpc/latest/mirroring/traffic-mirroring-considerations.html Network Services>
-- in the /Traffic Mirroring User Guide/ .
module Network.AWS.EC2.ModifyTrafficMirrorFilterNetworkServices
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

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newModifyTrafficMirrorFilterNetworkServices' smart constructor.
data ModifyTrafficMirrorFilterNetworkServices = ModifyTrafficMirrorFilterNetworkServices'
  { -- | The network service, for example Amazon DNS, that you want to mirror.
    addNetworkServices :: Core.Maybe [TrafficMirrorNetworkService],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The network service, for example Amazon DNS, that you no longer want to
    -- mirror.
    removeNetworkServices :: Core.Maybe [TrafficMirrorNetworkService],
    -- | The ID of the Traffic Mirror filter.
    trafficMirrorFilterId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  ModifyTrafficMirrorFilterNetworkServices
newModifyTrafficMirrorFilterNetworkServices
  pTrafficMirrorFilterId_ =
    ModifyTrafficMirrorFilterNetworkServices'
      { addNetworkServices =
          Core.Nothing,
        dryRun = Core.Nothing,
        removeNetworkServices =
          Core.Nothing,
        trafficMirrorFilterId =
          pTrafficMirrorFilterId_
      }

-- | The network service, for example Amazon DNS, that you want to mirror.
modifyTrafficMirrorFilterNetworkServices_addNetworkServices :: Lens.Lens' ModifyTrafficMirrorFilterNetworkServices (Core.Maybe [TrafficMirrorNetworkService])
modifyTrafficMirrorFilterNetworkServices_addNetworkServices = Lens.lens (\ModifyTrafficMirrorFilterNetworkServices' {addNetworkServices} -> addNetworkServices) (\s@ModifyTrafficMirrorFilterNetworkServices' {} a -> s {addNetworkServices = a} :: ModifyTrafficMirrorFilterNetworkServices) Core.. Lens.mapping Lens._Coerce

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
modifyTrafficMirrorFilterNetworkServices_dryRun :: Lens.Lens' ModifyTrafficMirrorFilterNetworkServices (Core.Maybe Core.Bool)
modifyTrafficMirrorFilterNetworkServices_dryRun = Lens.lens (\ModifyTrafficMirrorFilterNetworkServices' {dryRun} -> dryRun) (\s@ModifyTrafficMirrorFilterNetworkServices' {} a -> s {dryRun = a} :: ModifyTrafficMirrorFilterNetworkServices)

-- | The network service, for example Amazon DNS, that you no longer want to
-- mirror.
modifyTrafficMirrorFilterNetworkServices_removeNetworkServices :: Lens.Lens' ModifyTrafficMirrorFilterNetworkServices (Core.Maybe [TrafficMirrorNetworkService])
modifyTrafficMirrorFilterNetworkServices_removeNetworkServices = Lens.lens (\ModifyTrafficMirrorFilterNetworkServices' {removeNetworkServices} -> removeNetworkServices) (\s@ModifyTrafficMirrorFilterNetworkServices' {} a -> s {removeNetworkServices = a} :: ModifyTrafficMirrorFilterNetworkServices) Core.. Lens.mapping Lens._Coerce

-- | The ID of the Traffic Mirror filter.
modifyTrafficMirrorFilterNetworkServices_trafficMirrorFilterId :: Lens.Lens' ModifyTrafficMirrorFilterNetworkServices Core.Text
modifyTrafficMirrorFilterNetworkServices_trafficMirrorFilterId = Lens.lens (\ModifyTrafficMirrorFilterNetworkServices' {trafficMirrorFilterId} -> trafficMirrorFilterId) (\s@ModifyTrafficMirrorFilterNetworkServices' {} a -> s {trafficMirrorFilterId = a} :: ModifyTrafficMirrorFilterNetworkServices)

instance
  Core.AWSRequest
    ModifyTrafficMirrorFilterNetworkServices
  where
  type
    AWSResponse
      ModifyTrafficMirrorFilterNetworkServices =
      ModifyTrafficMirrorFilterNetworkServicesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          ModifyTrafficMirrorFilterNetworkServicesResponse'
            Core.<$> (x Core..@? "trafficMirrorFilter")
              Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    ModifyTrafficMirrorFilterNetworkServices

instance
  Core.NFData
    ModifyTrafficMirrorFilterNetworkServices

instance
  Core.ToHeaders
    ModifyTrafficMirrorFilterNetworkServices
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToPath
    ModifyTrafficMirrorFilterNetworkServices
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    ModifyTrafficMirrorFilterNetworkServices
  where
  toQuery ModifyTrafficMirrorFilterNetworkServices' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ( "ModifyTrafficMirrorFilterNetworkServices" ::
                      Core.ByteString
                  ),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        Core.toQuery
          ( Core.toQueryList "AddNetworkService"
              Core.<$> addNetworkServices
          ),
        "DryRun" Core.=: dryRun,
        Core.toQuery
          ( Core.toQueryList "RemoveNetworkService"
              Core.<$> removeNetworkServices
          ),
        "TrafficMirrorFilterId"
          Core.=: trafficMirrorFilterId
      ]

-- | /See:/ 'newModifyTrafficMirrorFilterNetworkServicesResponse' smart constructor.
data ModifyTrafficMirrorFilterNetworkServicesResponse = ModifyTrafficMirrorFilterNetworkServicesResponse'
  { -- | The Traffic Mirror filter that the network service is associated with.
    trafficMirrorFilter :: Core.Maybe TrafficMirrorFilter,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  ModifyTrafficMirrorFilterNetworkServicesResponse
newModifyTrafficMirrorFilterNetworkServicesResponse
  pHttpStatus_ =
    ModifyTrafficMirrorFilterNetworkServicesResponse'
      { trafficMirrorFilter =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The Traffic Mirror filter that the network service is associated with.
modifyTrafficMirrorFilterNetworkServicesResponse_trafficMirrorFilter :: Lens.Lens' ModifyTrafficMirrorFilterNetworkServicesResponse (Core.Maybe TrafficMirrorFilter)
modifyTrafficMirrorFilterNetworkServicesResponse_trafficMirrorFilter = Lens.lens (\ModifyTrafficMirrorFilterNetworkServicesResponse' {trafficMirrorFilter} -> trafficMirrorFilter) (\s@ModifyTrafficMirrorFilterNetworkServicesResponse' {} a -> s {trafficMirrorFilter = a} :: ModifyTrafficMirrorFilterNetworkServicesResponse)

-- | The response's http status code.
modifyTrafficMirrorFilterNetworkServicesResponse_httpStatus :: Lens.Lens' ModifyTrafficMirrorFilterNetworkServicesResponse Core.Int
modifyTrafficMirrorFilterNetworkServicesResponse_httpStatus = Lens.lens (\ModifyTrafficMirrorFilterNetworkServicesResponse' {httpStatus} -> httpStatus) (\s@ModifyTrafficMirrorFilterNetworkServicesResponse' {} a -> s {httpStatus = a} :: ModifyTrafficMirrorFilterNetworkServicesResponse)

instance
  Core.NFData
    ModifyTrafficMirrorFilterNetworkServicesResponse
