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
-- Module      : Network.AWS.EC2.DeleteNetworkInsightsPath
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified path.
module Network.AWS.EC2.DeleteNetworkInsightsPath
  ( -- * Creating a Request
    DeleteNetworkInsightsPath (..),
    newDeleteNetworkInsightsPath,

    -- * Request Lenses
    deleteNetworkInsightsPath_dryRun,
    deleteNetworkInsightsPath_networkInsightsPathId,

    -- * Destructuring the Response
    DeleteNetworkInsightsPathResponse (..),
    newDeleteNetworkInsightsPathResponse,

    -- * Response Lenses
    deleteNetworkInsightsPathResponse_networkInsightsPathId,
    deleteNetworkInsightsPathResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteNetworkInsightsPath' smart constructor.
data DeleteNetworkInsightsPath = DeleteNetworkInsightsPath'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The ID of the path.
    networkInsightsPathId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteNetworkInsightsPath' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'deleteNetworkInsightsPath_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'networkInsightsPathId', 'deleteNetworkInsightsPath_networkInsightsPathId' - The ID of the path.
newDeleteNetworkInsightsPath ::
  -- | 'networkInsightsPathId'
  Core.Text ->
  DeleteNetworkInsightsPath
newDeleteNetworkInsightsPath pNetworkInsightsPathId_ =
  DeleteNetworkInsightsPath'
    { dryRun = Core.Nothing,
      networkInsightsPathId = pNetworkInsightsPathId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteNetworkInsightsPath_dryRun :: Lens.Lens' DeleteNetworkInsightsPath (Core.Maybe Core.Bool)
deleteNetworkInsightsPath_dryRun = Lens.lens (\DeleteNetworkInsightsPath' {dryRun} -> dryRun) (\s@DeleteNetworkInsightsPath' {} a -> s {dryRun = a} :: DeleteNetworkInsightsPath)

-- | The ID of the path.
deleteNetworkInsightsPath_networkInsightsPathId :: Lens.Lens' DeleteNetworkInsightsPath Core.Text
deleteNetworkInsightsPath_networkInsightsPathId = Lens.lens (\DeleteNetworkInsightsPath' {networkInsightsPathId} -> networkInsightsPathId) (\s@DeleteNetworkInsightsPath' {} a -> s {networkInsightsPathId = a} :: DeleteNetworkInsightsPath)

instance Core.AWSRequest DeleteNetworkInsightsPath where
  type
    AWSResponse DeleteNetworkInsightsPath =
      DeleteNetworkInsightsPathResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteNetworkInsightsPathResponse'
            Core.<$> (x Core..@? "networkInsightsPathId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteNetworkInsightsPath

instance Core.NFData DeleteNetworkInsightsPath

instance Core.ToHeaders DeleteNetworkInsightsPath where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DeleteNetworkInsightsPath where
  toPath = Core.const "/"

instance Core.ToQuery DeleteNetworkInsightsPath where
  toQuery DeleteNetworkInsightsPath' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DeleteNetworkInsightsPath" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "DryRun" Core.=: dryRun,
        "NetworkInsightsPathId"
          Core.=: networkInsightsPathId
      ]

-- | /See:/ 'newDeleteNetworkInsightsPathResponse' smart constructor.
data DeleteNetworkInsightsPathResponse = DeleteNetworkInsightsPathResponse'
  { -- | The ID of the path.
    networkInsightsPathId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteNetworkInsightsPathResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'networkInsightsPathId', 'deleteNetworkInsightsPathResponse_networkInsightsPathId' - The ID of the path.
--
-- 'httpStatus', 'deleteNetworkInsightsPathResponse_httpStatus' - The response's http status code.
newDeleteNetworkInsightsPathResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteNetworkInsightsPathResponse
newDeleteNetworkInsightsPathResponse pHttpStatus_ =
  DeleteNetworkInsightsPathResponse'
    { networkInsightsPathId =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the path.
deleteNetworkInsightsPathResponse_networkInsightsPathId :: Lens.Lens' DeleteNetworkInsightsPathResponse (Core.Maybe Core.Text)
deleteNetworkInsightsPathResponse_networkInsightsPathId = Lens.lens (\DeleteNetworkInsightsPathResponse' {networkInsightsPathId} -> networkInsightsPathId) (\s@DeleteNetworkInsightsPathResponse' {} a -> s {networkInsightsPathId = a} :: DeleteNetworkInsightsPathResponse)

-- | The response's http status code.
deleteNetworkInsightsPathResponse_httpStatus :: Lens.Lens' DeleteNetworkInsightsPathResponse Core.Int
deleteNetworkInsightsPathResponse_httpStatus = Lens.lens (\DeleteNetworkInsightsPathResponse' {httpStatus} -> httpStatus) (\s@DeleteNetworkInsightsPathResponse' {} a -> s {httpStatus = a} :: DeleteNetworkInsightsPathResponse)

instance
  Core.NFData
    DeleteNetworkInsightsPathResponse
