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
-- Module      : Network.AWS.EC2.DeleteNetworkInsightsAnalysis
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified network insights analysis.
module Network.AWS.EC2.DeleteNetworkInsightsAnalysis
  ( -- * Creating a Request
    DeleteNetworkInsightsAnalysis (..),
    newDeleteNetworkInsightsAnalysis,

    -- * Request Lenses
    deleteNetworkInsightsAnalysis_dryRun,
    deleteNetworkInsightsAnalysis_networkInsightsAnalysisId,

    -- * Destructuring the Response
    DeleteNetworkInsightsAnalysisResponse (..),
    newDeleteNetworkInsightsAnalysisResponse,

    -- * Response Lenses
    deleteNetworkInsightsAnalysisResponse_networkInsightsAnalysisId,
    deleteNetworkInsightsAnalysisResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteNetworkInsightsAnalysis' smart constructor.
data DeleteNetworkInsightsAnalysis = DeleteNetworkInsightsAnalysis'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The ID of the network insights analysis.
    networkInsightsAnalysisId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteNetworkInsightsAnalysis' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'deleteNetworkInsightsAnalysis_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'networkInsightsAnalysisId', 'deleteNetworkInsightsAnalysis_networkInsightsAnalysisId' - The ID of the network insights analysis.
newDeleteNetworkInsightsAnalysis ::
  -- | 'networkInsightsAnalysisId'
  Core.Text ->
  DeleteNetworkInsightsAnalysis
newDeleteNetworkInsightsAnalysis
  pNetworkInsightsAnalysisId_ =
    DeleteNetworkInsightsAnalysis'
      { dryRun =
          Core.Nothing,
        networkInsightsAnalysisId =
          pNetworkInsightsAnalysisId_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteNetworkInsightsAnalysis_dryRun :: Lens.Lens' DeleteNetworkInsightsAnalysis (Core.Maybe Core.Bool)
deleteNetworkInsightsAnalysis_dryRun = Lens.lens (\DeleteNetworkInsightsAnalysis' {dryRun} -> dryRun) (\s@DeleteNetworkInsightsAnalysis' {} a -> s {dryRun = a} :: DeleteNetworkInsightsAnalysis)

-- | The ID of the network insights analysis.
deleteNetworkInsightsAnalysis_networkInsightsAnalysisId :: Lens.Lens' DeleteNetworkInsightsAnalysis Core.Text
deleteNetworkInsightsAnalysis_networkInsightsAnalysisId = Lens.lens (\DeleteNetworkInsightsAnalysis' {networkInsightsAnalysisId} -> networkInsightsAnalysisId) (\s@DeleteNetworkInsightsAnalysis' {} a -> s {networkInsightsAnalysisId = a} :: DeleteNetworkInsightsAnalysis)

instance
  Core.AWSRequest
    DeleteNetworkInsightsAnalysis
  where
  type
    AWSResponse DeleteNetworkInsightsAnalysis =
      DeleteNetworkInsightsAnalysisResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteNetworkInsightsAnalysisResponse'
            Core.<$> (x Core..@? "networkInsightsAnalysisId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteNetworkInsightsAnalysis

instance Core.NFData DeleteNetworkInsightsAnalysis

instance Core.ToHeaders DeleteNetworkInsightsAnalysis where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DeleteNetworkInsightsAnalysis where
  toPath = Core.const "/"

instance Core.ToQuery DeleteNetworkInsightsAnalysis where
  toQuery DeleteNetworkInsightsAnalysis' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DeleteNetworkInsightsAnalysis" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "DryRun" Core.=: dryRun,
        "NetworkInsightsAnalysisId"
          Core.=: networkInsightsAnalysisId
      ]

-- | /See:/ 'newDeleteNetworkInsightsAnalysisResponse' smart constructor.
data DeleteNetworkInsightsAnalysisResponse = DeleteNetworkInsightsAnalysisResponse'
  { -- | The ID of the network insights analysis.
    networkInsightsAnalysisId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteNetworkInsightsAnalysisResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'networkInsightsAnalysisId', 'deleteNetworkInsightsAnalysisResponse_networkInsightsAnalysisId' - The ID of the network insights analysis.
--
-- 'httpStatus', 'deleteNetworkInsightsAnalysisResponse_httpStatus' - The response's http status code.
newDeleteNetworkInsightsAnalysisResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteNetworkInsightsAnalysisResponse
newDeleteNetworkInsightsAnalysisResponse pHttpStatus_ =
  DeleteNetworkInsightsAnalysisResponse'
    { networkInsightsAnalysisId =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the network insights analysis.
deleteNetworkInsightsAnalysisResponse_networkInsightsAnalysisId :: Lens.Lens' DeleteNetworkInsightsAnalysisResponse (Core.Maybe Core.Text)
deleteNetworkInsightsAnalysisResponse_networkInsightsAnalysisId = Lens.lens (\DeleteNetworkInsightsAnalysisResponse' {networkInsightsAnalysisId} -> networkInsightsAnalysisId) (\s@DeleteNetworkInsightsAnalysisResponse' {} a -> s {networkInsightsAnalysisId = a} :: DeleteNetworkInsightsAnalysisResponse)

-- | The response's http status code.
deleteNetworkInsightsAnalysisResponse_httpStatus :: Lens.Lens' DeleteNetworkInsightsAnalysisResponse Core.Int
deleteNetworkInsightsAnalysisResponse_httpStatus = Lens.lens (\DeleteNetworkInsightsAnalysisResponse' {httpStatus} -> httpStatus) (\s@DeleteNetworkInsightsAnalysisResponse' {} a -> s {httpStatus = a} :: DeleteNetworkInsightsAnalysisResponse)

instance
  Core.NFData
    DeleteNetworkInsightsAnalysisResponse
