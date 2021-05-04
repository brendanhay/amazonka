{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteNetworkInsightsAnalysis' smart constructor.
data DeleteNetworkInsightsAnalysis = DeleteNetworkInsightsAnalysis'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the network insights analysis.
    networkInsightsAnalysisId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  DeleteNetworkInsightsAnalysis
newDeleteNetworkInsightsAnalysis
  pNetworkInsightsAnalysisId_ =
    DeleteNetworkInsightsAnalysis'
      { dryRun =
          Prelude.Nothing,
        networkInsightsAnalysisId =
          pNetworkInsightsAnalysisId_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteNetworkInsightsAnalysis_dryRun :: Lens.Lens' DeleteNetworkInsightsAnalysis (Prelude.Maybe Prelude.Bool)
deleteNetworkInsightsAnalysis_dryRun = Lens.lens (\DeleteNetworkInsightsAnalysis' {dryRun} -> dryRun) (\s@DeleteNetworkInsightsAnalysis' {} a -> s {dryRun = a} :: DeleteNetworkInsightsAnalysis)

-- | The ID of the network insights analysis.
deleteNetworkInsightsAnalysis_networkInsightsAnalysisId :: Lens.Lens' DeleteNetworkInsightsAnalysis Prelude.Text
deleteNetworkInsightsAnalysis_networkInsightsAnalysisId = Lens.lens (\DeleteNetworkInsightsAnalysis' {networkInsightsAnalysisId} -> networkInsightsAnalysisId) (\s@DeleteNetworkInsightsAnalysis' {} a -> s {networkInsightsAnalysisId = a} :: DeleteNetworkInsightsAnalysis)

instance
  Prelude.AWSRequest
    DeleteNetworkInsightsAnalysis
  where
  type
    Rs DeleteNetworkInsightsAnalysis =
      DeleteNetworkInsightsAnalysisResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteNetworkInsightsAnalysisResponse'
            Prelude.<$> (x Prelude..@? "networkInsightsAnalysisId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteNetworkInsightsAnalysis

instance Prelude.NFData DeleteNetworkInsightsAnalysis

instance
  Prelude.ToHeaders
    DeleteNetworkInsightsAnalysis
  where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DeleteNetworkInsightsAnalysis where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    DeleteNetworkInsightsAnalysis
  where
  toQuery DeleteNetworkInsightsAnalysis' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ( "DeleteNetworkInsightsAnalysis" ::
                         Prelude.ByteString
                     ),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Prelude.=: dryRun,
        "NetworkInsightsAnalysisId"
          Prelude.=: networkInsightsAnalysisId
      ]

-- | /See:/ 'newDeleteNetworkInsightsAnalysisResponse' smart constructor.
data DeleteNetworkInsightsAnalysisResponse = DeleteNetworkInsightsAnalysisResponse'
  { -- | The ID of the network insights analysis.
    networkInsightsAnalysisId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DeleteNetworkInsightsAnalysisResponse
newDeleteNetworkInsightsAnalysisResponse pHttpStatus_ =
  DeleteNetworkInsightsAnalysisResponse'
    { networkInsightsAnalysisId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the network insights analysis.
deleteNetworkInsightsAnalysisResponse_networkInsightsAnalysisId :: Lens.Lens' DeleteNetworkInsightsAnalysisResponse (Prelude.Maybe Prelude.Text)
deleteNetworkInsightsAnalysisResponse_networkInsightsAnalysisId = Lens.lens (\DeleteNetworkInsightsAnalysisResponse' {networkInsightsAnalysisId} -> networkInsightsAnalysisId) (\s@DeleteNetworkInsightsAnalysisResponse' {} a -> s {networkInsightsAnalysisId = a} :: DeleteNetworkInsightsAnalysisResponse)

-- | The response's http status code.
deleteNetworkInsightsAnalysisResponse_httpStatus :: Lens.Lens' DeleteNetworkInsightsAnalysisResponse Prelude.Int
deleteNetworkInsightsAnalysisResponse_httpStatus = Lens.lens (\DeleteNetworkInsightsAnalysisResponse' {httpStatus} -> httpStatus) (\s@DeleteNetworkInsightsAnalysisResponse' {} a -> s {httpStatus = a} :: DeleteNetworkInsightsAnalysisResponse)

instance
  Prelude.NFData
    DeleteNetworkInsightsAnalysisResponse
