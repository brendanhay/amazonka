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
-- Module      : Amazonka.EC2.DeleteNetworkInsightsAccessScopeAnalysis
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified Network Access Scope analysis.
module Amazonka.EC2.DeleteNetworkInsightsAccessScopeAnalysis
  ( -- * Creating a Request
    DeleteNetworkInsightsAccessScopeAnalysis (..),
    newDeleteNetworkInsightsAccessScopeAnalysis,

    -- * Request Lenses
    deleteNetworkInsightsAccessScopeAnalysis_dryRun,
    deleteNetworkInsightsAccessScopeAnalysis_networkInsightsAccessScopeAnalysisId,

    -- * Destructuring the Response
    DeleteNetworkInsightsAccessScopeAnalysisResponse (..),
    newDeleteNetworkInsightsAccessScopeAnalysisResponse,

    -- * Response Lenses
    deleteNetworkInsightsAccessScopeAnalysisResponse_networkInsightsAccessScopeAnalysisId,
    deleteNetworkInsightsAccessScopeAnalysisResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteNetworkInsightsAccessScopeAnalysis' smart constructor.
data DeleteNetworkInsightsAccessScopeAnalysis = DeleteNetworkInsightsAccessScopeAnalysis'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the Network Access Scope analysis.
    networkInsightsAccessScopeAnalysisId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteNetworkInsightsAccessScopeAnalysis' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'deleteNetworkInsightsAccessScopeAnalysis_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'networkInsightsAccessScopeAnalysisId', 'deleteNetworkInsightsAccessScopeAnalysis_networkInsightsAccessScopeAnalysisId' - The ID of the Network Access Scope analysis.
newDeleteNetworkInsightsAccessScopeAnalysis ::
  -- | 'networkInsightsAccessScopeAnalysisId'
  Prelude.Text ->
  DeleteNetworkInsightsAccessScopeAnalysis
newDeleteNetworkInsightsAccessScopeAnalysis
  pNetworkInsightsAccessScopeAnalysisId_ =
    DeleteNetworkInsightsAccessScopeAnalysis'
      { dryRun =
          Prelude.Nothing,
        networkInsightsAccessScopeAnalysisId =
          pNetworkInsightsAccessScopeAnalysisId_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteNetworkInsightsAccessScopeAnalysis_dryRun :: Lens.Lens' DeleteNetworkInsightsAccessScopeAnalysis (Prelude.Maybe Prelude.Bool)
deleteNetworkInsightsAccessScopeAnalysis_dryRun = Lens.lens (\DeleteNetworkInsightsAccessScopeAnalysis' {dryRun} -> dryRun) (\s@DeleteNetworkInsightsAccessScopeAnalysis' {} a -> s {dryRun = a} :: DeleteNetworkInsightsAccessScopeAnalysis)

-- | The ID of the Network Access Scope analysis.
deleteNetworkInsightsAccessScopeAnalysis_networkInsightsAccessScopeAnalysisId :: Lens.Lens' DeleteNetworkInsightsAccessScopeAnalysis Prelude.Text
deleteNetworkInsightsAccessScopeAnalysis_networkInsightsAccessScopeAnalysisId = Lens.lens (\DeleteNetworkInsightsAccessScopeAnalysis' {networkInsightsAccessScopeAnalysisId} -> networkInsightsAccessScopeAnalysisId) (\s@DeleteNetworkInsightsAccessScopeAnalysis' {} a -> s {networkInsightsAccessScopeAnalysisId = a} :: DeleteNetworkInsightsAccessScopeAnalysis)

instance
  Core.AWSRequest
    DeleteNetworkInsightsAccessScopeAnalysis
  where
  type
    AWSResponse
      DeleteNetworkInsightsAccessScopeAnalysis =
      DeleteNetworkInsightsAccessScopeAnalysisResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteNetworkInsightsAccessScopeAnalysisResponse'
            Prelude.<$> (x Data..@? "networkInsightsAccessScopeAnalysisId")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteNetworkInsightsAccessScopeAnalysis
  where
  hashWithSalt
    _salt
    DeleteNetworkInsightsAccessScopeAnalysis' {..} =
      _salt `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` networkInsightsAccessScopeAnalysisId

instance
  Prelude.NFData
    DeleteNetworkInsightsAccessScopeAnalysis
  where
  rnf DeleteNetworkInsightsAccessScopeAnalysis' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf networkInsightsAccessScopeAnalysisId

instance
  Data.ToHeaders
    DeleteNetworkInsightsAccessScopeAnalysis
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    DeleteNetworkInsightsAccessScopeAnalysis
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DeleteNetworkInsightsAccessScopeAnalysis
  where
  toQuery DeleteNetworkInsightsAccessScopeAnalysis' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DeleteNetworkInsightsAccessScopeAnalysis" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "NetworkInsightsAccessScopeAnalysisId"
          Data.=: networkInsightsAccessScopeAnalysisId
      ]

-- | /See:/ 'newDeleteNetworkInsightsAccessScopeAnalysisResponse' smart constructor.
data DeleteNetworkInsightsAccessScopeAnalysisResponse = DeleteNetworkInsightsAccessScopeAnalysisResponse'
  { -- | The ID of the Network Access Scope analysis.
    networkInsightsAccessScopeAnalysisId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteNetworkInsightsAccessScopeAnalysisResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'networkInsightsAccessScopeAnalysisId', 'deleteNetworkInsightsAccessScopeAnalysisResponse_networkInsightsAccessScopeAnalysisId' - The ID of the Network Access Scope analysis.
--
-- 'httpStatus', 'deleteNetworkInsightsAccessScopeAnalysisResponse_httpStatus' - The response's http status code.
newDeleteNetworkInsightsAccessScopeAnalysisResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteNetworkInsightsAccessScopeAnalysisResponse
newDeleteNetworkInsightsAccessScopeAnalysisResponse
  pHttpStatus_ =
    DeleteNetworkInsightsAccessScopeAnalysisResponse'
      { networkInsightsAccessScopeAnalysisId =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The ID of the Network Access Scope analysis.
deleteNetworkInsightsAccessScopeAnalysisResponse_networkInsightsAccessScopeAnalysisId :: Lens.Lens' DeleteNetworkInsightsAccessScopeAnalysisResponse (Prelude.Maybe Prelude.Text)
deleteNetworkInsightsAccessScopeAnalysisResponse_networkInsightsAccessScopeAnalysisId = Lens.lens (\DeleteNetworkInsightsAccessScopeAnalysisResponse' {networkInsightsAccessScopeAnalysisId} -> networkInsightsAccessScopeAnalysisId) (\s@DeleteNetworkInsightsAccessScopeAnalysisResponse' {} a -> s {networkInsightsAccessScopeAnalysisId = a} :: DeleteNetworkInsightsAccessScopeAnalysisResponse)

-- | The response's http status code.
deleteNetworkInsightsAccessScopeAnalysisResponse_httpStatus :: Lens.Lens' DeleteNetworkInsightsAccessScopeAnalysisResponse Prelude.Int
deleteNetworkInsightsAccessScopeAnalysisResponse_httpStatus = Lens.lens (\DeleteNetworkInsightsAccessScopeAnalysisResponse' {httpStatus} -> httpStatus) (\s@DeleteNetworkInsightsAccessScopeAnalysisResponse' {} a -> s {httpStatus = a} :: DeleteNetworkInsightsAccessScopeAnalysisResponse)

instance
  Prelude.NFData
    DeleteNetworkInsightsAccessScopeAnalysisResponse
  where
  rnf
    DeleteNetworkInsightsAccessScopeAnalysisResponse' {..} =
      Prelude.rnf networkInsightsAccessScopeAnalysisId
        `Prelude.seq` Prelude.rnf httpStatus
