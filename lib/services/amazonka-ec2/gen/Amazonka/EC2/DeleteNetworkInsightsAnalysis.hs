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
-- Module      : Amazonka.EC2.DeleteNetworkInsightsAnalysis
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified network insights analysis.
module Amazonka.EC2.DeleteNetworkInsightsAnalysis
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Core.AWSRequest
    DeleteNetworkInsightsAnalysis
  where
  type
    AWSResponse DeleteNetworkInsightsAnalysis =
      DeleteNetworkInsightsAnalysisResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteNetworkInsightsAnalysisResponse'
            Prelude.<$> (x Data..@? "networkInsightsAnalysisId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteNetworkInsightsAnalysis
  where
  hashWithSalt _salt DeleteNetworkInsightsAnalysis' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` networkInsightsAnalysisId

instance Prelude.NFData DeleteNetworkInsightsAnalysis where
  rnf DeleteNetworkInsightsAnalysis' {..} =
    Prelude.rnf dryRun `Prelude.seq`
      Prelude.rnf networkInsightsAnalysisId

instance Data.ToHeaders DeleteNetworkInsightsAnalysis where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteNetworkInsightsAnalysis where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteNetworkInsightsAnalysis where
  toQuery DeleteNetworkInsightsAnalysis' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DeleteNetworkInsightsAnalysis" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "NetworkInsightsAnalysisId"
          Data.=: networkInsightsAnalysisId
      ]

-- | /See:/ 'newDeleteNetworkInsightsAnalysisResponse' smart constructor.
data DeleteNetworkInsightsAnalysisResponse = DeleteNetworkInsightsAnalysisResponse'
  { -- | The ID of the network insights analysis.
    networkInsightsAnalysisId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf DeleteNetworkInsightsAnalysisResponse' {..} =
    Prelude.rnf networkInsightsAnalysisId `Prelude.seq`
      Prelude.rnf httpStatus
