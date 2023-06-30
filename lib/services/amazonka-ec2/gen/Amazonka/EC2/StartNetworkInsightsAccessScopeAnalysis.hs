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
-- Module      : Amazonka.EC2.StartNetworkInsightsAccessScopeAnalysis
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts analyzing the specified Network Access Scope.
module Amazonka.EC2.StartNetworkInsightsAccessScopeAnalysis
  ( -- * Creating a Request
    StartNetworkInsightsAccessScopeAnalysis (..),
    newStartNetworkInsightsAccessScopeAnalysis,

    -- * Request Lenses
    startNetworkInsightsAccessScopeAnalysis_dryRun,
    startNetworkInsightsAccessScopeAnalysis_tagSpecifications,
    startNetworkInsightsAccessScopeAnalysis_networkInsightsAccessScopeId,
    startNetworkInsightsAccessScopeAnalysis_clientToken,

    -- * Destructuring the Response
    StartNetworkInsightsAccessScopeAnalysisResponse (..),
    newStartNetworkInsightsAccessScopeAnalysisResponse,

    -- * Response Lenses
    startNetworkInsightsAccessScopeAnalysisResponse_networkInsightsAccessScopeAnalysis,
    startNetworkInsightsAccessScopeAnalysisResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartNetworkInsightsAccessScopeAnalysis' smart constructor.
data StartNetworkInsightsAccessScopeAnalysis = StartNetworkInsightsAccessScopeAnalysis'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The tags to apply.
    tagSpecifications :: Prelude.Maybe [TagSpecification],
    -- | The ID of the Network Access Scope.
    networkInsightsAccessScopeId :: Prelude.Text,
    -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to ensure idempotency>.
    clientToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartNetworkInsightsAccessScopeAnalysis' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'startNetworkInsightsAccessScopeAnalysis_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'tagSpecifications', 'startNetworkInsightsAccessScopeAnalysis_tagSpecifications' - The tags to apply.
--
-- 'networkInsightsAccessScopeId', 'startNetworkInsightsAccessScopeAnalysis_networkInsightsAccessScopeId' - The ID of the Network Access Scope.
--
-- 'clientToken', 'startNetworkInsightsAccessScopeAnalysis_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to ensure idempotency>.
newStartNetworkInsightsAccessScopeAnalysis ::
  -- | 'networkInsightsAccessScopeId'
  Prelude.Text ->
  -- | 'clientToken'
  Prelude.Text ->
  StartNetworkInsightsAccessScopeAnalysis
newStartNetworkInsightsAccessScopeAnalysis
  pNetworkInsightsAccessScopeId_
  pClientToken_ =
    StartNetworkInsightsAccessScopeAnalysis'
      { dryRun =
          Prelude.Nothing,
        tagSpecifications =
          Prelude.Nothing,
        networkInsightsAccessScopeId =
          pNetworkInsightsAccessScopeId_,
        clientToken = pClientToken_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
startNetworkInsightsAccessScopeAnalysis_dryRun :: Lens.Lens' StartNetworkInsightsAccessScopeAnalysis (Prelude.Maybe Prelude.Bool)
startNetworkInsightsAccessScopeAnalysis_dryRun = Lens.lens (\StartNetworkInsightsAccessScopeAnalysis' {dryRun} -> dryRun) (\s@StartNetworkInsightsAccessScopeAnalysis' {} a -> s {dryRun = a} :: StartNetworkInsightsAccessScopeAnalysis)

-- | The tags to apply.
startNetworkInsightsAccessScopeAnalysis_tagSpecifications :: Lens.Lens' StartNetworkInsightsAccessScopeAnalysis (Prelude.Maybe [TagSpecification])
startNetworkInsightsAccessScopeAnalysis_tagSpecifications = Lens.lens (\StartNetworkInsightsAccessScopeAnalysis' {tagSpecifications} -> tagSpecifications) (\s@StartNetworkInsightsAccessScopeAnalysis' {} a -> s {tagSpecifications = a} :: StartNetworkInsightsAccessScopeAnalysis) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the Network Access Scope.
startNetworkInsightsAccessScopeAnalysis_networkInsightsAccessScopeId :: Lens.Lens' StartNetworkInsightsAccessScopeAnalysis Prelude.Text
startNetworkInsightsAccessScopeAnalysis_networkInsightsAccessScopeId = Lens.lens (\StartNetworkInsightsAccessScopeAnalysis' {networkInsightsAccessScopeId} -> networkInsightsAccessScopeId) (\s@StartNetworkInsightsAccessScopeAnalysis' {} a -> s {networkInsightsAccessScopeId = a} :: StartNetworkInsightsAccessScopeAnalysis)

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to ensure idempotency>.
startNetworkInsightsAccessScopeAnalysis_clientToken :: Lens.Lens' StartNetworkInsightsAccessScopeAnalysis Prelude.Text
startNetworkInsightsAccessScopeAnalysis_clientToken = Lens.lens (\StartNetworkInsightsAccessScopeAnalysis' {clientToken} -> clientToken) (\s@StartNetworkInsightsAccessScopeAnalysis' {} a -> s {clientToken = a} :: StartNetworkInsightsAccessScopeAnalysis)

instance
  Core.AWSRequest
    StartNetworkInsightsAccessScopeAnalysis
  where
  type
    AWSResponse
      StartNetworkInsightsAccessScopeAnalysis =
      StartNetworkInsightsAccessScopeAnalysisResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          StartNetworkInsightsAccessScopeAnalysisResponse'
            Prelude.<$> (x Data..@? "networkInsightsAccessScopeAnalysis")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    StartNetworkInsightsAccessScopeAnalysis
  where
  hashWithSalt
    _salt
    StartNetworkInsightsAccessScopeAnalysis' {..} =
      _salt
        `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` tagSpecifications
        `Prelude.hashWithSalt` networkInsightsAccessScopeId
        `Prelude.hashWithSalt` clientToken

instance
  Prelude.NFData
    StartNetworkInsightsAccessScopeAnalysis
  where
  rnf StartNetworkInsightsAccessScopeAnalysis' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf tagSpecifications
      `Prelude.seq` Prelude.rnf networkInsightsAccessScopeId
      `Prelude.seq` Prelude.rnf clientToken

instance
  Data.ToHeaders
    StartNetworkInsightsAccessScopeAnalysis
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    StartNetworkInsightsAccessScopeAnalysis
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    StartNetworkInsightsAccessScopeAnalysis
  where
  toQuery StartNetworkInsightsAccessScopeAnalysis' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "StartNetworkInsightsAccessScopeAnalysis" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        Data.toQuery
          ( Data.toQueryList "TagSpecification"
              Prelude.<$> tagSpecifications
          ),
        "NetworkInsightsAccessScopeId"
          Data.=: networkInsightsAccessScopeId,
        "ClientToken" Data.=: clientToken
      ]

-- | /See:/ 'newStartNetworkInsightsAccessScopeAnalysisResponse' smart constructor.
data StartNetworkInsightsAccessScopeAnalysisResponse = StartNetworkInsightsAccessScopeAnalysisResponse'
  { -- | The Network Access Scope analysis.
    networkInsightsAccessScopeAnalysis :: Prelude.Maybe NetworkInsightsAccessScopeAnalysis,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartNetworkInsightsAccessScopeAnalysisResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'networkInsightsAccessScopeAnalysis', 'startNetworkInsightsAccessScopeAnalysisResponse_networkInsightsAccessScopeAnalysis' - The Network Access Scope analysis.
--
-- 'httpStatus', 'startNetworkInsightsAccessScopeAnalysisResponse_httpStatus' - The response's http status code.
newStartNetworkInsightsAccessScopeAnalysisResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartNetworkInsightsAccessScopeAnalysisResponse
newStartNetworkInsightsAccessScopeAnalysisResponse
  pHttpStatus_ =
    StartNetworkInsightsAccessScopeAnalysisResponse'
      { networkInsightsAccessScopeAnalysis =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The Network Access Scope analysis.
startNetworkInsightsAccessScopeAnalysisResponse_networkInsightsAccessScopeAnalysis :: Lens.Lens' StartNetworkInsightsAccessScopeAnalysisResponse (Prelude.Maybe NetworkInsightsAccessScopeAnalysis)
startNetworkInsightsAccessScopeAnalysisResponse_networkInsightsAccessScopeAnalysis = Lens.lens (\StartNetworkInsightsAccessScopeAnalysisResponse' {networkInsightsAccessScopeAnalysis} -> networkInsightsAccessScopeAnalysis) (\s@StartNetworkInsightsAccessScopeAnalysisResponse' {} a -> s {networkInsightsAccessScopeAnalysis = a} :: StartNetworkInsightsAccessScopeAnalysisResponse)

-- | The response's http status code.
startNetworkInsightsAccessScopeAnalysisResponse_httpStatus :: Lens.Lens' StartNetworkInsightsAccessScopeAnalysisResponse Prelude.Int
startNetworkInsightsAccessScopeAnalysisResponse_httpStatus = Lens.lens (\StartNetworkInsightsAccessScopeAnalysisResponse' {httpStatus} -> httpStatus) (\s@StartNetworkInsightsAccessScopeAnalysisResponse' {} a -> s {httpStatus = a} :: StartNetworkInsightsAccessScopeAnalysisResponse)

instance
  Prelude.NFData
    StartNetworkInsightsAccessScopeAnalysisResponse
  where
  rnf
    StartNetworkInsightsAccessScopeAnalysisResponse' {..} =
      Prelude.rnf networkInsightsAccessScopeAnalysis
        `Prelude.seq` Prelude.rnf httpStatus
