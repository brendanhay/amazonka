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
-- Module      : Amazonka.EC2.StartNetworkInsightsAnalysis
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts analyzing the specified path. If the path is reachable, the
-- operation returns the shortest feasible path.
module Amazonka.EC2.StartNetworkInsightsAnalysis
  ( -- * Creating a Request
    StartNetworkInsightsAnalysis (..),
    newStartNetworkInsightsAnalysis,

    -- * Request Lenses
    startNetworkInsightsAnalysis_additionalAccounts,
    startNetworkInsightsAnalysis_dryRun,
    startNetworkInsightsAnalysis_filterInArns,
    startNetworkInsightsAnalysis_tagSpecifications,
    startNetworkInsightsAnalysis_networkInsightsPathId,
    startNetworkInsightsAnalysis_clientToken,

    -- * Destructuring the Response
    StartNetworkInsightsAnalysisResponse (..),
    newStartNetworkInsightsAnalysisResponse,

    -- * Response Lenses
    startNetworkInsightsAnalysisResponse_networkInsightsAnalysis,
    startNetworkInsightsAnalysisResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartNetworkInsightsAnalysis' smart constructor.
data StartNetworkInsightsAnalysis = StartNetworkInsightsAnalysis'
  { -- | The member accounts that contain resources that the path can traverse.
    additionalAccounts :: Prelude.Maybe [Prelude.Text],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Names (ARN) of the resources that the path must
    -- traverse.
    filterInArns :: Prelude.Maybe [Prelude.Text],
    -- | The tags to apply.
    tagSpecifications :: Prelude.Maybe [TagSpecification],
    -- | The ID of the path.
    networkInsightsPathId :: Prelude.Text,
    -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to ensure idempotency>.
    clientToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartNetworkInsightsAnalysis' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'additionalAccounts', 'startNetworkInsightsAnalysis_additionalAccounts' - The member accounts that contain resources that the path can traverse.
--
-- 'dryRun', 'startNetworkInsightsAnalysis_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'filterInArns', 'startNetworkInsightsAnalysis_filterInArns' - The Amazon Resource Names (ARN) of the resources that the path must
-- traverse.
--
-- 'tagSpecifications', 'startNetworkInsightsAnalysis_tagSpecifications' - The tags to apply.
--
-- 'networkInsightsPathId', 'startNetworkInsightsAnalysis_networkInsightsPathId' - The ID of the path.
--
-- 'clientToken', 'startNetworkInsightsAnalysis_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to ensure idempotency>.
newStartNetworkInsightsAnalysis ::
  -- | 'networkInsightsPathId'
  Prelude.Text ->
  -- | 'clientToken'
  Prelude.Text ->
  StartNetworkInsightsAnalysis
newStartNetworkInsightsAnalysis
  pNetworkInsightsPathId_
  pClientToken_ =
    StartNetworkInsightsAnalysis'
      { additionalAccounts =
          Prelude.Nothing,
        dryRun = Prelude.Nothing,
        filterInArns = Prelude.Nothing,
        tagSpecifications = Prelude.Nothing,
        networkInsightsPathId =
          pNetworkInsightsPathId_,
        clientToken = pClientToken_
      }

-- | The member accounts that contain resources that the path can traverse.
startNetworkInsightsAnalysis_additionalAccounts :: Lens.Lens' StartNetworkInsightsAnalysis (Prelude.Maybe [Prelude.Text])
startNetworkInsightsAnalysis_additionalAccounts = Lens.lens (\StartNetworkInsightsAnalysis' {additionalAccounts} -> additionalAccounts) (\s@StartNetworkInsightsAnalysis' {} a -> s {additionalAccounts = a} :: StartNetworkInsightsAnalysis) Prelude.. Lens.mapping Lens.coerced

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
startNetworkInsightsAnalysis_dryRun :: Lens.Lens' StartNetworkInsightsAnalysis (Prelude.Maybe Prelude.Bool)
startNetworkInsightsAnalysis_dryRun = Lens.lens (\StartNetworkInsightsAnalysis' {dryRun} -> dryRun) (\s@StartNetworkInsightsAnalysis' {} a -> s {dryRun = a} :: StartNetworkInsightsAnalysis)

-- | The Amazon Resource Names (ARN) of the resources that the path must
-- traverse.
startNetworkInsightsAnalysis_filterInArns :: Lens.Lens' StartNetworkInsightsAnalysis (Prelude.Maybe [Prelude.Text])
startNetworkInsightsAnalysis_filterInArns = Lens.lens (\StartNetworkInsightsAnalysis' {filterInArns} -> filterInArns) (\s@StartNetworkInsightsAnalysis' {} a -> s {filterInArns = a} :: StartNetworkInsightsAnalysis) Prelude.. Lens.mapping Lens.coerced

-- | The tags to apply.
startNetworkInsightsAnalysis_tagSpecifications :: Lens.Lens' StartNetworkInsightsAnalysis (Prelude.Maybe [TagSpecification])
startNetworkInsightsAnalysis_tagSpecifications = Lens.lens (\StartNetworkInsightsAnalysis' {tagSpecifications} -> tagSpecifications) (\s@StartNetworkInsightsAnalysis' {} a -> s {tagSpecifications = a} :: StartNetworkInsightsAnalysis) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the path.
startNetworkInsightsAnalysis_networkInsightsPathId :: Lens.Lens' StartNetworkInsightsAnalysis Prelude.Text
startNetworkInsightsAnalysis_networkInsightsPathId = Lens.lens (\StartNetworkInsightsAnalysis' {networkInsightsPathId} -> networkInsightsPathId) (\s@StartNetworkInsightsAnalysis' {} a -> s {networkInsightsPathId = a} :: StartNetworkInsightsAnalysis)

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to ensure idempotency>.
startNetworkInsightsAnalysis_clientToken :: Lens.Lens' StartNetworkInsightsAnalysis Prelude.Text
startNetworkInsightsAnalysis_clientToken = Lens.lens (\StartNetworkInsightsAnalysis' {clientToken} -> clientToken) (\s@StartNetworkInsightsAnalysis' {} a -> s {clientToken = a} :: StartNetworkInsightsAnalysis)

instance Core.AWSRequest StartNetworkInsightsAnalysis where
  type
    AWSResponse StartNetworkInsightsAnalysis =
      StartNetworkInsightsAnalysisResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          StartNetworkInsightsAnalysisResponse'
            Prelude.<$> (x Data..@? "networkInsightsAnalysis")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    StartNetworkInsightsAnalysis
  where
  hashWithSalt _salt StartNetworkInsightsAnalysis' {..} =
    _salt
      `Prelude.hashWithSalt` additionalAccounts
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` filterInArns
      `Prelude.hashWithSalt` tagSpecifications
      `Prelude.hashWithSalt` networkInsightsPathId
      `Prelude.hashWithSalt` clientToken

instance Prelude.NFData StartNetworkInsightsAnalysis where
  rnf StartNetworkInsightsAnalysis' {..} =
    Prelude.rnf additionalAccounts
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf filterInArns
      `Prelude.seq` Prelude.rnf tagSpecifications
      `Prelude.seq` Prelude.rnf networkInsightsPathId
      `Prelude.seq` Prelude.rnf clientToken

instance Data.ToHeaders StartNetworkInsightsAnalysis where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath StartNetworkInsightsAnalysis where
  toPath = Prelude.const "/"

instance Data.ToQuery StartNetworkInsightsAnalysis where
  toQuery StartNetworkInsightsAnalysis' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "StartNetworkInsightsAnalysis" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        Data.toQuery
          ( Data.toQueryList "AdditionalAccount"
              Prelude.<$> additionalAccounts
          ),
        "DryRun" Data.=: dryRun,
        Data.toQuery
          ( Data.toQueryList "FilterInArn"
              Prelude.<$> filterInArns
          ),
        Data.toQuery
          ( Data.toQueryList "TagSpecification"
              Prelude.<$> tagSpecifications
          ),
        "NetworkInsightsPathId"
          Data.=: networkInsightsPathId,
        "ClientToken" Data.=: clientToken
      ]

-- | /See:/ 'newStartNetworkInsightsAnalysisResponse' smart constructor.
data StartNetworkInsightsAnalysisResponse = StartNetworkInsightsAnalysisResponse'
  { -- | Information about the network insights analysis.
    networkInsightsAnalysis :: Prelude.Maybe NetworkInsightsAnalysis,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartNetworkInsightsAnalysisResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'networkInsightsAnalysis', 'startNetworkInsightsAnalysisResponse_networkInsightsAnalysis' - Information about the network insights analysis.
--
-- 'httpStatus', 'startNetworkInsightsAnalysisResponse_httpStatus' - The response's http status code.
newStartNetworkInsightsAnalysisResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartNetworkInsightsAnalysisResponse
newStartNetworkInsightsAnalysisResponse pHttpStatus_ =
  StartNetworkInsightsAnalysisResponse'
    { networkInsightsAnalysis =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the network insights analysis.
startNetworkInsightsAnalysisResponse_networkInsightsAnalysis :: Lens.Lens' StartNetworkInsightsAnalysisResponse (Prelude.Maybe NetworkInsightsAnalysis)
startNetworkInsightsAnalysisResponse_networkInsightsAnalysis = Lens.lens (\StartNetworkInsightsAnalysisResponse' {networkInsightsAnalysis} -> networkInsightsAnalysis) (\s@StartNetworkInsightsAnalysisResponse' {} a -> s {networkInsightsAnalysis = a} :: StartNetworkInsightsAnalysisResponse)

-- | The response's http status code.
startNetworkInsightsAnalysisResponse_httpStatus :: Lens.Lens' StartNetworkInsightsAnalysisResponse Prelude.Int
startNetworkInsightsAnalysisResponse_httpStatus = Lens.lens (\StartNetworkInsightsAnalysisResponse' {httpStatus} -> httpStatus) (\s@StartNetworkInsightsAnalysisResponse' {} a -> s {httpStatus = a} :: StartNetworkInsightsAnalysisResponse)

instance
  Prelude.NFData
    StartNetworkInsightsAnalysisResponse
  where
  rnf StartNetworkInsightsAnalysisResponse' {..} =
    Prelude.rnf networkInsightsAnalysis
      `Prelude.seq` Prelude.rnf httpStatus
