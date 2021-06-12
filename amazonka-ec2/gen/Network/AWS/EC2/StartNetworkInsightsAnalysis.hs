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
-- Module      : Network.AWS.EC2.StartNetworkInsightsAnalysis
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts analyzing the specified path. If the path is reachable, the
-- operation returns the shortest feasible path.
module Network.AWS.EC2.StartNetworkInsightsAnalysis
  ( -- * Creating a Request
    StartNetworkInsightsAnalysis (..),
    newStartNetworkInsightsAnalysis,

    -- * Request Lenses
    startNetworkInsightsAnalysis_tagSpecifications,
    startNetworkInsightsAnalysis_filterInArns,
    startNetworkInsightsAnalysis_dryRun,
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

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStartNetworkInsightsAnalysis' smart constructor.
data StartNetworkInsightsAnalysis = StartNetworkInsightsAnalysis'
  { -- | The tags to apply.
    tagSpecifications :: Core.Maybe [TagSpecification],
    -- | The Amazon Resource Names (ARN) of the resources that the path must
    -- traverse.
    filterInArns :: Core.Maybe [Core.Text],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The ID of the path.
    networkInsightsPathId :: Core.Text,
    -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency>.
    clientToken :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StartNetworkInsightsAnalysis' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tagSpecifications', 'startNetworkInsightsAnalysis_tagSpecifications' - The tags to apply.
--
-- 'filterInArns', 'startNetworkInsightsAnalysis_filterInArns' - The Amazon Resource Names (ARN) of the resources that the path must
-- traverse.
--
-- 'dryRun', 'startNetworkInsightsAnalysis_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'networkInsightsPathId', 'startNetworkInsightsAnalysis_networkInsightsPathId' - The ID of the path.
--
-- 'clientToken', 'startNetworkInsightsAnalysis_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency>.
newStartNetworkInsightsAnalysis ::
  -- | 'networkInsightsPathId'
  Core.Text ->
  -- | 'clientToken'
  Core.Text ->
  StartNetworkInsightsAnalysis
newStartNetworkInsightsAnalysis
  pNetworkInsightsPathId_
  pClientToken_ =
    StartNetworkInsightsAnalysis'
      { tagSpecifications =
          Core.Nothing,
        filterInArns = Core.Nothing,
        dryRun = Core.Nothing,
        networkInsightsPathId =
          pNetworkInsightsPathId_,
        clientToken = pClientToken_
      }

-- | The tags to apply.
startNetworkInsightsAnalysis_tagSpecifications :: Lens.Lens' StartNetworkInsightsAnalysis (Core.Maybe [TagSpecification])
startNetworkInsightsAnalysis_tagSpecifications = Lens.lens (\StartNetworkInsightsAnalysis' {tagSpecifications} -> tagSpecifications) (\s@StartNetworkInsightsAnalysis' {} a -> s {tagSpecifications = a} :: StartNetworkInsightsAnalysis) Core.. Lens.mapping Lens._Coerce

-- | The Amazon Resource Names (ARN) of the resources that the path must
-- traverse.
startNetworkInsightsAnalysis_filterInArns :: Lens.Lens' StartNetworkInsightsAnalysis (Core.Maybe [Core.Text])
startNetworkInsightsAnalysis_filterInArns = Lens.lens (\StartNetworkInsightsAnalysis' {filterInArns} -> filterInArns) (\s@StartNetworkInsightsAnalysis' {} a -> s {filterInArns = a} :: StartNetworkInsightsAnalysis) Core.. Lens.mapping Lens._Coerce

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
startNetworkInsightsAnalysis_dryRun :: Lens.Lens' StartNetworkInsightsAnalysis (Core.Maybe Core.Bool)
startNetworkInsightsAnalysis_dryRun = Lens.lens (\StartNetworkInsightsAnalysis' {dryRun} -> dryRun) (\s@StartNetworkInsightsAnalysis' {} a -> s {dryRun = a} :: StartNetworkInsightsAnalysis)

-- | The ID of the path.
startNetworkInsightsAnalysis_networkInsightsPathId :: Lens.Lens' StartNetworkInsightsAnalysis Core.Text
startNetworkInsightsAnalysis_networkInsightsPathId = Lens.lens (\StartNetworkInsightsAnalysis' {networkInsightsPathId} -> networkInsightsPathId) (\s@StartNetworkInsightsAnalysis' {} a -> s {networkInsightsPathId = a} :: StartNetworkInsightsAnalysis)

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency>.
startNetworkInsightsAnalysis_clientToken :: Lens.Lens' StartNetworkInsightsAnalysis Core.Text
startNetworkInsightsAnalysis_clientToken = Lens.lens (\StartNetworkInsightsAnalysis' {clientToken} -> clientToken) (\s@StartNetworkInsightsAnalysis' {} a -> s {clientToken = a} :: StartNetworkInsightsAnalysis)

instance Core.AWSRequest StartNetworkInsightsAnalysis where
  type
    AWSResponse StartNetworkInsightsAnalysis =
      StartNetworkInsightsAnalysisResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          StartNetworkInsightsAnalysisResponse'
            Core.<$> (x Core..@? "networkInsightsAnalysis")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable StartNetworkInsightsAnalysis

instance Core.NFData StartNetworkInsightsAnalysis

instance Core.ToHeaders StartNetworkInsightsAnalysis where
  toHeaders = Core.const Core.mempty

instance Core.ToPath StartNetworkInsightsAnalysis where
  toPath = Core.const "/"

instance Core.ToQuery StartNetworkInsightsAnalysis where
  toQuery StartNetworkInsightsAnalysis' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("StartNetworkInsightsAnalysis" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        Core.toQuery
          ( Core.toQueryList "TagSpecification"
              Core.<$> tagSpecifications
          ),
        Core.toQuery
          ( Core.toQueryList "FilterInArn"
              Core.<$> filterInArns
          ),
        "DryRun" Core.=: dryRun,
        "NetworkInsightsPathId"
          Core.=: networkInsightsPathId,
        "ClientToken" Core.=: clientToken
      ]

-- | /See:/ 'newStartNetworkInsightsAnalysisResponse' smart constructor.
data StartNetworkInsightsAnalysisResponse = StartNetworkInsightsAnalysisResponse'
  { -- | Information about the network insights analysis.
    networkInsightsAnalysis :: Core.Maybe NetworkInsightsAnalysis,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  StartNetworkInsightsAnalysisResponse
newStartNetworkInsightsAnalysisResponse pHttpStatus_ =
  StartNetworkInsightsAnalysisResponse'
    { networkInsightsAnalysis =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the network insights analysis.
startNetworkInsightsAnalysisResponse_networkInsightsAnalysis :: Lens.Lens' StartNetworkInsightsAnalysisResponse (Core.Maybe NetworkInsightsAnalysis)
startNetworkInsightsAnalysisResponse_networkInsightsAnalysis = Lens.lens (\StartNetworkInsightsAnalysisResponse' {networkInsightsAnalysis} -> networkInsightsAnalysis) (\s@StartNetworkInsightsAnalysisResponse' {} a -> s {networkInsightsAnalysis = a} :: StartNetworkInsightsAnalysisResponse)

-- | The response's http status code.
startNetworkInsightsAnalysisResponse_httpStatus :: Lens.Lens' StartNetworkInsightsAnalysisResponse Core.Int
startNetworkInsightsAnalysisResponse_httpStatus = Lens.lens (\StartNetworkInsightsAnalysisResponse' {httpStatus} -> httpStatus) (\s@StartNetworkInsightsAnalysisResponse' {} a -> s {httpStatus = a} :: StartNetworkInsightsAnalysisResponse)

instance
  Core.NFData
    StartNetworkInsightsAnalysisResponse
