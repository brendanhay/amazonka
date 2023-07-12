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
-- Module      : Amazonka.NetworkManager.StartRouteAnalysis
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts analyzing the routing path between the specified source and
-- destination. For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/tgw/route-analyzer.html Route Analyzer>.
module Amazonka.NetworkManager.StartRouteAnalysis
  ( -- * Creating a Request
    StartRouteAnalysis (..),
    newStartRouteAnalysis,

    -- * Request Lenses
    startRouteAnalysis_includeReturnPath,
    startRouteAnalysis_useMiddleboxes,
    startRouteAnalysis_globalNetworkId,
    startRouteAnalysis_source,
    startRouteAnalysis_destination,

    -- * Destructuring the Response
    StartRouteAnalysisResponse (..),
    newStartRouteAnalysisResponse,

    -- * Response Lenses
    startRouteAnalysisResponse_routeAnalysis,
    startRouteAnalysisResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartRouteAnalysis' smart constructor.
data StartRouteAnalysis = StartRouteAnalysis'
  { -- | Indicates whether to analyze the return path. The default is @false@.
    includeReturnPath :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether to include the location of middlebox appliances in the
    -- route analysis. The default is @false@.
    useMiddleboxes :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the global network.
    globalNetworkId :: Prelude.Text,
    -- | The source from which traffic originates.
    source :: RouteAnalysisEndpointOptionsSpecification,
    -- | The destination.
    destination :: RouteAnalysisEndpointOptionsSpecification
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartRouteAnalysis' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'includeReturnPath', 'startRouteAnalysis_includeReturnPath' - Indicates whether to analyze the return path. The default is @false@.
--
-- 'useMiddleboxes', 'startRouteAnalysis_useMiddleboxes' - Indicates whether to include the location of middlebox appliances in the
-- route analysis. The default is @false@.
--
-- 'globalNetworkId', 'startRouteAnalysis_globalNetworkId' - The ID of the global network.
--
-- 'source', 'startRouteAnalysis_source' - The source from which traffic originates.
--
-- 'destination', 'startRouteAnalysis_destination' - The destination.
newStartRouteAnalysis ::
  -- | 'globalNetworkId'
  Prelude.Text ->
  -- | 'source'
  RouteAnalysisEndpointOptionsSpecification ->
  -- | 'destination'
  RouteAnalysisEndpointOptionsSpecification ->
  StartRouteAnalysis
newStartRouteAnalysis
  pGlobalNetworkId_
  pSource_
  pDestination_ =
    StartRouteAnalysis'
      { includeReturnPath =
          Prelude.Nothing,
        useMiddleboxes = Prelude.Nothing,
        globalNetworkId = pGlobalNetworkId_,
        source = pSource_,
        destination = pDestination_
      }

-- | Indicates whether to analyze the return path. The default is @false@.
startRouteAnalysis_includeReturnPath :: Lens.Lens' StartRouteAnalysis (Prelude.Maybe Prelude.Bool)
startRouteAnalysis_includeReturnPath = Lens.lens (\StartRouteAnalysis' {includeReturnPath} -> includeReturnPath) (\s@StartRouteAnalysis' {} a -> s {includeReturnPath = a} :: StartRouteAnalysis)

-- | Indicates whether to include the location of middlebox appliances in the
-- route analysis. The default is @false@.
startRouteAnalysis_useMiddleboxes :: Lens.Lens' StartRouteAnalysis (Prelude.Maybe Prelude.Bool)
startRouteAnalysis_useMiddleboxes = Lens.lens (\StartRouteAnalysis' {useMiddleboxes} -> useMiddleboxes) (\s@StartRouteAnalysis' {} a -> s {useMiddleboxes = a} :: StartRouteAnalysis)

-- | The ID of the global network.
startRouteAnalysis_globalNetworkId :: Lens.Lens' StartRouteAnalysis Prelude.Text
startRouteAnalysis_globalNetworkId = Lens.lens (\StartRouteAnalysis' {globalNetworkId} -> globalNetworkId) (\s@StartRouteAnalysis' {} a -> s {globalNetworkId = a} :: StartRouteAnalysis)

-- | The source from which traffic originates.
startRouteAnalysis_source :: Lens.Lens' StartRouteAnalysis RouteAnalysisEndpointOptionsSpecification
startRouteAnalysis_source = Lens.lens (\StartRouteAnalysis' {source} -> source) (\s@StartRouteAnalysis' {} a -> s {source = a} :: StartRouteAnalysis)

-- | The destination.
startRouteAnalysis_destination :: Lens.Lens' StartRouteAnalysis RouteAnalysisEndpointOptionsSpecification
startRouteAnalysis_destination = Lens.lens (\StartRouteAnalysis' {destination} -> destination) (\s@StartRouteAnalysis' {} a -> s {destination = a} :: StartRouteAnalysis)

instance Core.AWSRequest StartRouteAnalysis where
  type
    AWSResponse StartRouteAnalysis =
      StartRouteAnalysisResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartRouteAnalysisResponse'
            Prelude.<$> (x Data..?> "RouteAnalysis")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartRouteAnalysis where
  hashWithSalt _salt StartRouteAnalysis' {..} =
    _salt
      `Prelude.hashWithSalt` includeReturnPath
      `Prelude.hashWithSalt` useMiddleboxes
      `Prelude.hashWithSalt` globalNetworkId
      `Prelude.hashWithSalt` source
      `Prelude.hashWithSalt` destination

instance Prelude.NFData StartRouteAnalysis where
  rnf StartRouteAnalysis' {..} =
    Prelude.rnf includeReturnPath
      `Prelude.seq` Prelude.rnf useMiddleboxes
      `Prelude.seq` Prelude.rnf globalNetworkId
      `Prelude.seq` Prelude.rnf source
      `Prelude.seq` Prelude.rnf destination

instance Data.ToHeaders StartRouteAnalysis where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartRouteAnalysis where
  toJSON StartRouteAnalysis' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("IncludeReturnPath" Data..=)
              Prelude.<$> includeReturnPath,
            ("UseMiddleboxes" Data..=)
              Prelude.<$> useMiddleboxes,
            Prelude.Just ("Source" Data..= source),
            Prelude.Just ("Destination" Data..= destination)
          ]
      )

instance Data.ToPath StartRouteAnalysis where
  toPath StartRouteAnalysis' {..} =
    Prelude.mconcat
      [ "/global-networks/",
        Data.toBS globalNetworkId,
        "/route-analyses"
      ]

instance Data.ToQuery StartRouteAnalysis where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartRouteAnalysisResponse' smart constructor.
data StartRouteAnalysisResponse = StartRouteAnalysisResponse'
  { -- | The route analysis.
    routeAnalysis :: Prelude.Maybe RouteAnalysis,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartRouteAnalysisResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'routeAnalysis', 'startRouteAnalysisResponse_routeAnalysis' - The route analysis.
--
-- 'httpStatus', 'startRouteAnalysisResponse_httpStatus' - The response's http status code.
newStartRouteAnalysisResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartRouteAnalysisResponse
newStartRouteAnalysisResponse pHttpStatus_ =
  StartRouteAnalysisResponse'
    { routeAnalysis =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The route analysis.
startRouteAnalysisResponse_routeAnalysis :: Lens.Lens' StartRouteAnalysisResponse (Prelude.Maybe RouteAnalysis)
startRouteAnalysisResponse_routeAnalysis = Lens.lens (\StartRouteAnalysisResponse' {routeAnalysis} -> routeAnalysis) (\s@StartRouteAnalysisResponse' {} a -> s {routeAnalysis = a} :: StartRouteAnalysisResponse)

-- | The response's http status code.
startRouteAnalysisResponse_httpStatus :: Lens.Lens' StartRouteAnalysisResponse Prelude.Int
startRouteAnalysisResponse_httpStatus = Lens.lens (\StartRouteAnalysisResponse' {httpStatus} -> httpStatus) (\s@StartRouteAnalysisResponse' {} a -> s {httpStatus = a} :: StartRouteAnalysisResponse)

instance Prelude.NFData StartRouteAnalysisResponse where
  rnf StartRouteAnalysisResponse' {..} =
    Prelude.rnf routeAnalysis
      `Prelude.seq` Prelude.rnf httpStatus
