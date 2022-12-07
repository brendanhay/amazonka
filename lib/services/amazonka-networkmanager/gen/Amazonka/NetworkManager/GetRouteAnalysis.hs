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
-- Module      : Amazonka.NetworkManager.GetRouteAnalysis
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the specified route analysis.
module Amazonka.NetworkManager.GetRouteAnalysis
  ( -- * Creating a Request
    GetRouteAnalysis (..),
    newGetRouteAnalysis,

    -- * Request Lenses
    getRouteAnalysis_globalNetworkId,
    getRouteAnalysis_routeAnalysisId,

    -- * Destructuring the Response
    GetRouteAnalysisResponse (..),
    newGetRouteAnalysisResponse,

    -- * Response Lenses
    getRouteAnalysisResponse_routeAnalysis,
    getRouteAnalysisResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetRouteAnalysis' smart constructor.
data GetRouteAnalysis = GetRouteAnalysis'
  { -- | The ID of the global network.
    globalNetworkId :: Prelude.Text,
    -- | The ID of the route analysis.
    routeAnalysisId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRouteAnalysis' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'globalNetworkId', 'getRouteAnalysis_globalNetworkId' - The ID of the global network.
--
-- 'routeAnalysisId', 'getRouteAnalysis_routeAnalysisId' - The ID of the route analysis.
newGetRouteAnalysis ::
  -- | 'globalNetworkId'
  Prelude.Text ->
  -- | 'routeAnalysisId'
  Prelude.Text ->
  GetRouteAnalysis
newGetRouteAnalysis
  pGlobalNetworkId_
  pRouteAnalysisId_ =
    GetRouteAnalysis'
      { globalNetworkId =
          pGlobalNetworkId_,
        routeAnalysisId = pRouteAnalysisId_
      }

-- | The ID of the global network.
getRouteAnalysis_globalNetworkId :: Lens.Lens' GetRouteAnalysis Prelude.Text
getRouteAnalysis_globalNetworkId = Lens.lens (\GetRouteAnalysis' {globalNetworkId} -> globalNetworkId) (\s@GetRouteAnalysis' {} a -> s {globalNetworkId = a} :: GetRouteAnalysis)

-- | The ID of the route analysis.
getRouteAnalysis_routeAnalysisId :: Lens.Lens' GetRouteAnalysis Prelude.Text
getRouteAnalysis_routeAnalysisId = Lens.lens (\GetRouteAnalysis' {routeAnalysisId} -> routeAnalysisId) (\s@GetRouteAnalysis' {} a -> s {routeAnalysisId = a} :: GetRouteAnalysis)

instance Core.AWSRequest GetRouteAnalysis where
  type
    AWSResponse GetRouteAnalysis =
      GetRouteAnalysisResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRouteAnalysisResponse'
            Prelude.<$> (x Data..?> "RouteAnalysis")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetRouteAnalysis where
  hashWithSalt _salt GetRouteAnalysis' {..} =
    _salt `Prelude.hashWithSalt` globalNetworkId
      `Prelude.hashWithSalt` routeAnalysisId

instance Prelude.NFData GetRouteAnalysis where
  rnf GetRouteAnalysis' {..} =
    Prelude.rnf globalNetworkId
      `Prelude.seq` Prelude.rnf routeAnalysisId

instance Data.ToHeaders GetRouteAnalysis where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetRouteAnalysis where
  toPath GetRouteAnalysis' {..} =
    Prelude.mconcat
      [ "/global-networks/",
        Data.toBS globalNetworkId,
        "/route-analyses/",
        Data.toBS routeAnalysisId
      ]

instance Data.ToQuery GetRouteAnalysis where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetRouteAnalysisResponse' smart constructor.
data GetRouteAnalysisResponse = GetRouteAnalysisResponse'
  { -- | The route analysis.
    routeAnalysis :: Prelude.Maybe RouteAnalysis,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRouteAnalysisResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'routeAnalysis', 'getRouteAnalysisResponse_routeAnalysis' - The route analysis.
--
-- 'httpStatus', 'getRouteAnalysisResponse_httpStatus' - The response's http status code.
newGetRouteAnalysisResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetRouteAnalysisResponse
newGetRouteAnalysisResponse pHttpStatus_ =
  GetRouteAnalysisResponse'
    { routeAnalysis =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The route analysis.
getRouteAnalysisResponse_routeAnalysis :: Lens.Lens' GetRouteAnalysisResponse (Prelude.Maybe RouteAnalysis)
getRouteAnalysisResponse_routeAnalysis = Lens.lens (\GetRouteAnalysisResponse' {routeAnalysis} -> routeAnalysis) (\s@GetRouteAnalysisResponse' {} a -> s {routeAnalysis = a} :: GetRouteAnalysisResponse)

-- | The response's http status code.
getRouteAnalysisResponse_httpStatus :: Lens.Lens' GetRouteAnalysisResponse Prelude.Int
getRouteAnalysisResponse_httpStatus = Lens.lens (\GetRouteAnalysisResponse' {httpStatus} -> httpStatus) (\s@GetRouteAnalysisResponse' {} a -> s {httpStatus = a} :: GetRouteAnalysisResponse)

instance Prelude.NFData GetRouteAnalysisResponse where
  rnf GetRouteAnalysisResponse' {..} =
    Prelude.rnf routeAnalysis
      `Prelude.seq` Prelude.rnf httpStatus
