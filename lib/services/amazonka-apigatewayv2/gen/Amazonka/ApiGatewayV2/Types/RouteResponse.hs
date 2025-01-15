{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ApiGatewayV2.Types.RouteResponse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ApiGatewayV2.Types.RouteResponse where

import Amazonka.ApiGatewayV2.Types.ParameterConstraints
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents a route response.
--
-- /See:/ 'newRouteResponse' smart constructor.
data RouteResponse = RouteResponse'
  { -- | Represents the model selection expression of a route response. Supported
    -- only for WebSocket APIs.
    modelSelectionExpression :: Prelude.Maybe Prelude.Text,
    -- | Represents the response models of a route response.
    responseModels :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Represents the response parameters of a route response.
    responseParameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text ParameterConstraints),
    -- | Represents the identifier of a route response.
    routeResponseId :: Prelude.Maybe Prelude.Text,
    -- | Represents the route response key of a route response.
    routeResponseKey :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RouteResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'modelSelectionExpression', 'routeResponse_modelSelectionExpression' - Represents the model selection expression of a route response. Supported
-- only for WebSocket APIs.
--
-- 'responseModels', 'routeResponse_responseModels' - Represents the response models of a route response.
--
-- 'responseParameters', 'routeResponse_responseParameters' - Represents the response parameters of a route response.
--
-- 'routeResponseId', 'routeResponse_routeResponseId' - Represents the identifier of a route response.
--
-- 'routeResponseKey', 'routeResponse_routeResponseKey' - Represents the route response key of a route response.
newRouteResponse ::
  -- | 'routeResponseKey'
  Prelude.Text ->
  RouteResponse
newRouteResponse pRouteResponseKey_ =
  RouteResponse'
    { modelSelectionExpression =
        Prelude.Nothing,
      responseModels = Prelude.Nothing,
      responseParameters = Prelude.Nothing,
      routeResponseId = Prelude.Nothing,
      routeResponseKey = pRouteResponseKey_
    }

-- | Represents the model selection expression of a route response. Supported
-- only for WebSocket APIs.
routeResponse_modelSelectionExpression :: Lens.Lens' RouteResponse (Prelude.Maybe Prelude.Text)
routeResponse_modelSelectionExpression = Lens.lens (\RouteResponse' {modelSelectionExpression} -> modelSelectionExpression) (\s@RouteResponse' {} a -> s {modelSelectionExpression = a} :: RouteResponse)

-- | Represents the response models of a route response.
routeResponse_responseModels :: Lens.Lens' RouteResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
routeResponse_responseModels = Lens.lens (\RouteResponse' {responseModels} -> responseModels) (\s@RouteResponse' {} a -> s {responseModels = a} :: RouteResponse) Prelude.. Lens.mapping Lens.coerced

-- | Represents the response parameters of a route response.
routeResponse_responseParameters :: Lens.Lens' RouteResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text ParameterConstraints))
routeResponse_responseParameters = Lens.lens (\RouteResponse' {responseParameters} -> responseParameters) (\s@RouteResponse' {} a -> s {responseParameters = a} :: RouteResponse) Prelude.. Lens.mapping Lens.coerced

-- | Represents the identifier of a route response.
routeResponse_routeResponseId :: Lens.Lens' RouteResponse (Prelude.Maybe Prelude.Text)
routeResponse_routeResponseId = Lens.lens (\RouteResponse' {routeResponseId} -> routeResponseId) (\s@RouteResponse' {} a -> s {routeResponseId = a} :: RouteResponse)

-- | Represents the route response key of a route response.
routeResponse_routeResponseKey :: Lens.Lens' RouteResponse Prelude.Text
routeResponse_routeResponseKey = Lens.lens (\RouteResponse' {routeResponseKey} -> routeResponseKey) (\s@RouteResponse' {} a -> s {routeResponseKey = a} :: RouteResponse)

instance Data.FromJSON RouteResponse where
  parseJSON =
    Data.withObject
      "RouteResponse"
      ( \x ->
          RouteResponse'
            Prelude.<$> (x Data..:? "modelSelectionExpression")
            Prelude.<*> (x Data..:? "responseModels" Data..!= Prelude.mempty)
            Prelude.<*> ( x
                            Data..:? "responseParameters"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "routeResponseId")
            Prelude.<*> (x Data..: "routeResponseKey")
      )

instance Prelude.Hashable RouteResponse where
  hashWithSalt _salt RouteResponse' {..} =
    _salt
      `Prelude.hashWithSalt` modelSelectionExpression
      `Prelude.hashWithSalt` responseModels
      `Prelude.hashWithSalt` responseParameters
      `Prelude.hashWithSalt` routeResponseId
      `Prelude.hashWithSalt` routeResponseKey

instance Prelude.NFData RouteResponse where
  rnf RouteResponse' {..} =
    Prelude.rnf modelSelectionExpression `Prelude.seq`
      Prelude.rnf responseModels `Prelude.seq`
        Prelude.rnf responseParameters `Prelude.seq`
          Prelude.rnf routeResponseId `Prelude.seq`
            Prelude.rnf routeResponseKey
