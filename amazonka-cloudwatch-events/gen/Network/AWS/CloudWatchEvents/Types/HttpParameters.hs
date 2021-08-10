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
-- Module      : Network.AWS.CloudWatchEvents.Types.HttpParameters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.HttpParameters where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | These are custom parameter to be used when the target is an API Gateway
-- REST APIs or EventBridge ApiDestinations. In the latter case, these are
-- merged with any InvocationParameters specified on the Connection, with
-- any values from the Connection taking precedence.
--
-- /See:/ 'newHttpParameters' smart constructor.
data HttpParameters = HttpParameters'
  { -- | The query string keys\/values that need to be sent as part of request
    -- invoking the API Gateway REST API or EventBridge ApiDestination.
    queryStringParameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The path parameter values to be used to populate API Gateway REST API or
    -- EventBridge ApiDestination path wildcards (\"*\").
    pathParameterValues :: Prelude.Maybe [Prelude.Text],
    -- | The headers that need to be sent as part of request invoking the API
    -- Gateway REST API or EventBridge ApiDestination.
    headerParameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HttpParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'queryStringParameters', 'httpParameters_queryStringParameters' - The query string keys\/values that need to be sent as part of request
-- invoking the API Gateway REST API or EventBridge ApiDestination.
--
-- 'pathParameterValues', 'httpParameters_pathParameterValues' - The path parameter values to be used to populate API Gateway REST API or
-- EventBridge ApiDestination path wildcards (\"*\").
--
-- 'headerParameters', 'httpParameters_headerParameters' - The headers that need to be sent as part of request invoking the API
-- Gateway REST API or EventBridge ApiDestination.
newHttpParameters ::
  HttpParameters
newHttpParameters =
  HttpParameters'
    { queryStringParameters =
        Prelude.Nothing,
      pathParameterValues = Prelude.Nothing,
      headerParameters = Prelude.Nothing
    }

-- | The query string keys\/values that need to be sent as part of request
-- invoking the API Gateway REST API or EventBridge ApiDestination.
httpParameters_queryStringParameters :: Lens.Lens' HttpParameters (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
httpParameters_queryStringParameters = Lens.lens (\HttpParameters' {queryStringParameters} -> queryStringParameters) (\s@HttpParameters' {} a -> s {queryStringParameters = a} :: HttpParameters) Prelude.. Lens.mapping Lens._Coerce

-- | The path parameter values to be used to populate API Gateway REST API or
-- EventBridge ApiDestination path wildcards (\"*\").
httpParameters_pathParameterValues :: Lens.Lens' HttpParameters (Prelude.Maybe [Prelude.Text])
httpParameters_pathParameterValues = Lens.lens (\HttpParameters' {pathParameterValues} -> pathParameterValues) (\s@HttpParameters' {} a -> s {pathParameterValues = a} :: HttpParameters) Prelude.. Lens.mapping Lens._Coerce

-- | The headers that need to be sent as part of request invoking the API
-- Gateway REST API or EventBridge ApiDestination.
httpParameters_headerParameters :: Lens.Lens' HttpParameters (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
httpParameters_headerParameters = Lens.lens (\HttpParameters' {headerParameters} -> headerParameters) (\s@HttpParameters' {} a -> s {headerParameters = a} :: HttpParameters) Prelude.. Lens.mapping Lens._Coerce

instance Core.FromJSON HttpParameters where
  parseJSON =
    Core.withObject
      "HttpParameters"
      ( \x ->
          HttpParameters'
            Prelude.<$> ( x Core..:? "QueryStringParameters"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> ( x Core..:? "PathParameterValues"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> ( x Core..:? "HeaderParameters"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable HttpParameters

instance Prelude.NFData HttpParameters

instance Core.ToJSON HttpParameters where
  toJSON HttpParameters' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("QueryStringParameters" Core..=)
              Prelude.<$> queryStringParameters,
            ("PathParameterValues" Core..=)
              Prelude.<$> pathParameterValues,
            ("HeaderParameters" Core..=)
              Prelude.<$> headerParameters
          ]
      )
