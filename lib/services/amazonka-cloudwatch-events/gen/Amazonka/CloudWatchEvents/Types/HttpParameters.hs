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
-- Module      : Amazonka.CloudWatchEvents.Types.HttpParameters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatchEvents.Types.HttpParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | These are custom parameter to be used when the target is an API Gateway
-- REST APIs or EventBridge ApiDestinations. In the latter case, these are
-- merged with any InvocationParameters specified on the Connection, with
-- any values from the Connection taking precedence.
--
-- /See:/ 'newHttpParameters' smart constructor.
data HttpParameters = HttpParameters'
  { -- | The headers that need to be sent as part of request invoking the API
    -- Gateway REST API or EventBridge ApiDestination.
    headerParameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The path parameter values to be used to populate API Gateway REST API or
    -- EventBridge ApiDestination path wildcards (\"*\").
    pathParameterValues :: Prelude.Maybe [Prelude.Text],
    -- | The query string keys\/values that need to be sent as part of request
    -- invoking the API Gateway REST API or EventBridge ApiDestination.
    queryStringParameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
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
-- 'headerParameters', 'httpParameters_headerParameters' - The headers that need to be sent as part of request invoking the API
-- Gateway REST API or EventBridge ApiDestination.
--
-- 'pathParameterValues', 'httpParameters_pathParameterValues' - The path parameter values to be used to populate API Gateway REST API or
-- EventBridge ApiDestination path wildcards (\"*\").
--
-- 'queryStringParameters', 'httpParameters_queryStringParameters' - The query string keys\/values that need to be sent as part of request
-- invoking the API Gateway REST API or EventBridge ApiDestination.
newHttpParameters ::
  HttpParameters
newHttpParameters =
  HttpParameters'
    { headerParameters = Prelude.Nothing,
      pathParameterValues = Prelude.Nothing,
      queryStringParameters = Prelude.Nothing
    }

-- | The headers that need to be sent as part of request invoking the API
-- Gateway REST API or EventBridge ApiDestination.
httpParameters_headerParameters :: Lens.Lens' HttpParameters (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
httpParameters_headerParameters = Lens.lens (\HttpParameters' {headerParameters} -> headerParameters) (\s@HttpParameters' {} a -> s {headerParameters = a} :: HttpParameters) Prelude.. Lens.mapping Lens.coerced

-- | The path parameter values to be used to populate API Gateway REST API or
-- EventBridge ApiDestination path wildcards (\"*\").
httpParameters_pathParameterValues :: Lens.Lens' HttpParameters (Prelude.Maybe [Prelude.Text])
httpParameters_pathParameterValues = Lens.lens (\HttpParameters' {pathParameterValues} -> pathParameterValues) (\s@HttpParameters' {} a -> s {pathParameterValues = a} :: HttpParameters) Prelude.. Lens.mapping Lens.coerced

-- | The query string keys\/values that need to be sent as part of request
-- invoking the API Gateway REST API or EventBridge ApiDestination.
httpParameters_queryStringParameters :: Lens.Lens' HttpParameters (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
httpParameters_queryStringParameters = Lens.lens (\HttpParameters' {queryStringParameters} -> queryStringParameters) (\s@HttpParameters' {} a -> s {queryStringParameters = a} :: HttpParameters) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON HttpParameters where
  parseJSON =
    Data.withObject
      "HttpParameters"
      ( \x ->
          HttpParameters'
            Prelude.<$> ( x
                            Data..:? "HeaderParameters"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..:? "PathParameterValues"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..:? "QueryStringParameters"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable HttpParameters where
  hashWithSalt _salt HttpParameters' {..} =
    _salt
      `Prelude.hashWithSalt` headerParameters
      `Prelude.hashWithSalt` pathParameterValues
      `Prelude.hashWithSalt` queryStringParameters

instance Prelude.NFData HttpParameters where
  rnf HttpParameters' {..} =
    Prelude.rnf headerParameters
      `Prelude.seq` Prelude.rnf pathParameterValues
      `Prelude.seq` Prelude.rnf queryStringParameters

instance Data.ToJSON HttpParameters where
  toJSON HttpParameters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("HeaderParameters" Data..=)
              Prelude.<$> headerParameters,
            ("PathParameterValues" Data..=)
              Prelude.<$> pathParameterValues,
            ("QueryStringParameters" Data..=)
              Prelude.<$> queryStringParameters
          ]
      )
