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
-- Module      : Amazonka.Pipes.Types.PipeEnrichmentHttpParameters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pipes.Types.PipeEnrichmentHttpParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | These are custom parameter to be used when the target is an API Gateway
-- REST APIs or EventBridge ApiDestinations. In the latter case, these are
-- merged with any InvocationParameters specified on the Connection, with
-- any values from the Connection taking precedence.
--
-- /See:/ 'newPipeEnrichmentHttpParameters' smart constructor.
data PipeEnrichmentHttpParameters = PipeEnrichmentHttpParameters'
  { -- | The headers that need to be sent as part of request invoking the API
    -- Gateway REST API or EventBridge ApiDestination.
    headerParameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text (Data.Sensitive Prelude.Text)),
    -- | The path parameter values to be used to populate API Gateway REST API or
    -- EventBridge ApiDestination path wildcards (\"*\").
    pathParameterValues :: Prelude.Maybe [Data.Sensitive Prelude.Text],
    -- | The query string keys\/values that need to be sent as part of request
    -- invoking the API Gateway REST API or EventBridge ApiDestination.
    queryStringParameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text (Data.Sensitive Prelude.Text))
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PipeEnrichmentHttpParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'headerParameters', 'pipeEnrichmentHttpParameters_headerParameters' - The headers that need to be sent as part of request invoking the API
-- Gateway REST API or EventBridge ApiDestination.
--
-- 'pathParameterValues', 'pipeEnrichmentHttpParameters_pathParameterValues' - The path parameter values to be used to populate API Gateway REST API or
-- EventBridge ApiDestination path wildcards (\"*\").
--
-- 'queryStringParameters', 'pipeEnrichmentHttpParameters_queryStringParameters' - The query string keys\/values that need to be sent as part of request
-- invoking the API Gateway REST API or EventBridge ApiDestination.
newPipeEnrichmentHttpParameters ::
  PipeEnrichmentHttpParameters
newPipeEnrichmentHttpParameters =
  PipeEnrichmentHttpParameters'
    { headerParameters =
        Prelude.Nothing,
      pathParameterValues = Prelude.Nothing,
      queryStringParameters = Prelude.Nothing
    }

-- | The headers that need to be sent as part of request invoking the API
-- Gateway REST API or EventBridge ApiDestination.
pipeEnrichmentHttpParameters_headerParameters :: Lens.Lens' PipeEnrichmentHttpParameters (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
pipeEnrichmentHttpParameters_headerParameters = Lens.lens (\PipeEnrichmentHttpParameters' {headerParameters} -> headerParameters) (\s@PipeEnrichmentHttpParameters' {} a -> s {headerParameters = a} :: PipeEnrichmentHttpParameters) Prelude.. Lens.mapping Lens.coerced

-- | The path parameter values to be used to populate API Gateway REST API or
-- EventBridge ApiDestination path wildcards (\"*\").
pipeEnrichmentHttpParameters_pathParameterValues :: Lens.Lens' PipeEnrichmentHttpParameters (Prelude.Maybe [Prelude.Text])
pipeEnrichmentHttpParameters_pathParameterValues = Lens.lens (\PipeEnrichmentHttpParameters' {pathParameterValues} -> pathParameterValues) (\s@PipeEnrichmentHttpParameters' {} a -> s {pathParameterValues = a} :: PipeEnrichmentHttpParameters) Prelude.. Lens.mapping Lens.coerced

-- | The query string keys\/values that need to be sent as part of request
-- invoking the API Gateway REST API or EventBridge ApiDestination.
pipeEnrichmentHttpParameters_queryStringParameters :: Lens.Lens' PipeEnrichmentHttpParameters (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
pipeEnrichmentHttpParameters_queryStringParameters = Lens.lens (\PipeEnrichmentHttpParameters' {queryStringParameters} -> queryStringParameters) (\s@PipeEnrichmentHttpParameters' {} a -> s {queryStringParameters = a} :: PipeEnrichmentHttpParameters) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON PipeEnrichmentHttpParameters where
  parseJSON =
    Data.withObject
      "PipeEnrichmentHttpParameters"
      ( \x ->
          PipeEnrichmentHttpParameters'
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

instance
  Prelude.Hashable
    PipeEnrichmentHttpParameters
  where
  hashWithSalt _salt PipeEnrichmentHttpParameters' {..} =
    _salt
      `Prelude.hashWithSalt` headerParameters
      `Prelude.hashWithSalt` pathParameterValues
      `Prelude.hashWithSalt` queryStringParameters

instance Prelude.NFData PipeEnrichmentHttpParameters where
  rnf PipeEnrichmentHttpParameters' {..} =
    Prelude.rnf headerParameters `Prelude.seq`
      Prelude.rnf pathParameterValues `Prelude.seq`
        Prelude.rnf queryStringParameters

instance Data.ToJSON PipeEnrichmentHttpParameters where
  toJSON PipeEnrichmentHttpParameters' {..} =
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
