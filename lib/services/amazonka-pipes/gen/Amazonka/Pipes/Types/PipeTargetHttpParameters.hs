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
-- Module      : Amazonka.Pipes.Types.PipeTargetHttpParameters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pipes.Types.PipeTargetHttpParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | These are custom parameter to be used when the target is an API Gateway
-- REST APIs or EventBridge ApiDestinations.
--
-- /See:/ 'newPipeTargetHttpParameters' smart constructor.
data PipeTargetHttpParameters = PipeTargetHttpParameters'
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
-- Create a value of 'PipeTargetHttpParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'headerParameters', 'pipeTargetHttpParameters_headerParameters' - The headers that need to be sent as part of request invoking the API
-- Gateway REST API or EventBridge ApiDestination.
--
-- 'pathParameterValues', 'pipeTargetHttpParameters_pathParameterValues' - The path parameter values to be used to populate API Gateway REST API or
-- EventBridge ApiDestination path wildcards (\"*\").
--
-- 'queryStringParameters', 'pipeTargetHttpParameters_queryStringParameters' - The query string keys\/values that need to be sent as part of request
-- invoking the API Gateway REST API or EventBridge ApiDestination.
newPipeTargetHttpParameters ::
  PipeTargetHttpParameters
newPipeTargetHttpParameters =
  PipeTargetHttpParameters'
    { headerParameters =
        Prelude.Nothing,
      pathParameterValues = Prelude.Nothing,
      queryStringParameters = Prelude.Nothing
    }

-- | The headers that need to be sent as part of request invoking the API
-- Gateway REST API or EventBridge ApiDestination.
pipeTargetHttpParameters_headerParameters :: Lens.Lens' PipeTargetHttpParameters (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
pipeTargetHttpParameters_headerParameters = Lens.lens (\PipeTargetHttpParameters' {headerParameters} -> headerParameters) (\s@PipeTargetHttpParameters' {} a -> s {headerParameters = a} :: PipeTargetHttpParameters) Prelude.. Lens.mapping Lens.coerced

-- | The path parameter values to be used to populate API Gateway REST API or
-- EventBridge ApiDestination path wildcards (\"*\").
pipeTargetHttpParameters_pathParameterValues :: Lens.Lens' PipeTargetHttpParameters (Prelude.Maybe [Prelude.Text])
pipeTargetHttpParameters_pathParameterValues = Lens.lens (\PipeTargetHttpParameters' {pathParameterValues} -> pathParameterValues) (\s@PipeTargetHttpParameters' {} a -> s {pathParameterValues = a} :: PipeTargetHttpParameters) Prelude.. Lens.mapping Lens.coerced

-- | The query string keys\/values that need to be sent as part of request
-- invoking the API Gateway REST API or EventBridge ApiDestination.
pipeTargetHttpParameters_queryStringParameters :: Lens.Lens' PipeTargetHttpParameters (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
pipeTargetHttpParameters_queryStringParameters = Lens.lens (\PipeTargetHttpParameters' {queryStringParameters} -> queryStringParameters) (\s@PipeTargetHttpParameters' {} a -> s {queryStringParameters = a} :: PipeTargetHttpParameters) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON PipeTargetHttpParameters where
  parseJSON =
    Data.withObject
      "PipeTargetHttpParameters"
      ( \x ->
          PipeTargetHttpParameters'
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

instance Prelude.Hashable PipeTargetHttpParameters where
  hashWithSalt _salt PipeTargetHttpParameters' {..} =
    _salt
      `Prelude.hashWithSalt` headerParameters
      `Prelude.hashWithSalt` pathParameterValues
      `Prelude.hashWithSalt` queryStringParameters

instance Prelude.NFData PipeTargetHttpParameters where
  rnf PipeTargetHttpParameters' {..} =
    Prelude.rnf headerParameters
      `Prelude.seq` Prelude.rnf pathParameterValues
      `Prelude.seq` Prelude.rnf queryStringParameters

instance Data.ToJSON PipeTargetHttpParameters where
  toJSON PipeTargetHttpParameters' {..} =
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
