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
-- Module      : Amazonka.Pipes.Types.PipeEnrichmentParameters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pipes.Types.PipeEnrichmentParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pipes.Types.PipeEnrichmentHttpParameters
import qualified Amazonka.Prelude as Prelude

-- | The parameters required to set up enrichment on your pipe.
--
-- /See:/ 'newPipeEnrichmentParameters' smart constructor.
data PipeEnrichmentParameters = PipeEnrichmentParameters'
  { -- | Contains the HTTP parameters to use when the target is a API Gateway
    -- REST endpoint or EventBridge ApiDestination.
    --
    -- If you specify an API Gateway REST API or EventBridge ApiDestination as
    -- a target, you can use this parameter to specify headers, path
    -- parameters, and query string keys\/values as part of your target
    -- invoking request. If you\'re using ApiDestinations, the corresponding
    -- Connection can also have these values configured. In case of any
    -- conflicting keys, values from the Connection take precedence.
    httpParameters :: Prelude.Maybe PipeEnrichmentHttpParameters,
    -- | Valid JSON text passed to the enrichment. In this case, nothing from the
    -- event itself is passed to the enrichment. For more information, see
    -- <http://www.rfc-editor.org/rfc/rfc7159.txt The JavaScript Object Notation (JSON) Data Interchange Format>.
    inputTemplate :: Prelude.Maybe (Data.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PipeEnrichmentParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpParameters', 'pipeEnrichmentParameters_httpParameters' - Contains the HTTP parameters to use when the target is a API Gateway
-- REST endpoint or EventBridge ApiDestination.
--
-- If you specify an API Gateway REST API or EventBridge ApiDestination as
-- a target, you can use this parameter to specify headers, path
-- parameters, and query string keys\/values as part of your target
-- invoking request. If you\'re using ApiDestinations, the corresponding
-- Connection can also have these values configured. In case of any
-- conflicting keys, values from the Connection take precedence.
--
-- 'inputTemplate', 'pipeEnrichmentParameters_inputTemplate' - Valid JSON text passed to the enrichment. In this case, nothing from the
-- event itself is passed to the enrichment. For more information, see
-- <http://www.rfc-editor.org/rfc/rfc7159.txt The JavaScript Object Notation (JSON) Data Interchange Format>.
newPipeEnrichmentParameters ::
  PipeEnrichmentParameters
newPipeEnrichmentParameters =
  PipeEnrichmentParameters'
    { httpParameters =
        Prelude.Nothing,
      inputTemplate = Prelude.Nothing
    }

-- | Contains the HTTP parameters to use when the target is a API Gateway
-- REST endpoint or EventBridge ApiDestination.
--
-- If you specify an API Gateway REST API or EventBridge ApiDestination as
-- a target, you can use this parameter to specify headers, path
-- parameters, and query string keys\/values as part of your target
-- invoking request. If you\'re using ApiDestinations, the corresponding
-- Connection can also have these values configured. In case of any
-- conflicting keys, values from the Connection take precedence.
pipeEnrichmentParameters_httpParameters :: Lens.Lens' PipeEnrichmentParameters (Prelude.Maybe PipeEnrichmentHttpParameters)
pipeEnrichmentParameters_httpParameters = Lens.lens (\PipeEnrichmentParameters' {httpParameters} -> httpParameters) (\s@PipeEnrichmentParameters' {} a -> s {httpParameters = a} :: PipeEnrichmentParameters)

-- | Valid JSON text passed to the enrichment. In this case, nothing from the
-- event itself is passed to the enrichment. For more information, see
-- <http://www.rfc-editor.org/rfc/rfc7159.txt The JavaScript Object Notation (JSON) Data Interchange Format>.
pipeEnrichmentParameters_inputTemplate :: Lens.Lens' PipeEnrichmentParameters (Prelude.Maybe Prelude.Text)
pipeEnrichmentParameters_inputTemplate = Lens.lens (\PipeEnrichmentParameters' {inputTemplate} -> inputTemplate) (\s@PipeEnrichmentParameters' {} a -> s {inputTemplate = a} :: PipeEnrichmentParameters) Prelude.. Lens.mapping Data._Sensitive

instance Data.FromJSON PipeEnrichmentParameters where
  parseJSON =
    Data.withObject
      "PipeEnrichmentParameters"
      ( \x ->
          PipeEnrichmentParameters'
            Prelude.<$> (x Data..:? "HttpParameters")
            Prelude.<*> (x Data..:? "InputTemplate")
      )

instance Prelude.Hashable PipeEnrichmentParameters where
  hashWithSalt _salt PipeEnrichmentParameters' {..} =
    _salt
      `Prelude.hashWithSalt` httpParameters
      `Prelude.hashWithSalt` inputTemplate

instance Prelude.NFData PipeEnrichmentParameters where
  rnf PipeEnrichmentParameters' {..} =
    Prelude.rnf httpParameters
      `Prelude.seq` Prelude.rnf inputTemplate

instance Data.ToJSON PipeEnrichmentParameters where
  toJSON PipeEnrichmentParameters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("HttpParameters" Data..=)
              Prelude.<$> httpParameters,
            ("InputTemplate" Data..=) Prelude.<$> inputTemplate
          ]
      )
