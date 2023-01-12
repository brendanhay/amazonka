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
-- Module      : Amazonka.SageMakerGeoSpatial.Types.VectorEnrichmentJobErrorDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerGeoSpatial.Types.VectorEnrichmentJobErrorDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMakerGeoSpatial.Types.VectorEnrichmentJobErrorType

-- | VectorEnrichmentJob error details in response from
-- GetVectorEnrichmentJob.
--
-- /See:/ 'newVectorEnrichmentJobErrorDetails' smart constructor.
data VectorEnrichmentJobErrorDetails = VectorEnrichmentJobErrorDetails'
  { -- | A message that you define and then is processed and rendered by the
    -- Vector Enrichment job when the error occurs.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The type of error generated during the Vector Enrichment job.
    errorType :: Prelude.Maybe VectorEnrichmentJobErrorType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VectorEnrichmentJobErrorDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorMessage', 'vectorEnrichmentJobErrorDetails_errorMessage' - A message that you define and then is processed and rendered by the
-- Vector Enrichment job when the error occurs.
--
-- 'errorType', 'vectorEnrichmentJobErrorDetails_errorType' - The type of error generated during the Vector Enrichment job.
newVectorEnrichmentJobErrorDetails ::
  VectorEnrichmentJobErrorDetails
newVectorEnrichmentJobErrorDetails =
  VectorEnrichmentJobErrorDetails'
    { errorMessage =
        Prelude.Nothing,
      errorType = Prelude.Nothing
    }

-- | A message that you define and then is processed and rendered by the
-- Vector Enrichment job when the error occurs.
vectorEnrichmentJobErrorDetails_errorMessage :: Lens.Lens' VectorEnrichmentJobErrorDetails (Prelude.Maybe Prelude.Text)
vectorEnrichmentJobErrorDetails_errorMessage = Lens.lens (\VectorEnrichmentJobErrorDetails' {errorMessage} -> errorMessage) (\s@VectorEnrichmentJobErrorDetails' {} a -> s {errorMessage = a} :: VectorEnrichmentJobErrorDetails)

-- | The type of error generated during the Vector Enrichment job.
vectorEnrichmentJobErrorDetails_errorType :: Lens.Lens' VectorEnrichmentJobErrorDetails (Prelude.Maybe VectorEnrichmentJobErrorType)
vectorEnrichmentJobErrorDetails_errorType = Lens.lens (\VectorEnrichmentJobErrorDetails' {errorType} -> errorType) (\s@VectorEnrichmentJobErrorDetails' {} a -> s {errorType = a} :: VectorEnrichmentJobErrorDetails)

instance
  Data.FromJSON
    VectorEnrichmentJobErrorDetails
  where
  parseJSON =
    Data.withObject
      "VectorEnrichmentJobErrorDetails"
      ( \x ->
          VectorEnrichmentJobErrorDetails'
            Prelude.<$> (x Data..:? "ErrorMessage")
            Prelude.<*> (x Data..:? "ErrorType")
      )

instance
  Prelude.Hashable
    VectorEnrichmentJobErrorDetails
  where
  hashWithSalt
    _salt
    VectorEnrichmentJobErrorDetails' {..} =
      _salt `Prelude.hashWithSalt` errorMessage
        `Prelude.hashWithSalt` errorType

instance
  Prelude.NFData
    VectorEnrichmentJobErrorDetails
  where
  rnf VectorEnrichmentJobErrorDetails' {..} =
    Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf errorType
