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
-- Module      : Amazonka.SageMakerGeoSpatial.Types.VectorEnrichmentJobExportErrorDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerGeoSpatial.Types.VectorEnrichmentJobExportErrorDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMakerGeoSpatial.Types.VectorEnrichmentJobExportErrorType

-- | VectorEnrichmentJob export error details in response from
-- GetVectorEnrichmentJob.
--
-- /See:/ 'newVectorEnrichmentJobExportErrorDetails' smart constructor.
data VectorEnrichmentJobExportErrorDetails = VectorEnrichmentJobExportErrorDetails'
  { -- | The message providing details about the errors generated during the
    -- Vector Enrichment job.
    message :: Prelude.Maybe Prelude.Text,
    type' :: Prelude.Maybe VectorEnrichmentJobExportErrorType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VectorEnrichmentJobExportErrorDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'vectorEnrichmentJobExportErrorDetails_message' - The message providing details about the errors generated during the
-- Vector Enrichment job.
--
-- 'type'', 'vectorEnrichmentJobExportErrorDetails_type' -
newVectorEnrichmentJobExportErrorDetails ::
  VectorEnrichmentJobExportErrorDetails
newVectorEnrichmentJobExportErrorDetails =
  VectorEnrichmentJobExportErrorDetails'
    { message =
        Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The message providing details about the errors generated during the
-- Vector Enrichment job.
vectorEnrichmentJobExportErrorDetails_message :: Lens.Lens' VectorEnrichmentJobExportErrorDetails (Prelude.Maybe Prelude.Text)
vectorEnrichmentJobExportErrorDetails_message = Lens.lens (\VectorEnrichmentJobExportErrorDetails' {message} -> message) (\s@VectorEnrichmentJobExportErrorDetails' {} a -> s {message = a} :: VectorEnrichmentJobExportErrorDetails)

-- |
vectorEnrichmentJobExportErrorDetails_type :: Lens.Lens' VectorEnrichmentJobExportErrorDetails (Prelude.Maybe VectorEnrichmentJobExportErrorType)
vectorEnrichmentJobExportErrorDetails_type = Lens.lens (\VectorEnrichmentJobExportErrorDetails' {type'} -> type') (\s@VectorEnrichmentJobExportErrorDetails' {} a -> s {type' = a} :: VectorEnrichmentJobExportErrorDetails)

instance
  Data.FromJSON
    VectorEnrichmentJobExportErrorDetails
  where
  parseJSON =
    Data.withObject
      "VectorEnrichmentJobExportErrorDetails"
      ( \x ->
          VectorEnrichmentJobExportErrorDetails'
            Prelude.<$> (x Data..:? "Message")
            Prelude.<*> (x Data..:? "Type")
      )

instance
  Prelude.Hashable
    VectorEnrichmentJobExportErrorDetails
  where
  hashWithSalt
    _salt
    VectorEnrichmentJobExportErrorDetails' {..} =
      _salt `Prelude.hashWithSalt` message
        `Prelude.hashWithSalt` type'

instance
  Prelude.NFData
    VectorEnrichmentJobExportErrorDetails
  where
  rnf VectorEnrichmentJobExportErrorDetails' {..} =
    Prelude.rnf message `Prelude.seq` Prelude.rnf type'
