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
-- Module      : Amazonka.FraudDetector.Types.ExternalModelSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FraudDetector.Types.ExternalModelSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FraudDetector.Types.ModelSource
import qualified Amazonka.Prelude as Prelude

-- | The Amazon SageMaker model.
--
-- /See:/ 'newExternalModelSummary' smart constructor.
data ExternalModelSummary = ExternalModelSummary'
  { -- | The endpoint of the Amazon SageMaker model.
    modelEndpoint :: Prelude.Maybe Prelude.Text,
    -- | The source of the model.
    modelSource :: Prelude.Maybe ModelSource
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExternalModelSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'modelEndpoint', 'externalModelSummary_modelEndpoint' - The endpoint of the Amazon SageMaker model.
--
-- 'modelSource', 'externalModelSummary_modelSource' - The source of the model.
newExternalModelSummary ::
  ExternalModelSummary
newExternalModelSummary =
  ExternalModelSummary'
    { modelEndpoint =
        Prelude.Nothing,
      modelSource = Prelude.Nothing
    }

-- | The endpoint of the Amazon SageMaker model.
externalModelSummary_modelEndpoint :: Lens.Lens' ExternalModelSummary (Prelude.Maybe Prelude.Text)
externalModelSummary_modelEndpoint = Lens.lens (\ExternalModelSummary' {modelEndpoint} -> modelEndpoint) (\s@ExternalModelSummary' {} a -> s {modelEndpoint = a} :: ExternalModelSummary)

-- | The source of the model.
externalModelSummary_modelSource :: Lens.Lens' ExternalModelSummary (Prelude.Maybe ModelSource)
externalModelSummary_modelSource = Lens.lens (\ExternalModelSummary' {modelSource} -> modelSource) (\s@ExternalModelSummary' {} a -> s {modelSource = a} :: ExternalModelSummary)

instance Data.FromJSON ExternalModelSummary where
  parseJSON =
    Data.withObject
      "ExternalModelSummary"
      ( \x ->
          ExternalModelSummary'
            Prelude.<$> (x Data..:? "modelEndpoint")
            Prelude.<*> (x Data..:? "modelSource")
      )

instance Prelude.Hashable ExternalModelSummary where
  hashWithSalt _salt ExternalModelSummary' {..} =
    _salt
      `Prelude.hashWithSalt` modelEndpoint
      `Prelude.hashWithSalt` modelSource

instance Prelude.NFData ExternalModelSummary where
  rnf ExternalModelSummary' {..} =
    Prelude.rnf modelEndpoint
      `Prelude.seq` Prelude.rnf modelSource
