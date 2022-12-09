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
-- Module      : Amazonka.QuickSight.Types.AnalysisError
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.AnalysisError where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.AnalysisErrorType
import Amazonka.QuickSight.Types.Entity

-- | Analysis error.
--
-- /See:/ 'newAnalysisError' smart constructor.
data AnalysisError = AnalysisError'
  { -- | The message associated with the analysis error.
    message :: Prelude.Maybe Prelude.Text,
    -- | The type of the analysis error.
    type' :: Prelude.Maybe AnalysisErrorType,
    violatedEntities :: Prelude.Maybe [Entity]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AnalysisError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'analysisError_message' - The message associated with the analysis error.
--
-- 'type'', 'analysisError_type' - The type of the analysis error.
--
-- 'violatedEntities', 'analysisError_violatedEntities' -
newAnalysisError ::
  AnalysisError
newAnalysisError =
  AnalysisError'
    { message = Prelude.Nothing,
      type' = Prelude.Nothing,
      violatedEntities = Prelude.Nothing
    }

-- | The message associated with the analysis error.
analysisError_message :: Lens.Lens' AnalysisError (Prelude.Maybe Prelude.Text)
analysisError_message = Lens.lens (\AnalysisError' {message} -> message) (\s@AnalysisError' {} a -> s {message = a} :: AnalysisError)

-- | The type of the analysis error.
analysisError_type :: Lens.Lens' AnalysisError (Prelude.Maybe AnalysisErrorType)
analysisError_type = Lens.lens (\AnalysisError' {type'} -> type') (\s@AnalysisError' {} a -> s {type' = a} :: AnalysisError)

-- |
analysisError_violatedEntities :: Lens.Lens' AnalysisError (Prelude.Maybe [Entity])
analysisError_violatedEntities = Lens.lens (\AnalysisError' {violatedEntities} -> violatedEntities) (\s@AnalysisError' {} a -> s {violatedEntities = a} :: AnalysisError) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON AnalysisError where
  parseJSON =
    Data.withObject
      "AnalysisError"
      ( \x ->
          AnalysisError'
            Prelude.<$> (x Data..:? "Message")
            Prelude.<*> (x Data..:? "Type")
            Prelude.<*> ( x Data..:? "ViolatedEntities"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable AnalysisError where
  hashWithSalt _salt AnalysisError' {..} =
    _salt `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` violatedEntities

instance Prelude.NFData AnalysisError where
  rnf AnalysisError' {..} =
    Prelude.rnf message
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf violatedEntities
