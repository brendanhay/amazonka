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
-- Module      : Amazonka.StepFunctions.Types.MapRunFailedEventDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.StepFunctions.Types.MapRunFailedEventDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains details about a Map Run failure event that occurred during a
-- state machine execution.
--
-- /See:/ 'newMapRunFailedEventDetails' smart constructor.
data MapRunFailedEventDetails = MapRunFailedEventDetails'
  { -- | A more detailed explanation of the cause of the failure.
    cause :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The error code of the Map Run failure.
    error :: Prelude.Maybe (Data.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MapRunFailedEventDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cause', 'mapRunFailedEventDetails_cause' - A more detailed explanation of the cause of the failure.
--
-- 'error', 'mapRunFailedEventDetails_error' - The error code of the Map Run failure.
newMapRunFailedEventDetails ::
  MapRunFailedEventDetails
newMapRunFailedEventDetails =
  MapRunFailedEventDetails'
    { cause = Prelude.Nothing,
      error = Prelude.Nothing
    }

-- | A more detailed explanation of the cause of the failure.
mapRunFailedEventDetails_cause :: Lens.Lens' MapRunFailedEventDetails (Prelude.Maybe Prelude.Text)
mapRunFailedEventDetails_cause = Lens.lens (\MapRunFailedEventDetails' {cause} -> cause) (\s@MapRunFailedEventDetails' {} a -> s {cause = a} :: MapRunFailedEventDetails) Prelude.. Lens.mapping Data._Sensitive

-- | The error code of the Map Run failure.
mapRunFailedEventDetails_error :: Lens.Lens' MapRunFailedEventDetails (Prelude.Maybe Prelude.Text)
mapRunFailedEventDetails_error = Lens.lens (\MapRunFailedEventDetails' {error} -> error) (\s@MapRunFailedEventDetails' {} a -> s {error = a} :: MapRunFailedEventDetails) Prelude.. Lens.mapping Data._Sensitive

instance Data.FromJSON MapRunFailedEventDetails where
  parseJSON =
    Data.withObject
      "MapRunFailedEventDetails"
      ( \x ->
          MapRunFailedEventDetails'
            Prelude.<$> (x Data..:? "cause")
            Prelude.<*> (x Data..:? "error")
      )

instance Prelude.Hashable MapRunFailedEventDetails where
  hashWithSalt _salt MapRunFailedEventDetails' {..} =
    _salt
      `Prelude.hashWithSalt` cause
      `Prelude.hashWithSalt` error

instance Prelude.NFData MapRunFailedEventDetails where
  rnf MapRunFailedEventDetails' {..} =
    Prelude.rnf cause `Prelude.seq` Prelude.rnf error
