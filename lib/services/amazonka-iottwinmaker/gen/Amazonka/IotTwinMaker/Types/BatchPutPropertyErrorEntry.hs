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
-- Module      : Amazonka.IotTwinMaker.Types.BatchPutPropertyErrorEntry
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IotTwinMaker.Types.BatchPutPropertyErrorEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IotTwinMaker.Types.BatchPutPropertyError
import qualified Amazonka.Prelude as Prelude

-- | An object that contains information about errors returned by the
-- @BatchPutProperty@ action.
--
-- /See:/ 'newBatchPutPropertyErrorEntry' smart constructor.
data BatchPutPropertyErrorEntry = BatchPutPropertyErrorEntry'
  { -- | A list of objects that contain information about errors returned by the
    -- @BatchPutProperty@ action.
    errors :: Prelude.NonEmpty BatchPutPropertyError
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchPutPropertyErrorEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errors', 'batchPutPropertyErrorEntry_errors' - A list of objects that contain information about errors returned by the
-- @BatchPutProperty@ action.
newBatchPutPropertyErrorEntry ::
  -- | 'errors'
  Prelude.NonEmpty BatchPutPropertyError ->
  BatchPutPropertyErrorEntry
newBatchPutPropertyErrorEntry pErrors_ =
  BatchPutPropertyErrorEntry'
    { errors =
        Lens.coerced Lens.# pErrors_
    }

-- | A list of objects that contain information about errors returned by the
-- @BatchPutProperty@ action.
batchPutPropertyErrorEntry_errors :: Lens.Lens' BatchPutPropertyErrorEntry (Prelude.NonEmpty BatchPutPropertyError)
batchPutPropertyErrorEntry_errors = Lens.lens (\BatchPutPropertyErrorEntry' {errors} -> errors) (\s@BatchPutPropertyErrorEntry' {} a -> s {errors = a} :: BatchPutPropertyErrorEntry) Prelude.. Lens.coerced

instance Data.FromJSON BatchPutPropertyErrorEntry where
  parseJSON =
    Data.withObject
      "BatchPutPropertyErrorEntry"
      ( \x ->
          BatchPutPropertyErrorEntry'
            Prelude.<$> (x Data..: "errors")
      )

instance Prelude.Hashable BatchPutPropertyErrorEntry where
  hashWithSalt _salt BatchPutPropertyErrorEntry' {..} =
    _salt `Prelude.hashWithSalt` errors

instance Prelude.NFData BatchPutPropertyErrorEntry where
  rnf BatchPutPropertyErrorEntry' {..} =
    Prelude.rnf errors
