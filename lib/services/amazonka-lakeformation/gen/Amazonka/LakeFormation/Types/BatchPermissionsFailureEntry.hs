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
-- Module      : Amazonka.LakeFormation.Types.BatchPermissionsFailureEntry
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LakeFormation.Types.BatchPermissionsFailureEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LakeFormation.Types.BatchPermissionsRequestEntry
import Amazonka.LakeFormation.Types.ErrorDetail
import qualified Amazonka.Prelude as Prelude

-- | A list of failures when performing a batch grant or batch revoke
-- operation.
--
-- /See:/ 'newBatchPermissionsFailureEntry' smart constructor.
data BatchPermissionsFailureEntry = BatchPermissionsFailureEntry'
  { -- | An error message that applies to the failure of the entry.
    error :: Prelude.Maybe ErrorDetail,
    -- | An identifier for an entry of the batch request.
    requestEntry :: Prelude.Maybe BatchPermissionsRequestEntry
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchPermissionsFailureEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'error', 'batchPermissionsFailureEntry_error' - An error message that applies to the failure of the entry.
--
-- 'requestEntry', 'batchPermissionsFailureEntry_requestEntry' - An identifier for an entry of the batch request.
newBatchPermissionsFailureEntry ::
  BatchPermissionsFailureEntry
newBatchPermissionsFailureEntry =
  BatchPermissionsFailureEntry'
    { error =
        Prelude.Nothing,
      requestEntry = Prelude.Nothing
    }

-- | An error message that applies to the failure of the entry.
batchPermissionsFailureEntry_error :: Lens.Lens' BatchPermissionsFailureEntry (Prelude.Maybe ErrorDetail)
batchPermissionsFailureEntry_error = Lens.lens (\BatchPermissionsFailureEntry' {error} -> error) (\s@BatchPermissionsFailureEntry' {} a -> s {error = a} :: BatchPermissionsFailureEntry)

-- | An identifier for an entry of the batch request.
batchPermissionsFailureEntry_requestEntry :: Lens.Lens' BatchPermissionsFailureEntry (Prelude.Maybe BatchPermissionsRequestEntry)
batchPermissionsFailureEntry_requestEntry = Lens.lens (\BatchPermissionsFailureEntry' {requestEntry} -> requestEntry) (\s@BatchPermissionsFailureEntry' {} a -> s {requestEntry = a} :: BatchPermissionsFailureEntry)

instance Data.FromJSON BatchPermissionsFailureEntry where
  parseJSON =
    Data.withObject
      "BatchPermissionsFailureEntry"
      ( \x ->
          BatchPermissionsFailureEntry'
            Prelude.<$> (x Data..:? "Error")
            Prelude.<*> (x Data..:? "RequestEntry")
      )

instance
  Prelude.Hashable
    BatchPermissionsFailureEntry
  where
  hashWithSalt _salt BatchPermissionsFailureEntry' {..} =
    _salt
      `Prelude.hashWithSalt` error
      `Prelude.hashWithSalt` requestEntry

instance Prelude.NFData BatchPermissionsFailureEntry where
  rnf BatchPermissionsFailureEntry' {..} =
    Prelude.rnf error
      `Prelude.seq` Prelude.rnf requestEntry
