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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LakeFormation.Types.BatchPermissionsFailureEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LakeFormation.Types.BatchPermissionsRequestEntry
import Amazonka.LakeFormation.Types.ErrorDetail
import qualified Amazonka.Prelude as Prelude

-- | A list of failures when performing a batch grant or batch revoke
-- operation.
--
-- /See:/ 'newBatchPermissionsFailureEntry' smart constructor.
data BatchPermissionsFailureEntry = BatchPermissionsFailureEntry'
  { -- | An identifier for an entry of the batch request.
    requestEntry :: Prelude.Maybe BatchPermissionsRequestEntry,
    -- | An error message that applies to the failure of the entry.
    error :: Prelude.Maybe ErrorDetail
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
-- 'requestEntry', 'batchPermissionsFailureEntry_requestEntry' - An identifier for an entry of the batch request.
--
-- 'error', 'batchPermissionsFailureEntry_error' - An error message that applies to the failure of the entry.
newBatchPermissionsFailureEntry ::
  BatchPermissionsFailureEntry
newBatchPermissionsFailureEntry =
  BatchPermissionsFailureEntry'
    { requestEntry =
        Prelude.Nothing,
      error = Prelude.Nothing
    }

-- | An identifier for an entry of the batch request.
batchPermissionsFailureEntry_requestEntry :: Lens.Lens' BatchPermissionsFailureEntry (Prelude.Maybe BatchPermissionsRequestEntry)
batchPermissionsFailureEntry_requestEntry = Lens.lens (\BatchPermissionsFailureEntry' {requestEntry} -> requestEntry) (\s@BatchPermissionsFailureEntry' {} a -> s {requestEntry = a} :: BatchPermissionsFailureEntry)

-- | An error message that applies to the failure of the entry.
batchPermissionsFailureEntry_error :: Lens.Lens' BatchPermissionsFailureEntry (Prelude.Maybe ErrorDetail)
batchPermissionsFailureEntry_error = Lens.lens (\BatchPermissionsFailureEntry' {error} -> error) (\s@BatchPermissionsFailureEntry' {} a -> s {error = a} :: BatchPermissionsFailureEntry)

instance Core.FromJSON BatchPermissionsFailureEntry where
  parseJSON =
    Core.withObject
      "BatchPermissionsFailureEntry"
      ( \x ->
          BatchPermissionsFailureEntry'
            Prelude.<$> (x Core..:? "RequestEntry")
            Prelude.<*> (x Core..:? "Error")
      )

instance
  Prelude.Hashable
    BatchPermissionsFailureEntry
  where
  hashWithSalt _salt BatchPermissionsFailureEntry' {..} =
    _salt `Prelude.hashWithSalt` requestEntry
      `Prelude.hashWithSalt` error

instance Prelude.NFData BatchPermissionsFailureEntry where
  rnf BatchPermissionsFailureEntry' {..} =
    Prelude.rnf requestEntry
      `Prelude.seq` Prelude.rnf error
