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
-- Module      : Amazonka.CloudDirectory.Types.BatchDetachFromIndex
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudDirectory.Types.BatchDetachFromIndex where

import Amazonka.CloudDirectory.Types.ObjectReference
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Detaches the specified object from the specified index inside a
-- BatchRead operation. For more information, see DetachFromIndex and
-- BatchReadRequest$Operations.
--
-- /See:/ 'newBatchDetachFromIndex' smart constructor.
data BatchDetachFromIndex = BatchDetachFromIndex'
  { -- | A reference to the index object.
    indexReference :: ObjectReference,
    -- | A reference to the object being detached from the index.
    targetReference :: ObjectReference
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchDetachFromIndex' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'indexReference', 'batchDetachFromIndex_indexReference' - A reference to the index object.
--
-- 'targetReference', 'batchDetachFromIndex_targetReference' - A reference to the object being detached from the index.
newBatchDetachFromIndex ::
  -- | 'indexReference'
  ObjectReference ->
  -- | 'targetReference'
  ObjectReference ->
  BatchDetachFromIndex
newBatchDetachFromIndex
  pIndexReference_
  pTargetReference_ =
    BatchDetachFromIndex'
      { indexReference =
          pIndexReference_,
        targetReference = pTargetReference_
      }

-- | A reference to the index object.
batchDetachFromIndex_indexReference :: Lens.Lens' BatchDetachFromIndex ObjectReference
batchDetachFromIndex_indexReference = Lens.lens (\BatchDetachFromIndex' {indexReference} -> indexReference) (\s@BatchDetachFromIndex' {} a -> s {indexReference = a} :: BatchDetachFromIndex)

-- | A reference to the object being detached from the index.
batchDetachFromIndex_targetReference :: Lens.Lens' BatchDetachFromIndex ObjectReference
batchDetachFromIndex_targetReference = Lens.lens (\BatchDetachFromIndex' {targetReference} -> targetReference) (\s@BatchDetachFromIndex' {} a -> s {targetReference = a} :: BatchDetachFromIndex)

instance Prelude.Hashable BatchDetachFromIndex where
  hashWithSalt _salt BatchDetachFromIndex' {..} =
    _salt `Prelude.hashWithSalt` indexReference
      `Prelude.hashWithSalt` targetReference

instance Prelude.NFData BatchDetachFromIndex where
  rnf BatchDetachFromIndex' {..} =
    Prelude.rnf indexReference
      `Prelude.seq` Prelude.rnf targetReference

instance Core.ToJSON BatchDetachFromIndex where
  toJSON BatchDetachFromIndex' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("IndexReference" Core..= indexReference),
            Prelude.Just
              ("TargetReference" Core..= targetReference)
          ]
      )
