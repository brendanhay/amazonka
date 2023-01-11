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
-- Module      : Amazonka.CloudDirectory.Types.BatchAttachToIndex
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudDirectory.Types.BatchAttachToIndex where

import Amazonka.CloudDirectory.Types.ObjectReference
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Attaches the specified object to the specified index inside a BatchRead
-- operation. For more information, see AttachToIndex and
-- BatchReadRequest$Operations.
--
-- /See:/ 'newBatchAttachToIndex' smart constructor.
data BatchAttachToIndex = BatchAttachToIndex'
  { -- | A reference to the index that you are attaching the object to.
    indexReference :: ObjectReference,
    -- | A reference to the object that you are attaching to the index.
    targetReference :: ObjectReference
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchAttachToIndex' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'indexReference', 'batchAttachToIndex_indexReference' - A reference to the index that you are attaching the object to.
--
-- 'targetReference', 'batchAttachToIndex_targetReference' - A reference to the object that you are attaching to the index.
newBatchAttachToIndex ::
  -- | 'indexReference'
  ObjectReference ->
  -- | 'targetReference'
  ObjectReference ->
  BatchAttachToIndex
newBatchAttachToIndex
  pIndexReference_
  pTargetReference_ =
    BatchAttachToIndex'
      { indexReference =
          pIndexReference_,
        targetReference = pTargetReference_
      }

-- | A reference to the index that you are attaching the object to.
batchAttachToIndex_indexReference :: Lens.Lens' BatchAttachToIndex ObjectReference
batchAttachToIndex_indexReference = Lens.lens (\BatchAttachToIndex' {indexReference} -> indexReference) (\s@BatchAttachToIndex' {} a -> s {indexReference = a} :: BatchAttachToIndex)

-- | A reference to the object that you are attaching to the index.
batchAttachToIndex_targetReference :: Lens.Lens' BatchAttachToIndex ObjectReference
batchAttachToIndex_targetReference = Lens.lens (\BatchAttachToIndex' {targetReference} -> targetReference) (\s@BatchAttachToIndex' {} a -> s {targetReference = a} :: BatchAttachToIndex)

instance Prelude.Hashable BatchAttachToIndex where
  hashWithSalt _salt BatchAttachToIndex' {..} =
    _salt `Prelude.hashWithSalt` indexReference
      `Prelude.hashWithSalt` targetReference

instance Prelude.NFData BatchAttachToIndex where
  rnf BatchAttachToIndex' {..} =
    Prelude.rnf indexReference
      `Prelude.seq` Prelude.rnf targetReference

instance Data.ToJSON BatchAttachToIndex where
  toJSON BatchAttachToIndex' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("IndexReference" Data..= indexReference),
            Prelude.Just
              ("TargetReference" Data..= targetReference)
          ]
      )
