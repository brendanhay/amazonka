{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CloudDirectory.Types.BatchDetachFromIndex
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchDetachFromIndex where

import Network.AWS.CloudDirectory.Types.ObjectReference
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.Hashable BatchDetachFromIndex

instance Prelude.NFData BatchDetachFromIndex

instance Prelude.ToJSON BatchDetachFromIndex where
  toJSON BatchDetachFromIndex' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("IndexReference" Prelude..= indexReference),
            Prelude.Just
              ("TargetReference" Prelude..= targetReference)
          ]
      )
