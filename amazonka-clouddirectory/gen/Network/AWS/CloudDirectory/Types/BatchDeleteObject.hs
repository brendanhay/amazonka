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
-- Module      : Network.AWS.CloudDirectory.Types.BatchDeleteObject
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchDeleteObject where

import Network.AWS.CloudDirectory.Types.ObjectReference
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Represents the output of a DeleteObject operation.
--
-- /See:/ 'newBatchDeleteObject' smart constructor.
data BatchDeleteObject = BatchDeleteObject'
  { -- | The reference that identifies the object.
    objectReference :: ObjectReference
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BatchDeleteObject' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'objectReference', 'batchDeleteObject_objectReference' - The reference that identifies the object.
newBatchDeleteObject ::
  -- | 'objectReference'
  ObjectReference ->
  BatchDeleteObject
newBatchDeleteObject pObjectReference_ =
  BatchDeleteObject'
    { objectReference =
        pObjectReference_
    }

-- | The reference that identifies the object.
batchDeleteObject_objectReference :: Lens.Lens' BatchDeleteObject ObjectReference
batchDeleteObject_objectReference = Lens.lens (\BatchDeleteObject' {objectReference} -> objectReference) (\s@BatchDeleteObject' {} a -> s {objectReference = a} :: BatchDeleteObject)

instance Core.Hashable BatchDeleteObject

instance Core.NFData BatchDeleteObject

instance Core.ToJSON BatchDeleteObject where
  toJSON BatchDeleteObject' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("ObjectReference" Core..= objectReference)
          ]
      )
