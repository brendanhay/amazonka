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
-- Module      : Network.AWS.CloudDirectory.Types.BatchGetObjectInformation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchGetObjectInformation where

import Network.AWS.CloudDirectory.Types.ObjectReference
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Retrieves metadata about an object inside a BatchRead operation. For
-- more information, see GetObjectInformation and
-- BatchReadRequest$Operations.
--
-- /See:/ 'newBatchGetObjectInformation' smart constructor.
data BatchGetObjectInformation = BatchGetObjectInformation'
  { -- | A reference to the object.
    objectReference :: ObjectReference
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BatchGetObjectInformation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'objectReference', 'batchGetObjectInformation_objectReference' - A reference to the object.
newBatchGetObjectInformation ::
  -- | 'objectReference'
  ObjectReference ->
  BatchGetObjectInformation
newBatchGetObjectInformation pObjectReference_ =
  BatchGetObjectInformation'
    { objectReference =
        pObjectReference_
    }

-- | A reference to the object.
batchGetObjectInformation_objectReference :: Lens.Lens' BatchGetObjectInformation ObjectReference
batchGetObjectInformation_objectReference = Lens.lens (\BatchGetObjectInformation' {objectReference} -> objectReference) (\s@BatchGetObjectInformation' {} a -> s {objectReference = a} :: BatchGetObjectInformation)

instance Core.Hashable BatchGetObjectInformation

instance Core.NFData BatchGetObjectInformation

instance Core.ToJSON BatchGetObjectInformation where
  toJSON BatchGetObjectInformation' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("ObjectReference" Core..= objectReference)
          ]
      )
