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
-- Module      : Amazonka.CloudDirectory.Types.BatchDeleteObject
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudDirectory.Types.BatchDeleteObject where

import Amazonka.CloudDirectory.Types.ObjectReference
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents the output of a DeleteObject operation.
--
-- /See:/ 'newBatchDeleteObject' smart constructor.
data BatchDeleteObject = BatchDeleteObject'
  { -- | The reference that identifies the object.
    objectReference :: ObjectReference
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.Hashable BatchDeleteObject where
  hashWithSalt _salt BatchDeleteObject' {..} =
    _salt `Prelude.hashWithSalt` objectReference

instance Prelude.NFData BatchDeleteObject where
  rnf BatchDeleteObject' {..} =
    Prelude.rnf objectReference

instance Data.ToJSON BatchDeleteObject where
  toJSON BatchDeleteObject' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ObjectReference" Data..= objectReference)
          ]
      )
