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
-- Module      : Amazonka.CloudDirectory.Types.BatchGetObjectInformation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudDirectory.Types.BatchGetObjectInformation where

import Amazonka.CloudDirectory.Types.ObjectReference
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Retrieves metadata about an object inside a BatchRead operation. For
-- more information, see GetObjectInformation and
-- BatchReadRequest$Operations.
--
-- /See:/ 'newBatchGetObjectInformation' smart constructor.
data BatchGetObjectInformation = BatchGetObjectInformation'
  { -- | A reference to the object.
    objectReference :: ObjectReference
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.Hashable BatchGetObjectInformation where
  hashWithSalt _salt BatchGetObjectInformation' {..} =
    _salt `Prelude.hashWithSalt` objectReference

instance Prelude.NFData BatchGetObjectInformation where
  rnf BatchGetObjectInformation' {..} =
    Prelude.rnf objectReference

instance Data.ToJSON BatchGetObjectInformation where
  toJSON BatchGetObjectInformation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ObjectReference" Data..= objectReference)
          ]
      )
