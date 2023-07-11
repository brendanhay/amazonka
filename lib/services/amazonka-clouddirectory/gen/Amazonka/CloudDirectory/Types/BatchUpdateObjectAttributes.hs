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
-- Module      : Amazonka.CloudDirectory.Types.BatchUpdateObjectAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudDirectory.Types.BatchUpdateObjectAttributes where

import Amazonka.CloudDirectory.Types.ObjectAttributeUpdate
import Amazonka.CloudDirectory.Types.ObjectReference
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents the output of a @BatchUpdate@ operation.
--
-- /See:/ 'newBatchUpdateObjectAttributes' smart constructor.
data BatchUpdateObjectAttributes = BatchUpdateObjectAttributes'
  { -- | Reference that identifies the object.
    objectReference :: ObjectReference,
    -- | Attributes update structure.
    attributeUpdates :: [ObjectAttributeUpdate]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchUpdateObjectAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'objectReference', 'batchUpdateObjectAttributes_objectReference' - Reference that identifies the object.
--
-- 'attributeUpdates', 'batchUpdateObjectAttributes_attributeUpdates' - Attributes update structure.
newBatchUpdateObjectAttributes ::
  -- | 'objectReference'
  ObjectReference ->
  BatchUpdateObjectAttributes
newBatchUpdateObjectAttributes pObjectReference_ =
  BatchUpdateObjectAttributes'
    { objectReference =
        pObjectReference_,
      attributeUpdates = Prelude.mempty
    }

-- | Reference that identifies the object.
batchUpdateObjectAttributes_objectReference :: Lens.Lens' BatchUpdateObjectAttributes ObjectReference
batchUpdateObjectAttributes_objectReference = Lens.lens (\BatchUpdateObjectAttributes' {objectReference} -> objectReference) (\s@BatchUpdateObjectAttributes' {} a -> s {objectReference = a} :: BatchUpdateObjectAttributes)

-- | Attributes update structure.
batchUpdateObjectAttributes_attributeUpdates :: Lens.Lens' BatchUpdateObjectAttributes [ObjectAttributeUpdate]
batchUpdateObjectAttributes_attributeUpdates = Lens.lens (\BatchUpdateObjectAttributes' {attributeUpdates} -> attributeUpdates) (\s@BatchUpdateObjectAttributes' {} a -> s {attributeUpdates = a} :: BatchUpdateObjectAttributes) Prelude.. Lens.coerced

instance Prelude.Hashable BatchUpdateObjectAttributes where
  hashWithSalt _salt BatchUpdateObjectAttributes' {..} =
    _salt
      `Prelude.hashWithSalt` objectReference
      `Prelude.hashWithSalt` attributeUpdates

instance Prelude.NFData BatchUpdateObjectAttributes where
  rnf BatchUpdateObjectAttributes' {..} =
    Prelude.rnf objectReference
      `Prelude.seq` Prelude.rnf attributeUpdates

instance Data.ToJSON BatchUpdateObjectAttributes where
  toJSON BatchUpdateObjectAttributes' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ObjectReference" Data..= objectReference),
            Prelude.Just
              ("AttributeUpdates" Data..= attributeUpdates)
          ]
      )
