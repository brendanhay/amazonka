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
-- Module      : Network.AWS.CloudDirectory.Types.BatchUpdateObjectAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchUpdateObjectAttributes where

import Network.AWS.CloudDirectory.Types.ObjectAttributeUpdate
import Network.AWS.CloudDirectory.Types.ObjectReference
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents the output of a @BatchUpdate@ operation.
--
-- /See:/ 'newBatchUpdateObjectAttributes' smart constructor.
data BatchUpdateObjectAttributes = BatchUpdateObjectAttributes'
  { -- | Reference that identifies the object.
    objectReference :: ObjectReference,
    -- | Attributes update structure.
    attributeUpdates :: [ObjectAttributeUpdate]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
batchUpdateObjectAttributes_attributeUpdates = Lens.lens (\BatchUpdateObjectAttributes' {attributeUpdates} -> attributeUpdates) (\s@BatchUpdateObjectAttributes' {} a -> s {attributeUpdates = a} :: BatchUpdateObjectAttributes) Prelude.. Prelude._Coerce

instance Prelude.Hashable BatchUpdateObjectAttributes

instance Prelude.NFData BatchUpdateObjectAttributes

instance Prelude.ToJSON BatchUpdateObjectAttributes where
  toJSON BatchUpdateObjectAttributes' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ObjectReference" Prelude..= objectReference),
            Prelude.Just
              ("AttributeUpdates" Prelude..= attributeUpdates)
          ]
      )
