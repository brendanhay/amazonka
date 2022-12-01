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
-- Module      : Amazonka.CloudDirectory.Types.BatchAttachObject
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudDirectory.Types.BatchAttachObject where

import Amazonka.CloudDirectory.Types.ObjectReference
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Represents the output of an AttachObject operation.
--
-- /See:/ 'newBatchAttachObject' smart constructor.
data BatchAttachObject = BatchAttachObject'
  { -- | The parent object reference.
    parentReference :: ObjectReference,
    -- | The child object reference that is to be attached to the object.
    childReference :: ObjectReference,
    -- | The name of the link.
    linkName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchAttachObject' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parentReference', 'batchAttachObject_parentReference' - The parent object reference.
--
-- 'childReference', 'batchAttachObject_childReference' - The child object reference that is to be attached to the object.
--
-- 'linkName', 'batchAttachObject_linkName' - The name of the link.
newBatchAttachObject ::
  -- | 'parentReference'
  ObjectReference ->
  -- | 'childReference'
  ObjectReference ->
  -- | 'linkName'
  Prelude.Text ->
  BatchAttachObject
newBatchAttachObject
  pParentReference_
  pChildReference_
  pLinkName_ =
    BatchAttachObject'
      { parentReference =
          pParentReference_,
        childReference = pChildReference_,
        linkName = pLinkName_
      }

-- | The parent object reference.
batchAttachObject_parentReference :: Lens.Lens' BatchAttachObject ObjectReference
batchAttachObject_parentReference = Lens.lens (\BatchAttachObject' {parentReference} -> parentReference) (\s@BatchAttachObject' {} a -> s {parentReference = a} :: BatchAttachObject)

-- | The child object reference that is to be attached to the object.
batchAttachObject_childReference :: Lens.Lens' BatchAttachObject ObjectReference
batchAttachObject_childReference = Lens.lens (\BatchAttachObject' {childReference} -> childReference) (\s@BatchAttachObject' {} a -> s {childReference = a} :: BatchAttachObject)

-- | The name of the link.
batchAttachObject_linkName :: Lens.Lens' BatchAttachObject Prelude.Text
batchAttachObject_linkName = Lens.lens (\BatchAttachObject' {linkName} -> linkName) (\s@BatchAttachObject' {} a -> s {linkName = a} :: BatchAttachObject)

instance Prelude.Hashable BatchAttachObject where
  hashWithSalt _salt BatchAttachObject' {..} =
    _salt `Prelude.hashWithSalt` parentReference
      `Prelude.hashWithSalt` childReference
      `Prelude.hashWithSalt` linkName

instance Prelude.NFData BatchAttachObject where
  rnf BatchAttachObject' {..} =
    Prelude.rnf parentReference
      `Prelude.seq` Prelude.rnf childReference
      `Prelude.seq` Prelude.rnf linkName

instance Core.ToJSON BatchAttachObject where
  toJSON BatchAttachObject' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ParentReference" Core..= parentReference),
            Prelude.Just
              ("ChildReference" Core..= childReference),
            Prelude.Just ("LinkName" Core..= linkName)
          ]
      )
