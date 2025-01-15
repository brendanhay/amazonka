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
-- Module      : Amazonka.CloudDirectory.Types.BatchDetachObject
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudDirectory.Types.BatchDetachObject where

import Amazonka.CloudDirectory.Types.ObjectReference
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents the output of a DetachObject operation.
--
-- /See:/ 'newBatchDetachObject' smart constructor.
data BatchDetachObject = BatchDetachObject'
  { -- | The batch reference name. See
    -- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/transaction_support.html Transaction Support>
    -- for more information.
    batchReferenceName :: Prelude.Maybe Prelude.Text,
    -- | Parent reference from which the object with the specified link name is
    -- detached.
    parentReference :: ObjectReference,
    -- | The name of the link.
    linkName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchDetachObject' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'batchReferenceName', 'batchDetachObject_batchReferenceName' - The batch reference name. See
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/transaction_support.html Transaction Support>
-- for more information.
--
-- 'parentReference', 'batchDetachObject_parentReference' - Parent reference from which the object with the specified link name is
-- detached.
--
-- 'linkName', 'batchDetachObject_linkName' - The name of the link.
newBatchDetachObject ::
  -- | 'parentReference'
  ObjectReference ->
  -- | 'linkName'
  Prelude.Text ->
  BatchDetachObject
newBatchDetachObject pParentReference_ pLinkName_ =
  BatchDetachObject'
    { batchReferenceName =
        Prelude.Nothing,
      parentReference = pParentReference_,
      linkName = pLinkName_
    }

-- | The batch reference name. See
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/transaction_support.html Transaction Support>
-- for more information.
batchDetachObject_batchReferenceName :: Lens.Lens' BatchDetachObject (Prelude.Maybe Prelude.Text)
batchDetachObject_batchReferenceName = Lens.lens (\BatchDetachObject' {batchReferenceName} -> batchReferenceName) (\s@BatchDetachObject' {} a -> s {batchReferenceName = a} :: BatchDetachObject)

-- | Parent reference from which the object with the specified link name is
-- detached.
batchDetachObject_parentReference :: Lens.Lens' BatchDetachObject ObjectReference
batchDetachObject_parentReference = Lens.lens (\BatchDetachObject' {parentReference} -> parentReference) (\s@BatchDetachObject' {} a -> s {parentReference = a} :: BatchDetachObject)

-- | The name of the link.
batchDetachObject_linkName :: Lens.Lens' BatchDetachObject Prelude.Text
batchDetachObject_linkName = Lens.lens (\BatchDetachObject' {linkName} -> linkName) (\s@BatchDetachObject' {} a -> s {linkName = a} :: BatchDetachObject)

instance Prelude.Hashable BatchDetachObject where
  hashWithSalt _salt BatchDetachObject' {..} =
    _salt
      `Prelude.hashWithSalt` batchReferenceName
      `Prelude.hashWithSalt` parentReference
      `Prelude.hashWithSalt` linkName

instance Prelude.NFData BatchDetachObject where
  rnf BatchDetachObject' {..} =
    Prelude.rnf batchReferenceName `Prelude.seq`
      Prelude.rnf parentReference `Prelude.seq`
        Prelude.rnf linkName

instance Data.ToJSON BatchDetachObject where
  toJSON BatchDetachObject' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BatchReferenceName" Data..=)
              Prelude.<$> batchReferenceName,
            Prelude.Just
              ("ParentReference" Data..= parentReference),
            Prelude.Just ("LinkName" Data..= linkName)
          ]
      )
