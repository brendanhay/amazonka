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
-- Module      : Amazonka.LakeFormation.Types.WriteOperation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LakeFormation.Types.WriteOperation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LakeFormation.Types.AddObjectInput
import Amazonka.LakeFormation.Types.DeleteObjectInput
import qualified Amazonka.Prelude as Prelude

-- | Defines an object to add to or delete from a governed table.
--
-- /See:/ 'newWriteOperation' smart constructor.
data WriteOperation = WriteOperation'
  { -- | An object to delete from the governed table.
    deleteObject :: Prelude.Maybe DeleteObjectInput,
    -- | A new object to add to the governed table.
    addObject :: Prelude.Maybe AddObjectInput
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WriteOperation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deleteObject', 'writeOperation_deleteObject' - An object to delete from the governed table.
--
-- 'addObject', 'writeOperation_addObject' - A new object to add to the governed table.
newWriteOperation ::
  WriteOperation
newWriteOperation =
  WriteOperation'
    { deleteObject = Prelude.Nothing,
      addObject = Prelude.Nothing
    }

-- | An object to delete from the governed table.
writeOperation_deleteObject :: Lens.Lens' WriteOperation (Prelude.Maybe DeleteObjectInput)
writeOperation_deleteObject = Lens.lens (\WriteOperation' {deleteObject} -> deleteObject) (\s@WriteOperation' {} a -> s {deleteObject = a} :: WriteOperation)

-- | A new object to add to the governed table.
writeOperation_addObject :: Lens.Lens' WriteOperation (Prelude.Maybe AddObjectInput)
writeOperation_addObject = Lens.lens (\WriteOperation' {addObject} -> addObject) (\s@WriteOperation' {} a -> s {addObject = a} :: WriteOperation)

instance Prelude.Hashable WriteOperation where
  hashWithSalt _salt WriteOperation' {..} =
    _salt `Prelude.hashWithSalt` deleteObject
      `Prelude.hashWithSalt` addObject

instance Prelude.NFData WriteOperation where
  rnf WriteOperation' {..} =
    Prelude.rnf deleteObject
      `Prelude.seq` Prelude.rnf addObject

instance Core.ToJSON WriteOperation where
  toJSON WriteOperation' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("DeleteObject" Core..=) Prelude.<$> deleteObject,
            ("AddObject" Core..=) Prelude.<$> addObject
          ]
      )
