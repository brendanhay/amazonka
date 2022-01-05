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
-- Module      : Amazonka.S3.Types.Delete
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.Delete where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal
import Amazonka.S3.Types.ObjectIdentifier

-- | Container for the objects to delete.
--
-- /See:/ 'newDelete' smart constructor.
data Delete = Delete'
  { -- | Element to enable quiet mode for the request. When you add this element,
    -- you must set its value to true.
    quiet :: Prelude.Maybe Prelude.Bool,
    -- | The objects to delete.
    objects :: [ObjectIdentifier]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Delete' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'quiet', 'delete_quiet' - Element to enable quiet mode for the request. When you add this element,
-- you must set its value to true.
--
-- 'objects', 'delete_objects' - The objects to delete.
newDelete ::
  Delete
newDelete =
  Delete'
    { quiet = Prelude.Nothing,
      objects = Prelude.mempty
    }

-- | Element to enable quiet mode for the request. When you add this element,
-- you must set its value to true.
delete_quiet :: Lens.Lens' Delete (Prelude.Maybe Prelude.Bool)
delete_quiet = Lens.lens (\Delete' {quiet} -> quiet) (\s@Delete' {} a -> s {quiet = a} :: Delete)

-- | The objects to delete.
delete_objects :: Lens.Lens' Delete [ObjectIdentifier]
delete_objects = Lens.lens (\Delete' {objects} -> objects) (\s@Delete' {} a -> s {objects = a} :: Delete) Prelude.. Lens.coerced

instance Prelude.Hashable Delete where
  hashWithSalt _salt Delete' {..} =
    _salt `Prelude.hashWithSalt` quiet
      `Prelude.hashWithSalt` objects

instance Prelude.NFData Delete where
  rnf Delete' {..} =
    Prelude.rnf quiet `Prelude.seq` Prelude.rnf objects

instance Core.ToXML Delete where
  toXML Delete' {..} =
    Prelude.mconcat
      [ "Quiet" Core.@= quiet,
        Core.toXMLList "Object" objects
      ]
