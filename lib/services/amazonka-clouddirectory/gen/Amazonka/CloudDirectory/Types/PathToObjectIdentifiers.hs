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
-- Module      : Amazonka.CloudDirectory.Types.PathToObjectIdentifiers
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudDirectory.Types.PathToObjectIdentifiers where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Returns the path to the @ObjectIdentifiers@ that is associated with the
-- directory.
--
-- /See:/ 'newPathToObjectIdentifiers' smart constructor.
data PathToObjectIdentifiers = PathToObjectIdentifiers'
  { -- | The path that is used to identify the object starting from directory
    -- root.
    path :: Prelude.Maybe Prelude.Text,
    -- | Lists @ObjectIdentifiers@ starting from directory root to the object in
    -- the request.
    objectIdentifiers :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PathToObjectIdentifiers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'path', 'pathToObjectIdentifiers_path' - The path that is used to identify the object starting from directory
-- root.
--
-- 'objectIdentifiers', 'pathToObjectIdentifiers_objectIdentifiers' - Lists @ObjectIdentifiers@ starting from directory root to the object in
-- the request.
newPathToObjectIdentifiers ::
  PathToObjectIdentifiers
newPathToObjectIdentifiers =
  PathToObjectIdentifiers'
    { path = Prelude.Nothing,
      objectIdentifiers = Prelude.Nothing
    }

-- | The path that is used to identify the object starting from directory
-- root.
pathToObjectIdentifiers_path :: Lens.Lens' PathToObjectIdentifiers (Prelude.Maybe Prelude.Text)
pathToObjectIdentifiers_path = Lens.lens (\PathToObjectIdentifiers' {path} -> path) (\s@PathToObjectIdentifiers' {} a -> s {path = a} :: PathToObjectIdentifiers)

-- | Lists @ObjectIdentifiers@ starting from directory root to the object in
-- the request.
pathToObjectIdentifiers_objectIdentifiers :: Lens.Lens' PathToObjectIdentifiers (Prelude.Maybe [Prelude.Text])
pathToObjectIdentifiers_objectIdentifiers = Lens.lens (\PathToObjectIdentifiers' {objectIdentifiers} -> objectIdentifiers) (\s@PathToObjectIdentifiers' {} a -> s {objectIdentifiers = a} :: PathToObjectIdentifiers) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON PathToObjectIdentifiers where
  parseJSON =
    Core.withObject
      "PathToObjectIdentifiers"
      ( \x ->
          PathToObjectIdentifiers'
            Prelude.<$> (x Core..:? "Path")
            Prelude.<*> ( x Core..:? "ObjectIdentifiers"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable PathToObjectIdentifiers where
  hashWithSalt _salt PathToObjectIdentifiers' {..} =
    _salt `Prelude.hashWithSalt` path
      `Prelude.hashWithSalt` objectIdentifiers

instance Prelude.NFData PathToObjectIdentifiers where
  rnf PathToObjectIdentifiers' {..} =
    Prelude.rnf path
      `Prelude.seq` Prelude.rnf objectIdentifiers
