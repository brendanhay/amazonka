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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudDirectory.Types.PathToObjectIdentifiers where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Returns the path to the @ObjectIdentifiers@ that is associated with the
-- directory.
--
-- /See:/ 'newPathToObjectIdentifiers' smart constructor.
data PathToObjectIdentifiers = PathToObjectIdentifiers'
  { -- | Lists @ObjectIdentifiers@ starting from directory root to the object in
    -- the request.
    objectIdentifiers :: Prelude.Maybe [Prelude.Text],
    -- | The path that is used to identify the object starting from directory
    -- root.
    path :: Prelude.Maybe Prelude.Text
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
-- 'objectIdentifiers', 'pathToObjectIdentifiers_objectIdentifiers' - Lists @ObjectIdentifiers@ starting from directory root to the object in
-- the request.
--
-- 'path', 'pathToObjectIdentifiers_path' - The path that is used to identify the object starting from directory
-- root.
newPathToObjectIdentifiers ::
  PathToObjectIdentifiers
newPathToObjectIdentifiers =
  PathToObjectIdentifiers'
    { objectIdentifiers =
        Prelude.Nothing,
      path = Prelude.Nothing
    }

-- | Lists @ObjectIdentifiers@ starting from directory root to the object in
-- the request.
pathToObjectIdentifiers_objectIdentifiers :: Lens.Lens' PathToObjectIdentifiers (Prelude.Maybe [Prelude.Text])
pathToObjectIdentifiers_objectIdentifiers = Lens.lens (\PathToObjectIdentifiers' {objectIdentifiers} -> objectIdentifiers) (\s@PathToObjectIdentifiers' {} a -> s {objectIdentifiers = a} :: PathToObjectIdentifiers) Prelude.. Lens.mapping Lens.coerced

-- | The path that is used to identify the object starting from directory
-- root.
pathToObjectIdentifiers_path :: Lens.Lens' PathToObjectIdentifiers (Prelude.Maybe Prelude.Text)
pathToObjectIdentifiers_path = Lens.lens (\PathToObjectIdentifiers' {path} -> path) (\s@PathToObjectIdentifiers' {} a -> s {path = a} :: PathToObjectIdentifiers)

instance Data.FromJSON PathToObjectIdentifiers where
  parseJSON =
    Data.withObject
      "PathToObjectIdentifiers"
      ( \x ->
          PathToObjectIdentifiers'
            Prelude.<$> ( x
                            Data..:? "ObjectIdentifiers"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "Path")
      )

instance Prelude.Hashable PathToObjectIdentifiers where
  hashWithSalt _salt PathToObjectIdentifiers' {..} =
    _salt
      `Prelude.hashWithSalt` objectIdentifiers
      `Prelude.hashWithSalt` path

instance Prelude.NFData PathToObjectIdentifiers where
  rnf PathToObjectIdentifiers' {..} =
    Prelude.rnf objectIdentifiers `Prelude.seq`
      Prelude.rnf path
