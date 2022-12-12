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
-- Module      : Amazonka.CodeCommit.Types.ObjectTypes
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeCommit.Types.ObjectTypes where

import Amazonka.CodeCommit.Types.ObjectTypeEnum
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the type of an object in a merge operation.
--
-- /See:/ 'newObjectTypes' smart constructor.
data ObjectTypes = ObjectTypes'
  { -- | The type of the object in the base commit of the merge.
    base :: Prelude.Maybe ObjectTypeEnum,
    -- | The type of the object in the destination branch.
    destination :: Prelude.Maybe ObjectTypeEnum,
    -- | The type of the object in the source branch.
    source :: Prelude.Maybe ObjectTypeEnum
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ObjectTypes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'base', 'objectTypes_base' - The type of the object in the base commit of the merge.
--
-- 'destination', 'objectTypes_destination' - The type of the object in the destination branch.
--
-- 'source', 'objectTypes_source' - The type of the object in the source branch.
newObjectTypes ::
  ObjectTypes
newObjectTypes =
  ObjectTypes'
    { base = Prelude.Nothing,
      destination = Prelude.Nothing,
      source = Prelude.Nothing
    }

-- | The type of the object in the base commit of the merge.
objectTypes_base :: Lens.Lens' ObjectTypes (Prelude.Maybe ObjectTypeEnum)
objectTypes_base = Lens.lens (\ObjectTypes' {base} -> base) (\s@ObjectTypes' {} a -> s {base = a} :: ObjectTypes)

-- | The type of the object in the destination branch.
objectTypes_destination :: Lens.Lens' ObjectTypes (Prelude.Maybe ObjectTypeEnum)
objectTypes_destination = Lens.lens (\ObjectTypes' {destination} -> destination) (\s@ObjectTypes' {} a -> s {destination = a} :: ObjectTypes)

-- | The type of the object in the source branch.
objectTypes_source :: Lens.Lens' ObjectTypes (Prelude.Maybe ObjectTypeEnum)
objectTypes_source = Lens.lens (\ObjectTypes' {source} -> source) (\s@ObjectTypes' {} a -> s {source = a} :: ObjectTypes)

instance Data.FromJSON ObjectTypes where
  parseJSON =
    Data.withObject
      "ObjectTypes"
      ( \x ->
          ObjectTypes'
            Prelude.<$> (x Data..:? "base")
            Prelude.<*> (x Data..:? "destination")
            Prelude.<*> (x Data..:? "source")
      )

instance Prelude.Hashable ObjectTypes where
  hashWithSalt _salt ObjectTypes' {..} =
    _salt `Prelude.hashWithSalt` base
      `Prelude.hashWithSalt` destination
      `Prelude.hashWithSalt` source

instance Prelude.NFData ObjectTypes where
  rnf ObjectTypes' {..} =
    Prelude.rnf base
      `Prelude.seq` Prelude.rnf destination
      `Prelude.seq` Prelude.rnf source
