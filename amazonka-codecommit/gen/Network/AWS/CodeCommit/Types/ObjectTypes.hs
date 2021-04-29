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
-- Module      : Network.AWS.CodeCommit.Types.ObjectTypes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.ObjectTypes where

import Network.AWS.CodeCommit.Types.ObjectTypeEnum
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about the type of an object in a merge operation.
--
-- /See:/ 'newObjectTypes' smart constructor.
data ObjectTypes = ObjectTypes'
  { -- | The type of the object in the source branch.
    source :: Prelude.Maybe ObjectTypeEnum,
    -- | The type of the object in the destination branch.
    destination :: Prelude.Maybe ObjectTypeEnum,
    -- | The type of the object in the base commit of the merge.
    base :: Prelude.Maybe ObjectTypeEnum
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ObjectTypes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'source', 'objectTypes_source' - The type of the object in the source branch.
--
-- 'destination', 'objectTypes_destination' - The type of the object in the destination branch.
--
-- 'base', 'objectTypes_base' - The type of the object in the base commit of the merge.
newObjectTypes ::
  ObjectTypes
newObjectTypes =
  ObjectTypes'
    { source = Prelude.Nothing,
      destination = Prelude.Nothing,
      base = Prelude.Nothing
    }

-- | The type of the object in the source branch.
objectTypes_source :: Lens.Lens' ObjectTypes (Prelude.Maybe ObjectTypeEnum)
objectTypes_source = Lens.lens (\ObjectTypes' {source} -> source) (\s@ObjectTypes' {} a -> s {source = a} :: ObjectTypes)

-- | The type of the object in the destination branch.
objectTypes_destination :: Lens.Lens' ObjectTypes (Prelude.Maybe ObjectTypeEnum)
objectTypes_destination = Lens.lens (\ObjectTypes' {destination} -> destination) (\s@ObjectTypes' {} a -> s {destination = a} :: ObjectTypes)

-- | The type of the object in the base commit of the merge.
objectTypes_base :: Lens.Lens' ObjectTypes (Prelude.Maybe ObjectTypeEnum)
objectTypes_base = Lens.lens (\ObjectTypes' {base} -> base) (\s@ObjectTypes' {} a -> s {base = a} :: ObjectTypes)

instance Prelude.FromJSON ObjectTypes where
  parseJSON =
    Prelude.withObject
      "ObjectTypes"
      ( \x ->
          ObjectTypes'
            Prelude.<$> (x Prelude..:? "source")
            Prelude.<*> (x Prelude..:? "destination")
            Prelude.<*> (x Prelude..:? "base")
      )

instance Prelude.Hashable ObjectTypes

instance Prelude.NFData ObjectTypes
