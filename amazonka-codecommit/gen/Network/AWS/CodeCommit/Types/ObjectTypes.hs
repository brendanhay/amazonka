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
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Information about the type of an object in a merge operation.
--
-- /See:/ 'newObjectTypes' smart constructor.
data ObjectTypes = ObjectTypes'
  { -- | The type of the object in the source branch.
    source :: Core.Maybe ObjectTypeEnum,
    -- | The type of the object in the destination branch.
    destination :: Core.Maybe ObjectTypeEnum,
    -- | The type of the object in the base commit of the merge.
    base :: Core.Maybe ObjectTypeEnum
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { source = Core.Nothing,
      destination = Core.Nothing,
      base = Core.Nothing
    }

-- | The type of the object in the source branch.
objectTypes_source :: Lens.Lens' ObjectTypes (Core.Maybe ObjectTypeEnum)
objectTypes_source = Lens.lens (\ObjectTypes' {source} -> source) (\s@ObjectTypes' {} a -> s {source = a} :: ObjectTypes)

-- | The type of the object in the destination branch.
objectTypes_destination :: Lens.Lens' ObjectTypes (Core.Maybe ObjectTypeEnum)
objectTypes_destination = Lens.lens (\ObjectTypes' {destination} -> destination) (\s@ObjectTypes' {} a -> s {destination = a} :: ObjectTypes)

-- | The type of the object in the base commit of the merge.
objectTypes_base :: Lens.Lens' ObjectTypes (Core.Maybe ObjectTypeEnum)
objectTypes_base = Lens.lens (\ObjectTypes' {base} -> base) (\s@ObjectTypes' {} a -> s {base = a} :: ObjectTypes)

instance Core.FromJSON ObjectTypes where
  parseJSON =
    Core.withObject
      "ObjectTypes"
      ( \x ->
          ObjectTypes'
            Core.<$> (x Core..:? "source")
            Core.<*> (x Core..:? "destination")
            Core.<*> (x Core..:? "base")
      )

instance Core.Hashable ObjectTypes

instance Core.NFData ObjectTypes
