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
-- Module      : Network.AWS.CloudDirectory.Types.ObjectAttributeUpdate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.ObjectAttributeUpdate where

import Network.AWS.CloudDirectory.Types.AttributeKey
import Network.AWS.CloudDirectory.Types.ObjectAttributeAction
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Structure that contains attribute update information.
--
-- /See:/ 'newObjectAttributeUpdate' smart constructor.
data ObjectAttributeUpdate = ObjectAttributeUpdate'
  { -- | The action to perform as part of the attribute update.
    objectAttributeAction :: Core.Maybe ObjectAttributeAction,
    -- | The key of the attribute being updated.
    objectAttributeKey :: Core.Maybe AttributeKey
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ObjectAttributeUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'objectAttributeAction', 'objectAttributeUpdate_objectAttributeAction' - The action to perform as part of the attribute update.
--
-- 'objectAttributeKey', 'objectAttributeUpdate_objectAttributeKey' - The key of the attribute being updated.
newObjectAttributeUpdate ::
  ObjectAttributeUpdate
newObjectAttributeUpdate =
  ObjectAttributeUpdate'
    { objectAttributeAction =
        Core.Nothing,
      objectAttributeKey = Core.Nothing
    }

-- | The action to perform as part of the attribute update.
objectAttributeUpdate_objectAttributeAction :: Lens.Lens' ObjectAttributeUpdate (Core.Maybe ObjectAttributeAction)
objectAttributeUpdate_objectAttributeAction = Lens.lens (\ObjectAttributeUpdate' {objectAttributeAction} -> objectAttributeAction) (\s@ObjectAttributeUpdate' {} a -> s {objectAttributeAction = a} :: ObjectAttributeUpdate)

-- | The key of the attribute being updated.
objectAttributeUpdate_objectAttributeKey :: Lens.Lens' ObjectAttributeUpdate (Core.Maybe AttributeKey)
objectAttributeUpdate_objectAttributeKey = Lens.lens (\ObjectAttributeUpdate' {objectAttributeKey} -> objectAttributeKey) (\s@ObjectAttributeUpdate' {} a -> s {objectAttributeKey = a} :: ObjectAttributeUpdate)

instance Core.Hashable ObjectAttributeUpdate

instance Core.NFData ObjectAttributeUpdate

instance Core.ToJSON ObjectAttributeUpdate where
  toJSON ObjectAttributeUpdate' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ObjectAttributeAction" Core..=)
              Core.<$> objectAttributeAction,
            ("ObjectAttributeKey" Core..=)
              Core.<$> objectAttributeKey
          ]
      )
