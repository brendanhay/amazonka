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
-- Module      : Network.AWS.CloudDirectory.Types.ObjectAttributeUpdate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.ObjectAttributeUpdate where

import Network.AWS.CloudDirectory.Types.AttributeKey
import Network.AWS.CloudDirectory.Types.ObjectAttributeAction
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Structure that contains attribute update information.
--
-- /See:/ 'newObjectAttributeUpdate' smart constructor.
data ObjectAttributeUpdate = ObjectAttributeUpdate'
  { -- | The action to perform as part of the attribute update.
    objectAttributeAction :: Prelude.Maybe ObjectAttributeAction,
    -- | The key of the attribute being updated.
    objectAttributeKey :: Prelude.Maybe AttributeKey
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      objectAttributeKey = Prelude.Nothing
    }

-- | The action to perform as part of the attribute update.
objectAttributeUpdate_objectAttributeAction :: Lens.Lens' ObjectAttributeUpdate (Prelude.Maybe ObjectAttributeAction)
objectAttributeUpdate_objectAttributeAction = Lens.lens (\ObjectAttributeUpdate' {objectAttributeAction} -> objectAttributeAction) (\s@ObjectAttributeUpdate' {} a -> s {objectAttributeAction = a} :: ObjectAttributeUpdate)

-- | The key of the attribute being updated.
objectAttributeUpdate_objectAttributeKey :: Lens.Lens' ObjectAttributeUpdate (Prelude.Maybe AttributeKey)
objectAttributeUpdate_objectAttributeKey = Lens.lens (\ObjectAttributeUpdate' {objectAttributeKey} -> objectAttributeKey) (\s@ObjectAttributeUpdate' {} a -> s {objectAttributeKey = a} :: ObjectAttributeUpdate)

instance Prelude.Hashable ObjectAttributeUpdate

instance Prelude.NFData ObjectAttributeUpdate

instance Prelude.ToJSON ObjectAttributeUpdate where
  toJSON ObjectAttributeUpdate' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ObjectAttributeAction" Prelude..=)
              Prelude.<$> objectAttributeAction,
            ("ObjectAttributeKey" Prelude..=)
              Prelude.<$> objectAttributeKey
          ]
      )
