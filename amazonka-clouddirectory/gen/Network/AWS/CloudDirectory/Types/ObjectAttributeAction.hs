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
-- Module      : Network.AWS.CloudDirectory.Types.ObjectAttributeAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.ObjectAttributeAction where

import Network.AWS.CloudDirectory.Types.TypedAttributeValue
import Network.AWS.CloudDirectory.Types.UpdateActionType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The action to take on the object attribute.
--
-- /See:/ 'newObjectAttributeAction' smart constructor.
data ObjectAttributeAction = ObjectAttributeAction'
  { -- | A type that can be either @Update@ or @Delete@.
    objectAttributeActionType :: Prelude.Maybe UpdateActionType,
    -- | The value that you want to update to.
    objectAttributeUpdateValue :: Prelude.Maybe TypedAttributeValue
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ObjectAttributeAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'objectAttributeActionType', 'objectAttributeAction_objectAttributeActionType' - A type that can be either @Update@ or @Delete@.
--
-- 'objectAttributeUpdateValue', 'objectAttributeAction_objectAttributeUpdateValue' - The value that you want to update to.
newObjectAttributeAction ::
  ObjectAttributeAction
newObjectAttributeAction =
  ObjectAttributeAction'
    { objectAttributeActionType =
        Prelude.Nothing,
      objectAttributeUpdateValue = Prelude.Nothing
    }

-- | A type that can be either @Update@ or @Delete@.
objectAttributeAction_objectAttributeActionType :: Lens.Lens' ObjectAttributeAction (Prelude.Maybe UpdateActionType)
objectAttributeAction_objectAttributeActionType = Lens.lens (\ObjectAttributeAction' {objectAttributeActionType} -> objectAttributeActionType) (\s@ObjectAttributeAction' {} a -> s {objectAttributeActionType = a} :: ObjectAttributeAction)

-- | The value that you want to update to.
objectAttributeAction_objectAttributeUpdateValue :: Lens.Lens' ObjectAttributeAction (Prelude.Maybe TypedAttributeValue)
objectAttributeAction_objectAttributeUpdateValue = Lens.lens (\ObjectAttributeAction' {objectAttributeUpdateValue} -> objectAttributeUpdateValue) (\s@ObjectAttributeAction' {} a -> s {objectAttributeUpdateValue = a} :: ObjectAttributeAction)

instance Prelude.Hashable ObjectAttributeAction

instance Prelude.NFData ObjectAttributeAction

instance Prelude.ToJSON ObjectAttributeAction where
  toJSON ObjectAttributeAction' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ObjectAttributeActionType" Prelude..=)
              Prelude.<$> objectAttributeActionType,
            ("ObjectAttributeUpdateValue" Prelude..=)
              Prelude.<$> objectAttributeUpdateValue
          ]
      )
