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
-- Module      : Network.AWS.CloudDirectory.Types.LinkAttributeAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.LinkAttributeAction where

import Network.AWS.CloudDirectory.Types.TypedAttributeValue
import Network.AWS.CloudDirectory.Types.UpdateActionType
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The action to take on a typed link attribute value. Updates are only
-- supported for attributes which don’t contribute to link identity.
--
-- /See:/ 'newLinkAttributeAction' smart constructor.
data LinkAttributeAction = LinkAttributeAction'
  { -- | The value that you want to update to.
    attributeUpdateValue :: Core.Maybe TypedAttributeValue,
    -- | A type that can be either @UPDATE_OR_CREATE@ or @DELETE@.
    attributeActionType :: Core.Maybe UpdateActionType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'LinkAttributeAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributeUpdateValue', 'linkAttributeAction_attributeUpdateValue' - The value that you want to update to.
--
-- 'attributeActionType', 'linkAttributeAction_attributeActionType' - A type that can be either @UPDATE_OR_CREATE@ or @DELETE@.
newLinkAttributeAction ::
  LinkAttributeAction
newLinkAttributeAction =
  LinkAttributeAction'
    { attributeUpdateValue =
        Core.Nothing,
      attributeActionType = Core.Nothing
    }

-- | The value that you want to update to.
linkAttributeAction_attributeUpdateValue :: Lens.Lens' LinkAttributeAction (Core.Maybe TypedAttributeValue)
linkAttributeAction_attributeUpdateValue = Lens.lens (\LinkAttributeAction' {attributeUpdateValue} -> attributeUpdateValue) (\s@LinkAttributeAction' {} a -> s {attributeUpdateValue = a} :: LinkAttributeAction)

-- | A type that can be either @UPDATE_OR_CREATE@ or @DELETE@.
linkAttributeAction_attributeActionType :: Lens.Lens' LinkAttributeAction (Core.Maybe UpdateActionType)
linkAttributeAction_attributeActionType = Lens.lens (\LinkAttributeAction' {attributeActionType} -> attributeActionType) (\s@LinkAttributeAction' {} a -> s {attributeActionType = a} :: LinkAttributeAction)

instance Core.Hashable LinkAttributeAction

instance Core.NFData LinkAttributeAction

instance Core.ToJSON LinkAttributeAction where
  toJSON LinkAttributeAction' {..} =
    Core.object
      ( Core.catMaybes
          [ ("AttributeUpdateValue" Core..=)
              Core.<$> attributeUpdateValue,
            ("AttributeActionType" Core..=)
              Core.<$> attributeActionType
          ]
      )
