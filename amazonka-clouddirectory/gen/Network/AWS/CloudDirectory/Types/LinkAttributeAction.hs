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
-- Module      : Network.AWS.CloudDirectory.Types.LinkAttributeAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.LinkAttributeAction where

import Network.AWS.CloudDirectory.Types.TypedAttributeValue
import Network.AWS.CloudDirectory.Types.UpdateActionType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The action to take on a typed link attribute value. Updates are only
-- supported for attributes which don’t contribute to link identity.
--
-- /See:/ 'newLinkAttributeAction' smart constructor.
data LinkAttributeAction = LinkAttributeAction'
  { -- | The value that you want to update to.
    attributeUpdateValue :: Prelude.Maybe TypedAttributeValue,
    -- | A type that can be either @UPDATE_OR_CREATE@ or @DELETE@.
    attributeActionType :: Prelude.Maybe UpdateActionType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      attributeActionType = Prelude.Nothing
    }

-- | The value that you want to update to.
linkAttributeAction_attributeUpdateValue :: Lens.Lens' LinkAttributeAction (Prelude.Maybe TypedAttributeValue)
linkAttributeAction_attributeUpdateValue = Lens.lens (\LinkAttributeAction' {attributeUpdateValue} -> attributeUpdateValue) (\s@LinkAttributeAction' {} a -> s {attributeUpdateValue = a} :: LinkAttributeAction)

-- | A type that can be either @UPDATE_OR_CREATE@ or @DELETE@.
linkAttributeAction_attributeActionType :: Lens.Lens' LinkAttributeAction (Prelude.Maybe UpdateActionType)
linkAttributeAction_attributeActionType = Lens.lens (\LinkAttributeAction' {attributeActionType} -> attributeActionType) (\s@LinkAttributeAction' {} a -> s {attributeActionType = a} :: LinkAttributeAction)

instance Prelude.Hashable LinkAttributeAction

instance Prelude.NFData LinkAttributeAction

instance Prelude.ToJSON LinkAttributeAction where
  toJSON LinkAttributeAction' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("AttributeUpdateValue" Prelude..=)
              Prelude.<$> attributeUpdateValue,
            ("AttributeActionType" Prelude..=)
              Prelude.<$> attributeActionType
          ]
      )
