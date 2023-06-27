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
-- Module      : Amazonka.CloudDirectory.Types.LinkAttributeAction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudDirectory.Types.LinkAttributeAction where

import Amazonka.CloudDirectory.Types.TypedAttributeValue
import Amazonka.CloudDirectory.Types.UpdateActionType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The action to take on a typed link attribute value. Updates are only
-- supported for attributes which don’t contribute to link identity.
--
-- /See:/ 'newLinkAttributeAction' smart constructor.
data LinkAttributeAction = LinkAttributeAction'
  { -- | A type that can be either @UPDATE_OR_CREATE@ or @DELETE@.
    attributeActionType :: Prelude.Maybe UpdateActionType,
    -- | The value that you want to update to.
    attributeUpdateValue :: Prelude.Maybe TypedAttributeValue
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LinkAttributeAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributeActionType', 'linkAttributeAction_attributeActionType' - A type that can be either @UPDATE_OR_CREATE@ or @DELETE@.
--
-- 'attributeUpdateValue', 'linkAttributeAction_attributeUpdateValue' - The value that you want to update to.
newLinkAttributeAction ::
  LinkAttributeAction
newLinkAttributeAction =
  LinkAttributeAction'
    { attributeActionType =
        Prelude.Nothing,
      attributeUpdateValue = Prelude.Nothing
    }

-- | A type that can be either @UPDATE_OR_CREATE@ or @DELETE@.
linkAttributeAction_attributeActionType :: Lens.Lens' LinkAttributeAction (Prelude.Maybe UpdateActionType)
linkAttributeAction_attributeActionType = Lens.lens (\LinkAttributeAction' {attributeActionType} -> attributeActionType) (\s@LinkAttributeAction' {} a -> s {attributeActionType = a} :: LinkAttributeAction)

-- | The value that you want to update to.
linkAttributeAction_attributeUpdateValue :: Lens.Lens' LinkAttributeAction (Prelude.Maybe TypedAttributeValue)
linkAttributeAction_attributeUpdateValue = Lens.lens (\LinkAttributeAction' {attributeUpdateValue} -> attributeUpdateValue) (\s@LinkAttributeAction' {} a -> s {attributeUpdateValue = a} :: LinkAttributeAction)

instance Prelude.Hashable LinkAttributeAction where
  hashWithSalt _salt LinkAttributeAction' {..} =
    _salt
      `Prelude.hashWithSalt` attributeActionType
      `Prelude.hashWithSalt` attributeUpdateValue

instance Prelude.NFData LinkAttributeAction where
  rnf LinkAttributeAction' {..} =
    Prelude.rnf attributeActionType
      `Prelude.seq` Prelude.rnf attributeUpdateValue

instance Data.ToJSON LinkAttributeAction where
  toJSON LinkAttributeAction' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AttributeActionType" Data..=)
              Prelude.<$> attributeActionType,
            ("AttributeUpdateValue" Data..=)
              Prelude.<$> attributeUpdateValue
          ]
      )
