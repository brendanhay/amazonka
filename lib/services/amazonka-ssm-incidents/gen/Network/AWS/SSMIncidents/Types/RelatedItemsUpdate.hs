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
-- Module      : Network.AWS.SSMIncidents.Types.RelatedItemsUpdate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSMIncidents.Types.RelatedItemsUpdate where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SSMIncidents.Types.ItemIdentifier
import Network.AWS.SSMIncidents.Types.RelatedItem

-- | Details about the related item you\'re adding.
--
-- /See:/ 'newRelatedItemsUpdate' smart constructor.
data RelatedItemsUpdate = RelatedItemsUpdate'
  { -- | Details about the related item you\'re deleting.
    itemToRemove :: Prelude.Maybe ItemIdentifier,
    -- | Details about the related item you\'re adding.
    itemToAdd :: Prelude.Maybe RelatedItem
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RelatedItemsUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'itemToRemove', 'relatedItemsUpdate_itemToRemove' - Details about the related item you\'re deleting.
--
-- 'itemToAdd', 'relatedItemsUpdate_itemToAdd' - Details about the related item you\'re adding.
newRelatedItemsUpdate ::
  RelatedItemsUpdate
newRelatedItemsUpdate =
  RelatedItemsUpdate'
    { itemToRemove = Prelude.Nothing,
      itemToAdd = Prelude.Nothing
    }

-- | Details about the related item you\'re deleting.
relatedItemsUpdate_itemToRemove :: Lens.Lens' RelatedItemsUpdate (Prelude.Maybe ItemIdentifier)
relatedItemsUpdate_itemToRemove = Lens.lens (\RelatedItemsUpdate' {itemToRemove} -> itemToRemove) (\s@RelatedItemsUpdate' {} a -> s {itemToRemove = a} :: RelatedItemsUpdate)

-- | Details about the related item you\'re adding.
relatedItemsUpdate_itemToAdd :: Lens.Lens' RelatedItemsUpdate (Prelude.Maybe RelatedItem)
relatedItemsUpdate_itemToAdd = Lens.lens (\RelatedItemsUpdate' {itemToAdd} -> itemToAdd) (\s@RelatedItemsUpdate' {} a -> s {itemToAdd = a} :: RelatedItemsUpdate)

instance Prelude.Hashable RelatedItemsUpdate

instance Prelude.NFData RelatedItemsUpdate

instance Core.ToJSON RelatedItemsUpdate where
  toJSON RelatedItemsUpdate' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("itemToRemove" Core..=) Prelude.<$> itemToRemove,
            ("itemToAdd" Core..=) Prelude.<$> itemToAdd
          ]
      )
