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
-- Module      : Network.AWS.DynamoDB.Types.ItemResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ItemResponse where

import qualified Network.AWS.Core as Core
import Network.AWS.DynamoDB.Types.AttributeValue
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Details for the requested item.
--
-- /See:/ 'newItemResponse' smart constructor.
data ItemResponse = ItemResponse'
  { -- | Map of attribute data consisting of the data type and attribute value.
    item :: Prelude.Maybe (Prelude.HashMap Prelude.Text AttributeValue)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ItemResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'item', 'itemResponse_item' - Map of attribute data consisting of the data type and attribute value.
newItemResponse ::
  ItemResponse
newItemResponse =
  ItemResponse' {item = Prelude.Nothing}

-- | Map of attribute data consisting of the data type and attribute value.
itemResponse_item :: Lens.Lens' ItemResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text AttributeValue))
itemResponse_item = Lens.lens (\ItemResponse' {item} -> item) (\s@ItemResponse' {} a -> s {item = a} :: ItemResponse) Prelude.. Lens.mapping Lens._Coerce

instance Core.FromJSON ItemResponse where
  parseJSON =
    Core.withObject
      "ItemResponse"
      ( \x ->
          ItemResponse'
            Prelude.<$> (x Core..:? "Item" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable ItemResponse

instance Prelude.NFData ItemResponse
