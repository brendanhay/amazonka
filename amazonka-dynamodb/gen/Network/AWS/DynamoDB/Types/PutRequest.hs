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
-- Module      : Network.AWS.DynamoDB.Types.PutRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.PutRequest where

import qualified Network.AWS.Core as Core
import Network.AWS.DynamoDB.Types.AttributeValue
import qualified Network.AWS.Lens as Lens

-- | Represents a request to perform a @PutItem@ operation on an item.
--
-- /See:/ 'newPutRequest' smart constructor.
data PutRequest = PutRequest'
  { -- | A map of attribute name to attribute values, representing the primary
    -- key of an item to be processed by @PutItem@. All of the table\'s primary
    -- key attributes must be specified, and their data types must match those
    -- of the table\'s key schema. If any attributes are present in the item
    -- that are part of an index key schema for the table, their types must
    -- match the index key schema.
    item :: Core.HashMap Core.Text AttributeValue
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'item', 'putRequest_item' - A map of attribute name to attribute values, representing the primary
-- key of an item to be processed by @PutItem@. All of the table\'s primary
-- key attributes must be specified, and their data types must match those
-- of the table\'s key schema. If any attributes are present in the item
-- that are part of an index key schema for the table, their types must
-- match the index key schema.
newPutRequest ::
  PutRequest
newPutRequest = PutRequest' {item = Core.mempty}

-- | A map of attribute name to attribute values, representing the primary
-- key of an item to be processed by @PutItem@. All of the table\'s primary
-- key attributes must be specified, and their data types must match those
-- of the table\'s key schema. If any attributes are present in the item
-- that are part of an index key schema for the table, their types must
-- match the index key schema.
putRequest_item :: Lens.Lens' PutRequest (Core.HashMap Core.Text AttributeValue)
putRequest_item = Lens.lens (\PutRequest' {item} -> item) (\s@PutRequest' {} a -> s {item = a} :: PutRequest) Core.. Lens._Coerce

instance Core.FromJSON PutRequest where
  parseJSON =
    Core.withObject
      "PutRequest"
      ( \x ->
          PutRequest'
            Core.<$> (x Core..:? "Item" Core..!= Core.mempty)
      )

instance Core.Hashable PutRequest

instance Core.NFData PutRequest

instance Core.ToJSON PutRequest where
  toJSON PutRequest' {..} =
    Core.object
      (Core.catMaybes [Core.Just ("Item" Core..= item)])
