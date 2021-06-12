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
-- Module      : Network.AWS.DynamoDB.Types.DeleteRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.DeleteRequest where

import qualified Network.AWS.Core as Core
import Network.AWS.DynamoDB.Types.AttributeValue
import qualified Network.AWS.Lens as Lens

-- | Represents a request to perform a @DeleteItem@ operation on an item.
--
-- /See:/ 'newDeleteRequest' smart constructor.
data DeleteRequest = DeleteRequest'
  { -- | A map of attribute name to attribute values, representing the primary
    -- key of the item to delete. All of the table\'s primary key attributes
    -- must be specified, and their data types must match those of the table\'s
    -- key schema.
    key :: Core.HashMap Core.Text AttributeValue
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'deleteRequest_key' - A map of attribute name to attribute values, representing the primary
-- key of the item to delete. All of the table\'s primary key attributes
-- must be specified, and their data types must match those of the table\'s
-- key schema.
newDeleteRequest ::
  DeleteRequest
newDeleteRequest = DeleteRequest' {key = Core.mempty}

-- | A map of attribute name to attribute values, representing the primary
-- key of the item to delete. All of the table\'s primary key attributes
-- must be specified, and their data types must match those of the table\'s
-- key schema.
deleteRequest_key :: Lens.Lens' DeleteRequest (Core.HashMap Core.Text AttributeValue)
deleteRequest_key = Lens.lens (\DeleteRequest' {key} -> key) (\s@DeleteRequest' {} a -> s {key = a} :: DeleteRequest) Core.. Lens._Coerce

instance Core.FromJSON DeleteRequest where
  parseJSON =
    Core.withObject
      "DeleteRequest"
      ( \x ->
          DeleteRequest'
            Core.<$> (x Core..:? "Key" Core..!= Core.mempty)
      )

instance Core.Hashable DeleteRequest

instance Core.NFData DeleteRequest

instance Core.ToJSON DeleteRequest where
  toJSON DeleteRequest' {..} =
    Core.object
      (Core.catMaybes [Core.Just ("Key" Core..= key)])
