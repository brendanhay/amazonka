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
-- Module      : Network.AWS.Comprehend.Types.EntityTypesListItem
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.EntityTypesListItem where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | An entity type within a labeled training dataset that Amazon Comprehend
-- uses to train a custom entity recognizer.
--
-- /See:/ 'newEntityTypesListItem' smart constructor.
data EntityTypesListItem = EntityTypesListItem'
  { -- | An entity type within a labeled training dataset that Amazon Comprehend
    -- uses to train a custom entity recognizer.
    --
    -- Entity types must not contain the following invalid characters: \\n
    -- (line break), \\\\n (escaped line break, \\r (carriage return), \\\\r
    -- (escaped carriage return), \\t (tab), \\\\t (escaped tab), space, and ,
    -- (comma).
    type' :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EntityTypesListItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'entityTypesListItem_type' - An entity type within a labeled training dataset that Amazon Comprehend
-- uses to train a custom entity recognizer.
--
-- Entity types must not contain the following invalid characters: \\n
-- (line break), \\\\n (escaped line break, \\r (carriage return), \\\\r
-- (escaped carriage return), \\t (tab), \\\\t (escaped tab), space, and ,
-- (comma).
newEntityTypesListItem ::
  -- | 'type''
  Core.Text ->
  EntityTypesListItem
newEntityTypesListItem pType_ =
  EntityTypesListItem' {type' = pType_}

-- | An entity type within a labeled training dataset that Amazon Comprehend
-- uses to train a custom entity recognizer.
--
-- Entity types must not contain the following invalid characters: \\n
-- (line break), \\\\n (escaped line break, \\r (carriage return), \\\\r
-- (escaped carriage return), \\t (tab), \\\\t (escaped tab), space, and ,
-- (comma).
entityTypesListItem_type :: Lens.Lens' EntityTypesListItem Core.Text
entityTypesListItem_type = Lens.lens (\EntityTypesListItem' {type'} -> type') (\s@EntityTypesListItem' {} a -> s {type' = a} :: EntityTypesListItem)

instance Core.FromJSON EntityTypesListItem where
  parseJSON =
    Core.withObject
      "EntityTypesListItem"
      ( \x ->
          EntityTypesListItem' Core.<$> (x Core..: "Type")
      )

instance Core.Hashable EntityTypesListItem

instance Core.NFData EntityTypesListItem

instance Core.ToJSON EntityTypesListItem where
  toJSON EntityTypesListItem' {..} =
    Core.object
      (Core.catMaybes [Core.Just ("Type" Core..= type')])
