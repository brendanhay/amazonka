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
-- Module      : Network.AWS.Comprehend.Types.EntityTypesListItem
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.EntityTypesListItem where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
    type' :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
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
entityTypesListItem_type :: Lens.Lens' EntityTypesListItem Prelude.Text
entityTypesListItem_type = Lens.lens (\EntityTypesListItem' {type'} -> type') (\s@EntityTypesListItem' {} a -> s {type' = a} :: EntityTypesListItem)

instance Prelude.FromJSON EntityTypesListItem where
  parseJSON =
    Prelude.withObject
      "EntityTypesListItem"
      ( \x ->
          EntityTypesListItem'
            Prelude.<$> (x Prelude..: "Type")
      )

instance Prelude.Hashable EntityTypesListItem

instance Prelude.NFData EntityTypesListItem

instance Prelude.ToJSON EntityTypesListItem where
  toJSON EntityTypesListItem' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("Type" Prelude..= type')]
      )
