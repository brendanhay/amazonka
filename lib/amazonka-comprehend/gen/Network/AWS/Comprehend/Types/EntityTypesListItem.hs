{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.EntityTypesListItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.EntityTypesListItem
  ( EntityTypesListItem (..),

    -- * Smart constructor
    mkEntityTypesListItem,

    -- * Lenses
    etliType,
  )
where

import qualified Network.AWS.Comprehend.Types.EntityTypeName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An entity type within a labeled training dataset that Amazon Comprehend uses to train a custom entity recognizer.
--
-- /See:/ 'mkEntityTypesListItem' smart constructor.
newtype EntityTypesListItem = EntityTypesListItem'
  { -- | An entity type within a labeled training dataset that Amazon Comprehend uses to train a custom entity recognizer.
    --
    -- Entity types must not contain the following invalid characters: \n (line break), \\n (escaped line break, \r (carriage return), \\r (escaped carriage return), \t (tab), \\t (escaped tab), space, and , (comma).
    type' :: Types.EntityTypeName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'EntityTypesListItem' value with any optional fields omitted.
mkEntityTypesListItem ::
  -- | 'type\''
  Types.EntityTypeName ->
  EntityTypesListItem
mkEntityTypesListItem type' = EntityTypesListItem' {type'}

-- | An entity type within a labeled training dataset that Amazon Comprehend uses to train a custom entity recognizer.
--
-- Entity types must not contain the following invalid characters: \n (line break), \\n (escaped line break, \r (carriage return), \\r (escaped carriage return), \t (tab), \\t (escaped tab), space, and , (comma).
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etliType :: Lens.Lens' EntityTypesListItem Types.EntityTypeName
etliType = Lens.field @"type'"
{-# DEPRECATED etliType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Core.FromJSON EntityTypesListItem where
  toJSON EntityTypesListItem {..} =
    Core.object (Core.catMaybes [Core.Just ("Type" Core..= type')])

instance Core.FromJSON EntityTypesListItem where
  parseJSON =
    Core.withObject "EntityTypesListItem" Core.$
      \x -> EntityTypesListItem' Core.<$> (x Core..: "Type")
