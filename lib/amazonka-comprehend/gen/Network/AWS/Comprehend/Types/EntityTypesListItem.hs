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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An entity type within a labeled training dataset that Amazon Comprehend uses to train a custom entity recognizer.
--
-- /See:/ 'mkEntityTypesListItem' smart constructor.
newtype EntityTypesListItem = EntityTypesListItem'
  { type' ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EntityTypesListItem' with the minimum fields required to make a request.
--
-- * 'type'' - An entity type within a labeled training dataset that Amazon Comprehend uses to train a custom entity recognizer.
--
-- Entity types must not contain the following invalid characters: \n (line break), \\n (escaped line break, \r (carriage return), \\r (escaped carriage return), \t (tab), \\t (escaped tab), space, and , (comma).
mkEntityTypesListItem ::
  -- | 'type''
  Lude.Text ->
  EntityTypesListItem
mkEntityTypesListItem pType_ = EntityTypesListItem' {type' = pType_}

-- | An entity type within a labeled training dataset that Amazon Comprehend uses to train a custom entity recognizer.
--
-- Entity types must not contain the following invalid characters: \n (line break), \\n (escaped line break, \r (carriage return), \\r (escaped carriage return), \t (tab), \\t (escaped tab), space, and , (comma).
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etliType :: Lens.Lens' EntityTypesListItem Lude.Text
etliType = Lens.lens (type' :: EntityTypesListItem -> Lude.Text) (\s a -> s {type' = a} :: EntityTypesListItem)
{-# DEPRECATED etliType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromJSON EntityTypesListItem where
  parseJSON =
    Lude.withObject
      "EntityTypesListItem"
      (\x -> EntityTypesListItem' Lude.<$> (x Lude..: "Type"))

instance Lude.ToJSON EntityTypesListItem where
  toJSON EntityTypesListItem' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("Type" Lude..= type')])
