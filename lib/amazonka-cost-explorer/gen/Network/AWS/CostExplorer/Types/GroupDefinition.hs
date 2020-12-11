-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.GroupDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.GroupDefinition
  ( GroupDefinition (..),

    -- * Smart constructor
    mkGroupDefinition,

    -- * Lenses
    gdKey,
    gdType,
  )
where

import Network.AWS.CostExplorer.Types.GroupDefinitionType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents a group when you specify a group by criteria or in the response to a query with a specific grouping.
--
-- /See:/ 'mkGroupDefinition' smart constructor.
data GroupDefinition = GroupDefinition'
  { key ::
      Lude.Maybe Lude.Text,
    type' :: Lude.Maybe GroupDefinitionType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GroupDefinition' with the minimum fields required to make a request.
--
-- * 'key' - The string that represents a key for a specified group.
-- * 'type'' - The string that represents the type of group.
mkGroupDefinition ::
  GroupDefinition
mkGroupDefinition =
  GroupDefinition' {key = Lude.Nothing, type' = Lude.Nothing}

-- | The string that represents a key for a specified group.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdKey :: Lens.Lens' GroupDefinition (Lude.Maybe Lude.Text)
gdKey = Lens.lens (key :: GroupDefinition -> Lude.Maybe Lude.Text) (\s a -> s {key = a} :: GroupDefinition)
{-# DEPRECATED gdKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | The string that represents the type of group.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdType :: Lens.Lens' GroupDefinition (Lude.Maybe GroupDefinitionType)
gdType = Lens.lens (type' :: GroupDefinition -> Lude.Maybe GroupDefinitionType) (\s a -> s {type' = a} :: GroupDefinition)
{-# DEPRECATED gdType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromJSON GroupDefinition where
  parseJSON =
    Lude.withObject
      "GroupDefinition"
      ( \x ->
          GroupDefinition'
            Lude.<$> (x Lude..:? "Key") Lude.<*> (x Lude..:? "Type")
      )

instance Lude.ToJSON GroupDefinition where
  toJSON GroupDefinition' {..} =
    Lude.object
      ( Lude.catMaybes
          [("Key" Lude..=) Lude.<$> key, ("Type" Lude..=) Lude.<$> type']
      )
