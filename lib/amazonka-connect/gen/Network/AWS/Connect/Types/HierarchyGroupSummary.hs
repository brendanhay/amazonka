{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.HierarchyGroupSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.HierarchyGroupSummary
  ( HierarchyGroupSummary (..),

    -- * Smart constructor
    mkHierarchyGroupSummary,

    -- * Lenses
    hgsARN,
    hgsName,
    hgsId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains summary information about a hierarchy group.
--
-- /See:/ 'mkHierarchyGroupSummary' smart constructor.
data HierarchyGroupSummary = HierarchyGroupSummary'
  { arn ::
      Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text,
    id :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HierarchyGroupSummary' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of the hierarchy group.
-- * 'id' - The identifier of the hierarchy group.
-- * 'name' - The name of the hierarchy group.
mkHierarchyGroupSummary ::
  HierarchyGroupSummary
mkHierarchyGroupSummary =
  HierarchyGroupSummary'
    { arn = Lude.Nothing,
      name = Lude.Nothing,
      id = Lude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the hierarchy group.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsARN :: Lens.Lens' HierarchyGroupSummary (Lude.Maybe Lude.Text)
hgsARN = Lens.lens (arn :: HierarchyGroupSummary -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: HierarchyGroupSummary)
{-# DEPRECATED hgsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The name of the hierarchy group.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsName :: Lens.Lens' HierarchyGroupSummary (Lude.Maybe Lude.Text)
hgsName = Lens.lens (name :: HierarchyGroupSummary -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: HierarchyGroupSummary)
{-# DEPRECATED hgsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The identifier of the hierarchy group.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hgsId :: Lens.Lens' HierarchyGroupSummary (Lude.Maybe Lude.Text)
hgsId = Lens.lens (id :: HierarchyGroupSummary -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: HierarchyGroupSummary)
{-# DEPRECATED hgsId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.FromJSON HierarchyGroupSummary where
  parseJSON =
    Lude.withObject
      "HierarchyGroupSummary"
      ( \x ->
          HierarchyGroupSummary'
            Lude.<$> (x Lude..:? "Arn")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "Id")
      )
