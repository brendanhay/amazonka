-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.LaunchPathSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.LaunchPathSummary
  ( LaunchPathSummary (..),

    -- * Smart constructor
    mkLaunchPathSummary,

    -- * Lenses
    lpsConstraintSummaries,
    lpsName,
    lpsId,
    lpsTags,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.ServiceCatalog.Types.ConstraintSummary
import Network.AWS.ServiceCatalog.Types.Tag

-- | Summary information about a product path for a user.
--
-- /See:/ 'mkLaunchPathSummary' smart constructor.
data LaunchPathSummary = LaunchPathSummary'
  { constraintSummaries ::
      Lude.Maybe [ConstraintSummary],
    name :: Lude.Maybe Lude.Text,
    id :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LaunchPathSummary' with the minimum fields required to make a request.
--
-- * 'constraintSummaries' - The constraints on the portfolio-product relationship.
-- * 'id' - The identifier of the product path.
-- * 'name' - The name of the portfolio to which the user was assigned.
-- * 'tags' - The tags associated with this product path.
mkLaunchPathSummary ::
  LaunchPathSummary
mkLaunchPathSummary =
  LaunchPathSummary'
    { constraintSummaries = Lude.Nothing,
      name = Lude.Nothing,
      id = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The constraints on the portfolio-product relationship.
--
-- /Note:/ Consider using 'constraintSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpsConstraintSummaries :: Lens.Lens' LaunchPathSummary (Lude.Maybe [ConstraintSummary])
lpsConstraintSummaries = Lens.lens (constraintSummaries :: LaunchPathSummary -> Lude.Maybe [ConstraintSummary]) (\s a -> s {constraintSummaries = a} :: LaunchPathSummary)
{-# DEPRECATED lpsConstraintSummaries "Use generic-lens or generic-optics with 'constraintSummaries' instead." #-}

-- | The name of the portfolio to which the user was assigned.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpsName :: Lens.Lens' LaunchPathSummary (Lude.Maybe Lude.Text)
lpsName = Lens.lens (name :: LaunchPathSummary -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: LaunchPathSummary)
{-# DEPRECATED lpsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The identifier of the product path.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpsId :: Lens.Lens' LaunchPathSummary (Lude.Maybe Lude.Text)
lpsId = Lens.lens (id :: LaunchPathSummary -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: LaunchPathSummary)
{-# DEPRECATED lpsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The tags associated with this product path.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpsTags :: Lens.Lens' LaunchPathSummary (Lude.Maybe [Tag])
lpsTags = Lens.lens (tags :: LaunchPathSummary -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: LaunchPathSummary)
{-# DEPRECATED lpsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromJSON LaunchPathSummary where
  parseJSON =
    Lude.withObject
      "LaunchPathSummary"
      ( \x ->
          LaunchPathSummary'
            Lude.<$> (x Lude..:? "ConstraintSummaries" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "Id")
            Lude.<*> (x Lude..:? "Tags" Lude..!= Lude.mempty)
      )
