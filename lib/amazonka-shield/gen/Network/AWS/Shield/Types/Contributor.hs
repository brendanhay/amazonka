{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.Types.Contributor
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Shield.Types.Contributor
  ( Contributor (..),

    -- * Smart constructor
    mkContributor,

    -- * Lenses
    cValue,
    cName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A contributor to the attack and their contribution.
--
-- /See:/ 'mkContributor' smart constructor.
data Contributor = Contributor'
  { -- | The contribution of this contributor expressed in 'Protection' units. For example @10,000@ .
    value :: Lude.Maybe Lude.Integer,
    -- | The name of the contributor. This is dependent on the @AttackPropertyIdentifier@ . For example, if the @AttackPropertyIdentifier@ is @SOURCE_COUNTRY@ , the @Name@ could be @United States@ .
    name :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Contributor' with the minimum fields required to make a request.
--
-- * 'value' - The contribution of this contributor expressed in 'Protection' units. For example @10,000@ .
-- * 'name' - The name of the contributor. This is dependent on the @AttackPropertyIdentifier@ . For example, if the @AttackPropertyIdentifier@ is @SOURCE_COUNTRY@ , the @Name@ could be @United States@ .
mkContributor ::
  Contributor
mkContributor =
  Contributor' {value = Lude.Nothing, name = Lude.Nothing}

-- | The contribution of this contributor expressed in 'Protection' units. For example @10,000@ .
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cValue :: Lens.Lens' Contributor (Lude.Maybe Lude.Integer)
cValue = Lens.lens (value :: Contributor -> Lude.Maybe Lude.Integer) (\s a -> s {value = a} :: Contributor)
{-# DEPRECATED cValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The name of the contributor. This is dependent on the @AttackPropertyIdentifier@ . For example, if the @AttackPropertyIdentifier@ is @SOURCE_COUNTRY@ , the @Name@ could be @United States@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cName :: Lens.Lens' Contributor (Lude.Maybe Lude.Text)
cName = Lens.lens (name :: Contributor -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Contributor)
{-# DEPRECATED cName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON Contributor where
  parseJSON =
    Lude.withObject
      "Contributor"
      ( \x ->
          Contributor'
            Lude.<$> (x Lude..:? "Value") Lude.<*> (x Lude..:? "Name")
      )
