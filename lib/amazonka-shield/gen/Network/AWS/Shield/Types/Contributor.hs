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
    cName,
    cValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Shield.Types.String as Types

-- | A contributor to the attack and their contribution.
--
-- /See:/ 'mkContributor' smart constructor.
data Contributor = Contributor'
  { -- | The name of the contributor. This is dependent on the @AttackPropertyIdentifier@ . For example, if the @AttackPropertyIdentifier@ is @SOURCE_COUNTRY@ , the @Name@ could be @United States@ .
    name :: Core.Maybe Types.String,
    -- | The contribution of this contributor expressed in 'Protection' units. For example @10,000@ .
    value :: Core.Maybe Core.Integer
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Contributor' value with any optional fields omitted.
mkContributor ::
  Contributor
mkContributor =
  Contributor' {name = Core.Nothing, value = Core.Nothing}

-- | The name of the contributor. This is dependent on the @AttackPropertyIdentifier@ . For example, if the @AttackPropertyIdentifier@ is @SOURCE_COUNTRY@ , the @Name@ could be @United States@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cName :: Lens.Lens' Contributor (Core.Maybe Types.String)
cName = Lens.field @"name"
{-# DEPRECATED cName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The contribution of this contributor expressed in 'Protection' units. For example @10,000@ .
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cValue :: Lens.Lens' Contributor (Core.Maybe Core.Integer)
cValue = Lens.field @"value"
{-# DEPRECATED cValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Core.FromJSON Contributor where
  parseJSON =
    Core.withObject "Contributor" Core.$
      \x ->
        Contributor'
          Core.<$> (x Core..:? "Name") Core.<*> (x Core..:? "Value")
