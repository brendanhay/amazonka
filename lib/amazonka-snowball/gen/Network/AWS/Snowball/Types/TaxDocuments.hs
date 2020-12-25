{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.Types.TaxDocuments
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Snowball.Types.TaxDocuments
  ( TaxDocuments (..),

    -- * Smart constructor
    mkTaxDocuments,

    -- * Lenses
    tdIND,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Snowball.Types.INDTaxDocuments as Types

-- | The tax documents required in your AWS Region.
--
-- /See:/ 'mkTaxDocuments' smart constructor.
newtype TaxDocuments = TaxDocuments'
  { ind :: Core.Maybe Types.INDTaxDocuments
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'TaxDocuments' value with any optional fields omitted.
mkTaxDocuments ::
  TaxDocuments
mkTaxDocuments = TaxDocuments' {ind = Core.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'ind' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdIND :: Lens.Lens' TaxDocuments (Core.Maybe Types.INDTaxDocuments)
tdIND = Lens.field @"ind"
{-# DEPRECATED tdIND "Use generic-lens or generic-optics with 'ind' instead." #-}

instance Core.FromJSON TaxDocuments where
  toJSON TaxDocuments {..} =
    Core.object (Core.catMaybes [("IND" Core..=) Core.<$> ind])

instance Core.FromJSON TaxDocuments where
  parseJSON =
    Core.withObject "TaxDocuments" Core.$
      \x -> TaxDocuments' Core.<$> (x Core..:? "IND")
