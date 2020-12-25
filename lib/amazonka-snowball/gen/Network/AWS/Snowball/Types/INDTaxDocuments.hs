{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.Types.INDTaxDocuments
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Snowball.Types.INDTaxDocuments
  ( INDTaxDocuments (..),

    -- * Smart constructor
    mkINDTaxDocuments,

    -- * Lenses
    indtdGSTIN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Snowball.Types.GSTIN as Types

-- | The tax documents required in AWS Regions in India.
--
-- /See:/ 'mkINDTaxDocuments' smart constructor.
newtype INDTaxDocuments = INDTaxDocuments'
  { -- | The Goods and Services Tax (GST) documents required in AWS Regions in India.
    gstin :: Core.Maybe Types.GSTIN
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'INDTaxDocuments' value with any optional fields omitted.
mkINDTaxDocuments ::
  INDTaxDocuments
mkINDTaxDocuments = INDTaxDocuments' {gstin = Core.Nothing}

-- | The Goods and Services Tax (GST) documents required in AWS Regions in India.
--
-- /Note:/ Consider using 'gstin' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
indtdGSTIN :: Lens.Lens' INDTaxDocuments (Core.Maybe Types.GSTIN)
indtdGSTIN = Lens.field @"gstin"
{-# DEPRECATED indtdGSTIN "Use generic-lens or generic-optics with 'gstin' instead." #-}

instance Core.FromJSON INDTaxDocuments where
  toJSON INDTaxDocuments {..} =
    Core.object (Core.catMaybes [("GSTIN" Core..=) Core.<$> gstin])

instance Core.FromJSON INDTaxDocuments where
  parseJSON =
    Core.withObject "INDTaxDocuments" Core.$
      \x -> INDTaxDocuments' Core.<$> (x Core..:? "GSTIN")
