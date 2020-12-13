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
import qualified Network.AWS.Prelude as Lude

-- | The tax documents required in AWS Regions in India.
--
-- /See:/ 'mkINDTaxDocuments' smart constructor.
newtype INDTaxDocuments = INDTaxDocuments'
  { -- | The Goods and Services Tax (GST) documents required in AWS Regions in India.
    gSTIN :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'INDTaxDocuments' with the minimum fields required to make a request.
--
-- * 'gSTIN' - The Goods and Services Tax (GST) documents required in AWS Regions in India.
mkINDTaxDocuments ::
  INDTaxDocuments
mkINDTaxDocuments = INDTaxDocuments' {gSTIN = Lude.Nothing}

-- | The Goods and Services Tax (GST) documents required in AWS Regions in India.
--
-- /Note:/ Consider using 'gSTIN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
indtdGSTIN :: Lens.Lens' INDTaxDocuments (Lude.Maybe Lude.Text)
indtdGSTIN = Lens.lens (gSTIN :: INDTaxDocuments -> Lude.Maybe Lude.Text) (\s a -> s {gSTIN = a} :: INDTaxDocuments)
{-# DEPRECATED indtdGSTIN "Use generic-lens or generic-optics with 'gSTIN' instead." #-}

instance Lude.FromJSON INDTaxDocuments where
  parseJSON =
    Lude.withObject
      "INDTaxDocuments"
      (\x -> INDTaxDocuments' Lude.<$> (x Lude..:? "GSTIN"))

instance Lude.ToJSON INDTaxDocuments where
  toJSON INDTaxDocuments' {..} =
    Lude.object (Lude.catMaybes [("GSTIN" Lude..=) Lude.<$> gSTIN])
