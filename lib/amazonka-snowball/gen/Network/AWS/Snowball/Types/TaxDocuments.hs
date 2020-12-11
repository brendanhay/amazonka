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
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Snowball.Types.INDTaxDocuments

-- | The tax documents required in your AWS Region.
--
-- /See:/ 'mkTaxDocuments' smart constructor.
newtype TaxDocuments = TaxDocuments'
  { iND ::
      Lude.Maybe INDTaxDocuments
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TaxDocuments' with the minimum fields required to make a request.
--
-- * 'iND' - Undocumented field.
mkTaxDocuments ::
  TaxDocuments
mkTaxDocuments = TaxDocuments' {iND = Lude.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'iND' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdIND :: Lens.Lens' TaxDocuments (Lude.Maybe INDTaxDocuments)
tdIND = Lens.lens (iND :: TaxDocuments -> Lude.Maybe INDTaxDocuments) (\s a -> s {iND = a} :: TaxDocuments)
{-# DEPRECATED tdIND "Use generic-lens or generic-optics with 'iND' instead." #-}

instance Lude.FromJSON TaxDocuments where
  parseJSON =
    Lude.withObject
      "TaxDocuments"
      (\x -> TaxDocuments' Lude.<$> (x Lude..:? "IND"))

instance Lude.ToJSON TaxDocuments where
  toJSON TaxDocuments' {..} =
    Lude.object (Lude.catMaybes [("IND" Lude..=) Lude.<$> iND])
