-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.BlobAttributeValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.BlobAttributeValue
  ( BlobAttributeValue (..),

    -- * Smart constructor
    mkBlobAttributeValue,

    -- * Lenses
    bavValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | /See:/ 'mkBlobAttributeValue' smart constructor.
newtype BlobAttributeValue = BlobAttributeValue'
  { value ::
      Lude.Maybe Lude.Base64
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BlobAttributeValue' with the minimum fields required to make a request.
--
-- * 'value' - Undocumented field.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
mkBlobAttributeValue ::
  BlobAttributeValue
mkBlobAttributeValue = BlobAttributeValue' {value = Lude.Nothing}

-- | Undocumented field.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bavValue :: Lens.Lens' BlobAttributeValue (Lude.Maybe Lude.Base64)
bavValue = Lens.lens (value :: BlobAttributeValue -> Lude.Maybe Lude.Base64) (\s a -> s {value = a} :: BlobAttributeValue)
{-# DEPRECATED bavValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Lude.ToQuery BlobAttributeValue where
  toQuery BlobAttributeValue' {..} =
    Lude.mconcat ["Value" Lude.=: value]
