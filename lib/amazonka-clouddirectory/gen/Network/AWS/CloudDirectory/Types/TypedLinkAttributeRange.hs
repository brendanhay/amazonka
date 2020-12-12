{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.TypedLinkAttributeRange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.TypedLinkAttributeRange
  ( TypedLinkAttributeRange (..),

    -- * Smart constructor
    mkTypedLinkAttributeRange,

    -- * Lenses
    tlarAttributeName,
    tlarRange,
  )
where

import Network.AWS.CloudDirectory.Types.TypedAttributeValueRange
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Identifies the range of attributes that are used by a specified filter.
--
-- /See:/ 'mkTypedLinkAttributeRange' smart constructor.
data TypedLinkAttributeRange = TypedLinkAttributeRange'
  { attributeName ::
      Lude.Maybe Lude.Text,
    range :: TypedAttributeValueRange
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TypedLinkAttributeRange' with the minimum fields required to make a request.
--
-- * 'attributeName' - The unique name of the typed link attribute.
-- * 'range' - The range of attribute values that are being selected.
mkTypedLinkAttributeRange ::
  -- | 'range'
  TypedAttributeValueRange ->
  TypedLinkAttributeRange
mkTypedLinkAttributeRange pRange_ =
  TypedLinkAttributeRange'
    { attributeName = Lude.Nothing,
      range = pRange_
    }

-- | The unique name of the typed link attribute.
--
-- /Note:/ Consider using 'attributeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tlarAttributeName :: Lens.Lens' TypedLinkAttributeRange (Lude.Maybe Lude.Text)
tlarAttributeName = Lens.lens (attributeName :: TypedLinkAttributeRange -> Lude.Maybe Lude.Text) (\s a -> s {attributeName = a} :: TypedLinkAttributeRange)
{-# DEPRECATED tlarAttributeName "Use generic-lens or generic-optics with 'attributeName' instead." #-}

-- | The range of attribute values that are being selected.
--
-- /Note:/ Consider using 'range' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tlarRange :: Lens.Lens' TypedLinkAttributeRange TypedAttributeValueRange
tlarRange = Lens.lens (range :: TypedLinkAttributeRange -> TypedAttributeValueRange) (\s a -> s {range = a} :: TypedLinkAttributeRange)
{-# DEPRECATED tlarRange "Use generic-lens or generic-optics with 'range' instead." #-}

instance Lude.ToJSON TypedLinkAttributeRange where
  toJSON TypedLinkAttributeRange' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("AttributeName" Lude..=) Lude.<$> attributeName,
            Lude.Just ("Range" Lude..= range)
          ]
      )
