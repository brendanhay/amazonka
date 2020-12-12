{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.ObjectAttributeRange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.ObjectAttributeRange
  ( ObjectAttributeRange (..),

    -- * Smart constructor
    mkObjectAttributeRange,

    -- * Lenses
    oarRange,
    oarAttributeKey,
  )
where

import Network.AWS.CloudDirectory.Types.AttributeKey
import Network.AWS.CloudDirectory.Types.TypedAttributeValueRange
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A range of attributes.
--
-- /See:/ 'mkObjectAttributeRange' smart constructor.
data ObjectAttributeRange = ObjectAttributeRange'
  { range ::
      Lude.Maybe TypedAttributeValueRange,
    attributeKey :: Lude.Maybe AttributeKey
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ObjectAttributeRange' with the minimum fields required to make a request.
--
-- * 'attributeKey' - The key of the attribute that the attribute range covers.
-- * 'range' - The range of attribute values being selected.
mkObjectAttributeRange ::
  ObjectAttributeRange
mkObjectAttributeRange =
  ObjectAttributeRange'
    { range = Lude.Nothing,
      attributeKey = Lude.Nothing
    }

-- | The range of attribute values being selected.
--
-- /Note:/ Consider using 'range' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oarRange :: Lens.Lens' ObjectAttributeRange (Lude.Maybe TypedAttributeValueRange)
oarRange = Lens.lens (range :: ObjectAttributeRange -> Lude.Maybe TypedAttributeValueRange) (\s a -> s {range = a} :: ObjectAttributeRange)
{-# DEPRECATED oarRange "Use generic-lens or generic-optics with 'range' instead." #-}

-- | The key of the attribute that the attribute range covers.
--
-- /Note:/ Consider using 'attributeKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oarAttributeKey :: Lens.Lens' ObjectAttributeRange (Lude.Maybe AttributeKey)
oarAttributeKey = Lens.lens (attributeKey :: ObjectAttributeRange -> Lude.Maybe AttributeKey) (\s a -> s {attributeKey = a} :: ObjectAttributeRange)
{-# DEPRECATED oarAttributeKey "Use generic-lens or generic-optics with 'attributeKey' instead." #-}

instance Lude.ToJSON ObjectAttributeRange where
  toJSON ObjectAttributeRange' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Range" Lude..=) Lude.<$> range,
            ("AttributeKey" Lude..=) Lude.<$> attributeKey
          ]
      )
