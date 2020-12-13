{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.TypedAttributeValueRange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.TypedAttributeValueRange
  ( TypedAttributeValueRange (..),

    -- * Smart constructor
    mkTypedAttributeValueRange,

    -- * Lenses
    tavrEndValue,
    tavrStartValue,
    tavrStartMode,
    tavrEndMode,
  )
where

import Network.AWS.CloudDirectory.Types.RangeMode
import Network.AWS.CloudDirectory.Types.TypedAttributeValue
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A range of attribute values. For more information, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_range_filters.html Range Filters> .
--
-- /See:/ 'mkTypedAttributeValueRange' smart constructor.
data TypedAttributeValueRange = TypedAttributeValueRange'
  { -- | The attribute value to terminate the range at.
    endValue :: Lude.Maybe TypedAttributeValue,
    -- | The value to start the range at.
    startValue :: Lude.Maybe TypedAttributeValue,
    -- | The inclusive or exclusive range start.
    startMode :: RangeMode,
    -- | The inclusive or exclusive range end.
    endMode :: RangeMode
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TypedAttributeValueRange' with the minimum fields required to make a request.
--
-- * 'endValue' - The attribute value to terminate the range at.
-- * 'startValue' - The value to start the range at.
-- * 'startMode' - The inclusive or exclusive range start.
-- * 'endMode' - The inclusive or exclusive range end.
mkTypedAttributeValueRange ::
  -- | 'startMode'
  RangeMode ->
  -- | 'endMode'
  RangeMode ->
  TypedAttributeValueRange
mkTypedAttributeValueRange pStartMode_ pEndMode_ =
  TypedAttributeValueRange'
    { endValue = Lude.Nothing,
      startValue = Lude.Nothing,
      startMode = pStartMode_,
      endMode = pEndMode_
    }

-- | The attribute value to terminate the range at.
--
-- /Note:/ Consider using 'endValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tavrEndValue :: Lens.Lens' TypedAttributeValueRange (Lude.Maybe TypedAttributeValue)
tavrEndValue = Lens.lens (endValue :: TypedAttributeValueRange -> Lude.Maybe TypedAttributeValue) (\s a -> s {endValue = a} :: TypedAttributeValueRange)
{-# DEPRECATED tavrEndValue "Use generic-lens or generic-optics with 'endValue' instead." #-}

-- | The value to start the range at.
--
-- /Note:/ Consider using 'startValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tavrStartValue :: Lens.Lens' TypedAttributeValueRange (Lude.Maybe TypedAttributeValue)
tavrStartValue = Lens.lens (startValue :: TypedAttributeValueRange -> Lude.Maybe TypedAttributeValue) (\s a -> s {startValue = a} :: TypedAttributeValueRange)
{-# DEPRECATED tavrStartValue "Use generic-lens or generic-optics with 'startValue' instead." #-}

-- | The inclusive or exclusive range start.
--
-- /Note:/ Consider using 'startMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tavrStartMode :: Lens.Lens' TypedAttributeValueRange RangeMode
tavrStartMode = Lens.lens (startMode :: TypedAttributeValueRange -> RangeMode) (\s a -> s {startMode = a} :: TypedAttributeValueRange)
{-# DEPRECATED tavrStartMode "Use generic-lens or generic-optics with 'startMode' instead." #-}

-- | The inclusive or exclusive range end.
--
-- /Note:/ Consider using 'endMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tavrEndMode :: Lens.Lens' TypedAttributeValueRange RangeMode
tavrEndMode = Lens.lens (endMode :: TypedAttributeValueRange -> RangeMode) (\s a -> s {endMode = a} :: TypedAttributeValueRange)
{-# DEPRECATED tavrEndMode "Use generic-lens or generic-optics with 'endMode' instead." #-}

instance Lude.ToJSON TypedAttributeValueRange where
  toJSON TypedAttributeValueRange' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("EndValue" Lude..=) Lude.<$> endValue,
            ("StartValue" Lude..=) Lude.<$> startValue,
            Lude.Just ("StartMode" Lude..= startMode),
            Lude.Just ("EndMode" Lude..= endMode)
          ]
      )
