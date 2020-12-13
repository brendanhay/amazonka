{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.ValueWithServiceIds
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.ValueWithServiceIds
  ( ValueWithServiceIds (..),

    -- * Smart constructor
    mkValueWithServiceIds,

    -- * Lenses
    vwsiServiceIds,
    vwsiAnnotationValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.XRay.Types.AnnotationValue
import Network.AWS.XRay.Types.ServiceId

-- | Information about a segment annotation.
--
-- /See:/ 'mkValueWithServiceIds' smart constructor.
data ValueWithServiceIds = ValueWithServiceIds'
  { -- | Services to which the annotation applies.
    serviceIds :: Lude.Maybe [ServiceId],
    -- | Values of the annotation.
    annotationValue :: Lude.Maybe AnnotationValue
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ValueWithServiceIds' with the minimum fields required to make a request.
--
-- * 'serviceIds' - Services to which the annotation applies.
-- * 'annotationValue' - Values of the annotation.
mkValueWithServiceIds ::
  ValueWithServiceIds
mkValueWithServiceIds =
  ValueWithServiceIds'
    { serviceIds = Lude.Nothing,
      annotationValue = Lude.Nothing
    }

-- | Services to which the annotation applies.
--
-- /Note:/ Consider using 'serviceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vwsiServiceIds :: Lens.Lens' ValueWithServiceIds (Lude.Maybe [ServiceId])
vwsiServiceIds = Lens.lens (serviceIds :: ValueWithServiceIds -> Lude.Maybe [ServiceId]) (\s a -> s {serviceIds = a} :: ValueWithServiceIds)
{-# DEPRECATED vwsiServiceIds "Use generic-lens or generic-optics with 'serviceIds' instead." #-}

-- | Values of the annotation.
--
-- /Note:/ Consider using 'annotationValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vwsiAnnotationValue :: Lens.Lens' ValueWithServiceIds (Lude.Maybe AnnotationValue)
vwsiAnnotationValue = Lens.lens (annotationValue :: ValueWithServiceIds -> Lude.Maybe AnnotationValue) (\s a -> s {annotationValue = a} :: ValueWithServiceIds)
{-# DEPRECATED vwsiAnnotationValue "Use generic-lens or generic-optics with 'annotationValue' instead." #-}

instance Lude.FromJSON ValueWithServiceIds where
  parseJSON =
    Lude.withObject
      "ValueWithServiceIds"
      ( \x ->
          ValueWithServiceIds'
            Lude.<$> (x Lude..:? "ServiceIds" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "AnnotationValue")
      )
