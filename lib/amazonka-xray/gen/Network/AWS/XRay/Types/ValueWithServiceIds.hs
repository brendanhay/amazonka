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
    vwsiAnnotationValue,
    vwsiServiceIds,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.XRay.Types.AnnotationValue as Types
import qualified Network.AWS.XRay.Types.ServiceId as Types

-- | Information about a segment annotation.
--
-- /See:/ 'mkValueWithServiceIds' smart constructor.
data ValueWithServiceIds = ValueWithServiceIds'
  { -- | Values of the annotation.
    annotationValue :: Core.Maybe Types.AnnotationValue,
    -- | Services to which the annotation applies.
    serviceIds :: Core.Maybe [Types.ServiceId]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ValueWithServiceIds' value with any optional fields omitted.
mkValueWithServiceIds ::
  ValueWithServiceIds
mkValueWithServiceIds =
  ValueWithServiceIds'
    { annotationValue = Core.Nothing,
      serviceIds = Core.Nothing
    }

-- | Values of the annotation.
--
-- /Note:/ Consider using 'annotationValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vwsiAnnotationValue :: Lens.Lens' ValueWithServiceIds (Core.Maybe Types.AnnotationValue)
vwsiAnnotationValue = Lens.field @"annotationValue"
{-# DEPRECATED vwsiAnnotationValue "Use generic-lens or generic-optics with 'annotationValue' instead." #-}

-- | Services to which the annotation applies.
--
-- /Note:/ Consider using 'serviceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vwsiServiceIds :: Lens.Lens' ValueWithServiceIds (Core.Maybe [Types.ServiceId])
vwsiServiceIds = Lens.field @"serviceIds"
{-# DEPRECATED vwsiServiceIds "Use generic-lens or generic-optics with 'serviceIds' instead." #-}

instance Core.FromJSON ValueWithServiceIds where
  parseJSON =
    Core.withObject "ValueWithServiceIds" Core.$
      \x ->
        ValueWithServiceIds'
          Core.<$> (x Core..:? "AnnotationValue") Core.<*> (x Core..:? "ServiceIds")
