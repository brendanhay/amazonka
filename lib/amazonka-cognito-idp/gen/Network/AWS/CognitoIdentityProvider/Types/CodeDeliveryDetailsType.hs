{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.CodeDeliveryDetailsType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.CodeDeliveryDetailsType
  ( CodeDeliveryDetailsType (..),

    -- * Smart constructor
    mkCodeDeliveryDetailsType,

    -- * Lenses
    cddtAttributeName,
    cddtDeliveryMedium,
    cddtDestination,
  )
where

import qualified Network.AWS.CognitoIdentityProvider.Types.AttributeName as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.DeliveryMediumType as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.Destination as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The code delivery details being returned from the server.
--
-- /See:/ 'mkCodeDeliveryDetailsType' smart constructor.
data CodeDeliveryDetailsType = CodeDeliveryDetailsType'
  { -- | The attribute name.
    attributeName :: Core.Maybe Types.AttributeName,
    -- | The delivery medium (email message or phone number).
    deliveryMedium :: Core.Maybe Types.DeliveryMediumType,
    -- | The destination for the code delivery details.
    destination :: Core.Maybe Types.Destination
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CodeDeliveryDetailsType' value with any optional fields omitted.
mkCodeDeliveryDetailsType ::
  CodeDeliveryDetailsType
mkCodeDeliveryDetailsType =
  CodeDeliveryDetailsType'
    { attributeName = Core.Nothing,
      deliveryMedium = Core.Nothing,
      destination = Core.Nothing
    }

-- | The attribute name.
--
-- /Note:/ Consider using 'attributeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cddtAttributeName :: Lens.Lens' CodeDeliveryDetailsType (Core.Maybe Types.AttributeName)
cddtAttributeName = Lens.field @"attributeName"
{-# DEPRECATED cddtAttributeName "Use generic-lens or generic-optics with 'attributeName' instead." #-}

-- | The delivery medium (email message or phone number).
--
-- /Note:/ Consider using 'deliveryMedium' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cddtDeliveryMedium :: Lens.Lens' CodeDeliveryDetailsType (Core.Maybe Types.DeliveryMediumType)
cddtDeliveryMedium = Lens.field @"deliveryMedium"
{-# DEPRECATED cddtDeliveryMedium "Use generic-lens or generic-optics with 'deliveryMedium' instead." #-}

-- | The destination for the code delivery details.
--
-- /Note:/ Consider using 'destination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cddtDestination :: Lens.Lens' CodeDeliveryDetailsType (Core.Maybe Types.Destination)
cddtDestination = Lens.field @"destination"
{-# DEPRECATED cddtDestination "Use generic-lens or generic-optics with 'destination' instead." #-}

instance Core.FromJSON CodeDeliveryDetailsType where
  parseJSON =
    Core.withObject "CodeDeliveryDetailsType" Core.$
      \x ->
        CodeDeliveryDetailsType'
          Core.<$> (x Core..:? "AttributeName")
          Core.<*> (x Core..:? "DeliveryMedium")
          Core.<*> (x Core..:? "Destination")
