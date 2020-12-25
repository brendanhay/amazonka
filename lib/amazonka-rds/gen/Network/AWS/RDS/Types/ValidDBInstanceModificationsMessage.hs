{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.ValidDBInstanceModificationsMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.ValidDBInstanceModificationsMessage
  ( ValidDBInstanceModificationsMessage (..),

    -- * Smart constructor
    mkValidDBInstanceModificationsMessage,

    -- * Lenses
    vdbimmStorage,
    vdbimmValidProcessorFeatures,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types.AvailableProcessorFeature as Types
import qualified Network.AWS.RDS.Types.ValidStorageOptions as Types

-- | Information about valid modifications that you can make to your DB instance. Contains the result of a successful call to the @DescribeValidDBInstanceModifications@ action. You can use this information when you call @ModifyDBInstance@ .
--
-- /See:/ 'mkValidDBInstanceModificationsMessage' smart constructor.
data ValidDBInstanceModificationsMessage = ValidDBInstanceModificationsMessage'
  { -- | Valid storage options for your DB instance.
    storage :: Core.Maybe [Types.ValidStorageOptions],
    -- | Valid processor features for your DB instance.
    validProcessorFeatures :: Core.Maybe [Types.AvailableProcessorFeature]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ValidDBInstanceModificationsMessage' value with any optional fields omitted.
mkValidDBInstanceModificationsMessage ::
  ValidDBInstanceModificationsMessage
mkValidDBInstanceModificationsMessage =
  ValidDBInstanceModificationsMessage'
    { storage = Core.Nothing,
      validProcessorFeatures = Core.Nothing
    }

-- | Valid storage options for your DB instance.
--
-- /Note:/ Consider using 'storage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vdbimmStorage :: Lens.Lens' ValidDBInstanceModificationsMessage (Core.Maybe [Types.ValidStorageOptions])
vdbimmStorage = Lens.field @"storage"
{-# DEPRECATED vdbimmStorage "Use generic-lens or generic-optics with 'storage' instead." #-}

-- | Valid processor features for your DB instance.
--
-- /Note:/ Consider using 'validProcessorFeatures' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vdbimmValidProcessorFeatures :: Lens.Lens' ValidDBInstanceModificationsMessage (Core.Maybe [Types.AvailableProcessorFeature])
vdbimmValidProcessorFeatures = Lens.field @"validProcessorFeatures"
{-# DEPRECATED vdbimmValidProcessorFeatures "Use generic-lens or generic-optics with 'validProcessorFeatures' instead." #-}

instance Core.FromXML ValidDBInstanceModificationsMessage where
  parseXML x =
    ValidDBInstanceModificationsMessage'
      Core.<$> ( x Core..@? "Storage"
                   Core..<@> Core.parseXMLList "ValidStorageOptions"
               )
      Core.<*> ( x Core..@? "ValidProcessorFeatures"
                   Core..<@> Core.parseXMLList "AvailableProcessorFeature"
               )
