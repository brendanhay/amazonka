{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.NotificationConfigurationFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.NotificationConfigurationFilter
  ( NotificationConfigurationFilter (..)
  -- * Smart constructor
  , mkNotificationConfigurationFilter
  -- * Lenses
  , ncfKey
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types
import qualified Network.AWS.S3.Types.S3KeyFilter as Types

-- | Specifies object key name filtering rules. For information about key name filtering, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/NotificationHowTo.html Configuring Event Notifications> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- /See:/ 'mkNotificationConfigurationFilter' smart constructor.
newtype NotificationConfigurationFilter = NotificationConfigurationFilter'
  { key :: Core.Maybe Types.S3KeyFilter
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'NotificationConfigurationFilter' value with any optional fields omitted.
mkNotificationConfigurationFilter
    :: NotificationConfigurationFilter
mkNotificationConfigurationFilter
  = NotificationConfigurationFilter'{key = Core.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ncfKey :: Lens.Lens' NotificationConfigurationFilter (Core.Maybe Types.S3KeyFilter)
ncfKey = Lens.field @"key"
{-# INLINEABLE ncfKey #-}
{-# DEPRECATED key "Use generic-lens or generic-optics with 'key' instead"  #-}

instance Core.ToXML NotificationConfigurationFilter where
        toXML NotificationConfigurationFilter{..}
          = Core.maybe Core.mempty (Core.toXMLElement "S3Key") key

instance Core.FromXML NotificationConfigurationFilter where
        parseXML x
          = NotificationConfigurationFilter' Core.<$> (x Core..@? "S3Key")
