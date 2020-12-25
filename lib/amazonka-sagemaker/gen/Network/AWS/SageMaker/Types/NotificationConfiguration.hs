{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.NotificationConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.NotificationConfiguration
  ( NotificationConfiguration (..),

    -- * Smart constructor
    mkNotificationConfiguration,

    -- * Lenses
    ncNotificationTopicArn,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.NotificationTopicArn as Types

-- | Configures SNS notifications of available or expiring work items for work teams.
--
-- /See:/ 'mkNotificationConfiguration' smart constructor.
newtype NotificationConfiguration = NotificationConfiguration'
  { -- | The ARN for the SNS topic to which notifications should be published.
    notificationTopicArn :: Core.Maybe Types.NotificationTopicArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'NotificationConfiguration' value with any optional fields omitted.
mkNotificationConfiguration ::
  NotificationConfiguration
mkNotificationConfiguration =
  NotificationConfiguration' {notificationTopicArn = Core.Nothing}

-- | The ARN for the SNS topic to which notifications should be published.
--
-- /Note:/ Consider using 'notificationTopicArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ncNotificationTopicArn :: Lens.Lens' NotificationConfiguration (Core.Maybe Types.NotificationTopicArn)
ncNotificationTopicArn = Lens.field @"notificationTopicArn"
{-# DEPRECATED ncNotificationTopicArn "Use generic-lens or generic-optics with 'notificationTopicArn' instead." #-}

instance Core.FromJSON NotificationConfiguration where
  toJSON NotificationConfiguration {..} =
    Core.object
      ( Core.catMaybes
          [("NotificationTopicArn" Core..=) Core.<$> notificationTopicArn]
      )

instance Core.FromJSON NotificationConfiguration where
  parseJSON =
    Core.withObject "NotificationConfiguration" Core.$
      \x ->
        NotificationConfiguration'
          Core.<$> (x Core..:? "NotificationTopicArn")
