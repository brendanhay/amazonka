{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ConnectionLogResponseOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ConnectionLogResponseOptions
  ( ConnectionLogResponseOptions (..),

    -- * Smart constructor
    mkConnectionLogResponseOptions,

    -- * Lenses
    clroCloudwatchLogGroup,
    clroCloudwatchLogStream,
    clroEnabled,
  )
where

import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about the client connection logging options for a Client VPN endpoint.
--
-- /See:/ 'mkConnectionLogResponseOptions' smart constructor.
data ConnectionLogResponseOptions = ConnectionLogResponseOptions'
  { -- | The name of the Amazon CloudWatch Logs log group to which connection logging data is published.
    cloudwatchLogGroup :: Core.Maybe Types.String,
    -- | The name of the Amazon CloudWatch Logs log stream to which connection logging data is published.
    cloudwatchLogStream :: Core.Maybe Types.String,
    -- | Indicates whether client connection logging is enabled for the Client VPN endpoint.
    enabled :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ConnectionLogResponseOptions' value with any optional fields omitted.
mkConnectionLogResponseOptions ::
  ConnectionLogResponseOptions
mkConnectionLogResponseOptions =
  ConnectionLogResponseOptions'
    { cloudwatchLogGroup = Core.Nothing,
      cloudwatchLogStream = Core.Nothing,
      enabled = Core.Nothing
    }

-- | The name of the Amazon CloudWatch Logs log group to which connection logging data is published.
--
-- /Note:/ Consider using 'cloudwatchLogGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clroCloudwatchLogGroup :: Lens.Lens' ConnectionLogResponseOptions (Core.Maybe Types.String)
clroCloudwatchLogGroup = Lens.field @"cloudwatchLogGroup"
{-# DEPRECATED clroCloudwatchLogGroup "Use generic-lens or generic-optics with 'cloudwatchLogGroup' instead." #-}

-- | The name of the Amazon CloudWatch Logs log stream to which connection logging data is published.
--
-- /Note:/ Consider using 'cloudwatchLogStream' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clroCloudwatchLogStream :: Lens.Lens' ConnectionLogResponseOptions (Core.Maybe Types.String)
clroCloudwatchLogStream = Lens.field @"cloudwatchLogStream"
{-# DEPRECATED clroCloudwatchLogStream "Use generic-lens or generic-optics with 'cloudwatchLogStream' instead." #-}

-- | Indicates whether client connection logging is enabled for the Client VPN endpoint.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clroEnabled :: Lens.Lens' ConnectionLogResponseOptions (Core.Maybe Core.Bool)
clroEnabled = Lens.field @"enabled"
{-# DEPRECATED clroEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

instance Core.FromXML ConnectionLogResponseOptions where
  parseXML x =
    ConnectionLogResponseOptions'
      Core.<$> (x Core..@? "CloudwatchLogGroup")
      Core.<*> (x Core..@? "CloudwatchLogStream")
      Core.<*> (x Core..@? "Enabled")
