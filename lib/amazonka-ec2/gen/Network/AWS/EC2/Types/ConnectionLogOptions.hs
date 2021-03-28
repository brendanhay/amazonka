{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ConnectionLogOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.ConnectionLogOptions
  ( ConnectionLogOptions (..)
  -- * Smart constructor
  , mkConnectionLogOptions
  -- * Lenses
  , cloCloudwatchLogGroup
  , cloCloudwatchLogStream
  , cloEnabled
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the client connection logging options for the Client VPN endpoint.
--
-- /See:/ 'mkConnectionLogOptions' smart constructor.
data ConnectionLogOptions = ConnectionLogOptions'
  { cloudwatchLogGroup :: Core.Maybe Core.Text
    -- ^ The name of the CloudWatch Logs log group. Required if connection logging is enabled.
  , cloudwatchLogStream :: Core.Maybe Core.Text
    -- ^ The name of the CloudWatch Logs log stream to which the connection data is published.
  , enabled :: Core.Maybe Core.Bool
    -- ^ Indicates whether connection logging is enabled.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ConnectionLogOptions' value with any optional fields omitted.
mkConnectionLogOptions
    :: ConnectionLogOptions
mkConnectionLogOptions
  = ConnectionLogOptions'{cloudwatchLogGroup = Core.Nothing,
                          cloudwatchLogStream = Core.Nothing, enabled = Core.Nothing}

-- | The name of the CloudWatch Logs log group. Required if connection logging is enabled.
--
-- /Note:/ Consider using 'cloudwatchLogGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cloCloudwatchLogGroup :: Lens.Lens' ConnectionLogOptions (Core.Maybe Core.Text)
cloCloudwatchLogGroup = Lens.field @"cloudwatchLogGroup"
{-# INLINEABLE cloCloudwatchLogGroup #-}
{-# DEPRECATED cloudwatchLogGroup "Use generic-lens or generic-optics with 'cloudwatchLogGroup' instead"  #-}

-- | The name of the CloudWatch Logs log stream to which the connection data is published.
--
-- /Note:/ Consider using 'cloudwatchLogStream' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cloCloudwatchLogStream :: Lens.Lens' ConnectionLogOptions (Core.Maybe Core.Text)
cloCloudwatchLogStream = Lens.field @"cloudwatchLogStream"
{-# INLINEABLE cloCloudwatchLogStream #-}
{-# DEPRECATED cloudwatchLogStream "Use generic-lens or generic-optics with 'cloudwatchLogStream' instead"  #-}

-- | Indicates whether connection logging is enabled.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cloEnabled :: Lens.Lens' ConnectionLogOptions (Core.Maybe Core.Bool)
cloEnabled = Lens.field @"enabled"
{-# INLINEABLE cloEnabled #-}
{-# DEPRECATED enabled "Use generic-lens or generic-optics with 'enabled' instead"  #-}

instance Core.ToQuery ConnectionLogOptions where
        toQuery ConnectionLogOptions{..}
          = Core.maybe Core.mempty (Core.toQueryPair "CloudwatchLogGroup")
              cloudwatchLogGroup
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "CloudwatchLogStream")
                cloudwatchLogStream
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Enabled") enabled
