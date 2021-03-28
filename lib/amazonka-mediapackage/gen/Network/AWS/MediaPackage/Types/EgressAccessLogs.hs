{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.Types.EgressAccessLogs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaPackage.Types.EgressAccessLogs
  ( EgressAccessLogs (..)
  -- * Smart constructor
  , mkEgressAccessLogs
  -- * Lenses
  , ealLogGroupName
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Configure egress access logging.
--
-- /See:/ 'mkEgressAccessLogs' smart constructor.
newtype EgressAccessLogs = EgressAccessLogs'
  { logGroupName :: Core.Maybe Core.Text
    -- ^ Customize the log group name.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'EgressAccessLogs' value with any optional fields omitted.
mkEgressAccessLogs
    :: EgressAccessLogs
mkEgressAccessLogs = EgressAccessLogs'{logGroupName = Core.Nothing}

-- | Customize the log group name.
--
-- /Note:/ Consider using 'logGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ealLogGroupName :: Lens.Lens' EgressAccessLogs (Core.Maybe Core.Text)
ealLogGroupName = Lens.field @"logGroupName"
{-# INLINEABLE ealLogGroupName #-}
{-# DEPRECATED logGroupName "Use generic-lens or generic-optics with 'logGroupName' instead"  #-}

instance Core.FromJSON EgressAccessLogs where
        toJSON EgressAccessLogs{..}
          = Core.object
              (Core.catMaybes [("logGroupName" Core..=) Core.<$> logGroupName])

instance Core.FromJSON EgressAccessLogs where
        parseJSON
          = Core.withObject "EgressAccessLogs" Core.$
              \ x -> EgressAccessLogs' Core.<$> (x Core..:? "logGroupName")
