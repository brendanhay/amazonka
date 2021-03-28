{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.Types.IngressAccessLogs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaPackage.Types.IngressAccessLogs
  ( IngressAccessLogs (..)
  -- * Smart constructor
  , mkIngressAccessLogs
  -- * Lenses
  , ialLogGroupName
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Configure ingress access logging.
--
-- /See:/ 'mkIngressAccessLogs' smart constructor.
newtype IngressAccessLogs = IngressAccessLogs'
  { logGroupName :: Core.Maybe Core.Text
    -- ^ Customize the log group name.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'IngressAccessLogs' value with any optional fields omitted.
mkIngressAccessLogs
    :: IngressAccessLogs
mkIngressAccessLogs
  = IngressAccessLogs'{logGroupName = Core.Nothing}

-- | Customize the log group name.
--
-- /Note:/ Consider using 'logGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ialLogGroupName :: Lens.Lens' IngressAccessLogs (Core.Maybe Core.Text)
ialLogGroupName = Lens.field @"logGroupName"
{-# INLINEABLE ialLogGroupName #-}
{-# DEPRECATED logGroupName "Use generic-lens or generic-optics with 'logGroupName' instead"  #-}

instance Core.FromJSON IngressAccessLogs where
        toJSON IngressAccessLogs{..}
          = Core.object
              (Core.catMaybes [("logGroupName" Core..=) Core.<$> logGroupName])

instance Core.FromJSON IngressAccessLogs where
        parseJSON
          = Core.withObject "IngressAccessLogs" Core.$
              \ x -> IngressAccessLogs' Core.<$> (x Core..:? "logGroupName")
