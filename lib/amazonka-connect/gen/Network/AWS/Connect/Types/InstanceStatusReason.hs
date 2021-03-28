{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.InstanceStatusReason
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Connect.Types.InstanceStatusReason
  ( InstanceStatusReason (..)
  -- * Smart constructor
  , mkInstanceStatusReason
  -- * Lenses
  , isrMessage
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Relevant details why the instance was not successfully created.
--
-- /See:/ 'mkInstanceStatusReason' smart constructor.
newtype InstanceStatusReason = InstanceStatusReason'
  { message :: Core.Maybe Core.Text
    -- ^ The message.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'InstanceStatusReason' value with any optional fields omitted.
mkInstanceStatusReason
    :: InstanceStatusReason
mkInstanceStatusReason
  = InstanceStatusReason'{message = Core.Nothing}

-- | The message.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isrMessage :: Lens.Lens' InstanceStatusReason (Core.Maybe Core.Text)
isrMessage = Lens.field @"message"
{-# INLINEABLE isrMessage #-}
{-# DEPRECATED message "Use generic-lens or generic-optics with 'message' instead"  #-}

instance Core.FromJSON InstanceStatusReason where
        parseJSON
          = Core.withObject "InstanceStatusReason" Core.$
              \ x -> InstanceStatusReason' Core.<$> (x Core..:? "Message")
