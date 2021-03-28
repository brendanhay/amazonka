{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.RootCauseException
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.XRay.Types.RootCauseException
  ( RootCauseException (..)
  -- * Smart constructor
  , mkRootCauseException
  -- * Lenses
  , rceMessage
  , rceName
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The exception associated with a root cause.
--
-- /See:/ 'mkRootCauseException' smart constructor.
data RootCauseException = RootCauseException'
  { message :: Core.Maybe Core.Text
    -- ^ The message of the exception.
  , name :: Core.Maybe Core.Text
    -- ^ The name of the exception.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RootCauseException' value with any optional fields omitted.
mkRootCauseException
    :: RootCauseException
mkRootCauseException
  = RootCauseException'{message = Core.Nothing, name = Core.Nothing}

-- | The message of the exception.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rceMessage :: Lens.Lens' RootCauseException (Core.Maybe Core.Text)
rceMessage = Lens.field @"message"
{-# INLINEABLE rceMessage #-}
{-# DEPRECATED message "Use generic-lens or generic-optics with 'message' instead"  #-}

-- | The name of the exception.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rceName :: Lens.Lens' RootCauseException (Core.Maybe Core.Text)
rceName = Lens.field @"name"
{-# INLINEABLE rceName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.FromJSON RootCauseException where
        parseJSON
          = Core.withObject "RootCauseException" Core.$
              \ x ->
                RootCauseException' Core.<$>
                  (x Core..:? "Message") Core.<*> x Core..:? "Name"
