{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexRuntime.Types.ActiveContext
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.LexRuntime.Types.ActiveContext
  ( ActiveContext (..)
  -- * Smart constructor
  , mkActiveContext
  -- * Lenses
  , acName
  , acTimeToLive
  , acParameters
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.LexRuntime.Types.ActiveContextName as Types
import qualified Network.AWS.LexRuntime.Types.ActiveContextTimeToLive as Types
import qualified Network.AWS.LexRuntime.Types.ParameterName as Types
import qualified Network.AWS.LexRuntime.Types.Text as Types
import qualified Network.AWS.Prelude as Core

-- | A context is a variable that contains information about the current state of the conversation between a user and Amazon Lex. Context can be set automatically by Amazon Lex when an intent is fulfilled, or it can be set at runtime using the @PutContent@ , @PutText@ , or @PutSession@ operation.
--
-- /See:/ 'mkActiveContext' smart constructor.
data ActiveContext = ActiveContext'
  { name :: Types.ActiveContextName
    -- ^ The name of the context.
  , timeToLive :: Types.ActiveContextTimeToLive
    -- ^ The length of time or number of turns that a context remains active.
  , parameters :: Core.HashMap Types.ParameterName Types.Text
    -- ^ State variables for the current context. You can use these values as default values for slots in subsequent events.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ActiveContext' value with any optional fields omitted.
mkActiveContext
    :: Types.ActiveContextName -- ^ 'name'
    -> Types.ActiveContextTimeToLive -- ^ 'timeToLive'
    -> ActiveContext
mkActiveContext name timeToLive
  = ActiveContext'{name, timeToLive, parameters = Core.mempty}

-- | The name of the context.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acName :: Lens.Lens' ActiveContext Types.ActiveContextName
acName = Lens.field @"name"
{-# INLINEABLE acName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The length of time or number of turns that a context remains active.
--
-- /Note:/ Consider using 'timeToLive' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acTimeToLive :: Lens.Lens' ActiveContext Types.ActiveContextTimeToLive
acTimeToLive = Lens.field @"timeToLive"
{-# INLINEABLE acTimeToLive #-}
{-# DEPRECATED timeToLive "Use generic-lens or generic-optics with 'timeToLive' instead"  #-}

-- | State variables for the current context. You can use these values as default values for slots in subsequent events.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acParameters :: Lens.Lens' ActiveContext (Core.HashMap Types.ParameterName Types.Text)
acParameters = Lens.field @"parameters"
{-# INLINEABLE acParameters #-}
{-# DEPRECATED parameters "Use generic-lens or generic-optics with 'parameters' instead"  #-}

instance Core.FromJSON ActiveContext where
        toJSON ActiveContext{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("name" Core..= name),
                  Core.Just ("timeToLive" Core..= timeToLive),
                  Core.Just ("parameters" Core..= parameters)])

instance Core.FromJSON ActiveContext where
        parseJSON
          = Core.withObject "ActiveContext" Core.$
              \ x ->
                ActiveContext' Core.<$>
                  (x Core..: "name") Core.<*> x Core..: "timeToLive" Core.<*>
                    x Core..:? "parameters" Core..!= Core.mempty
