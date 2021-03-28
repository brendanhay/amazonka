{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.SessionFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.SessionFilter
  ( SessionFilter (..)
  -- * Smart constructor
  , mkSessionFilter
  -- * Lenses
  , sfKey
  , sfValue
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.SessionFilterKey as Types
import qualified Network.AWS.SSM.Types.Value as Types

-- | Describes a filter for Session Manager information.
--
-- /See:/ 'mkSessionFilter' smart constructor.
data SessionFilter = SessionFilter'
  { key :: Types.SessionFilterKey
    -- ^ The name of the filter.
  , value :: Types.Value
    -- ^ The filter value. Valid values for each filter key are as follows:
--
--
--     * InvokedAfter: Specify a timestamp to limit your results. For example, specify 2018-08-29T00:00:00Z to see sessions that started August 29, 2018, and later.
--
--
--     * InvokedBefore: Specify a timestamp to limit your results. For example, specify 2018-08-29T00:00:00Z to see sessions that started before August 29, 2018.
--
--
--     * Target: Specify an instance to which session connections have been made.
--
--
--     * Owner: Specify an AWS user account to see a list of sessions started by that user.
--
--
--     * Status: Specify a valid session status to see a list of all sessions with that status. Status values you can specify include:
--
--     * Connected
--
--
--     * Connecting
--
--
--     * Disconnected
--
--
--     * Terminated
--
--
--     * Terminating
--
--
--     * Failed
--
--
--
--
--     * SessionId: Specify a session ID to return details about the session.
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SessionFilter' value with any optional fields omitted.
mkSessionFilter
    :: Types.SessionFilterKey -- ^ 'key'
    -> Types.Value -- ^ 'value'
    -> SessionFilter
mkSessionFilter key value = SessionFilter'{key, value}

-- | The name of the filter.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfKey :: Lens.Lens' SessionFilter Types.SessionFilterKey
sfKey = Lens.field @"key"
{-# INLINEABLE sfKey #-}
{-# DEPRECATED key "Use generic-lens or generic-optics with 'key' instead"  #-}

-- | The filter value. Valid values for each filter key are as follows:
--
--
--     * InvokedAfter: Specify a timestamp to limit your results. For example, specify 2018-08-29T00:00:00Z to see sessions that started August 29, 2018, and later.
--
--
--     * InvokedBefore: Specify a timestamp to limit your results. For example, specify 2018-08-29T00:00:00Z to see sessions that started before August 29, 2018.
--
--
--     * Target: Specify an instance to which session connections have been made.
--
--
--     * Owner: Specify an AWS user account to see a list of sessions started by that user.
--
--
--     * Status: Specify a valid session status to see a list of all sessions with that status. Status values you can specify include:
--
--     * Connected
--
--
--     * Connecting
--
--
--     * Disconnected
--
--
--     * Terminated
--
--
--     * Terminating
--
--
--     * Failed
--
--
--
--
--     * SessionId: Specify a session ID to return details about the session.
--
--
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfValue :: Lens.Lens' SessionFilter Types.Value
sfValue = Lens.field @"value"
{-# INLINEABLE sfValue #-}
{-# DEPRECATED value "Use generic-lens or generic-optics with 'value' instead"  #-}

instance Core.FromJSON SessionFilter where
        toJSON SessionFilter{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("key" Core..= key), Core.Just ("value" Core..= value)])
