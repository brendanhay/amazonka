{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Support.Types.TrustedAdvisorCheckRefreshStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Support.Types.TrustedAdvisorCheckRefreshStatus
  ( TrustedAdvisorCheckRefreshStatus (..),

    -- * Smart constructor
    mkTrustedAdvisorCheckRefreshStatus,

    -- * Lenses
    tacrsCheckId,
    tacrsStatus,
    tacrsMillisUntilNextRefreshable,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Support.Types.String as Types

-- | The refresh status of a Trusted Advisor check.
--
-- /See:/ 'mkTrustedAdvisorCheckRefreshStatus' smart constructor.
data TrustedAdvisorCheckRefreshStatus = TrustedAdvisorCheckRefreshStatus'
  { -- | The unique identifier for the Trusted Advisor check.
    checkId :: Types.String,
    -- | The status of the Trusted Advisor check for which a refresh has been requested:
    --
    --
    --     * @none:@ The check is not refreshed or the non-success status exceeds the timeout
    --
    --
    --     * @enqueued:@ The check refresh requests has entered the refresh queue
    --
    --
    --     * @processing:@ The check refresh request is picked up by the rule processing engine
    --
    --
    --     * @success:@ The check is successfully refreshed
    --
    --
    --     * @abandoned:@ The check refresh has failed
    status :: Types.String,
    -- | The amount of time, in milliseconds, until the Trusted Advisor check is eligible for refresh.
    millisUntilNextRefreshable :: Core.Integer
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TrustedAdvisorCheckRefreshStatus' value with any optional fields omitted.
mkTrustedAdvisorCheckRefreshStatus ::
  -- | 'checkId'
  Types.String ->
  -- | 'status'
  Types.String ->
  -- | 'millisUntilNextRefreshable'
  Core.Integer ->
  TrustedAdvisorCheckRefreshStatus
mkTrustedAdvisorCheckRefreshStatus
  checkId
  status
  millisUntilNextRefreshable =
    TrustedAdvisorCheckRefreshStatus'
      { checkId,
        status,
        millisUntilNextRefreshable
      }

-- | The unique identifier for the Trusted Advisor check.
--
-- /Note:/ Consider using 'checkId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tacrsCheckId :: Lens.Lens' TrustedAdvisorCheckRefreshStatus Types.String
tacrsCheckId = Lens.field @"checkId"
{-# DEPRECATED tacrsCheckId "Use generic-lens or generic-optics with 'checkId' instead." #-}

-- | The status of the Trusted Advisor check for which a refresh has been requested:
--
--
--     * @none:@ The check is not refreshed or the non-success status exceeds the timeout
--
--
--     * @enqueued:@ The check refresh requests has entered the refresh queue
--
--
--     * @processing:@ The check refresh request is picked up by the rule processing engine
--
--
--     * @success:@ The check is successfully refreshed
--
--
--     * @abandoned:@ The check refresh has failed
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tacrsStatus :: Lens.Lens' TrustedAdvisorCheckRefreshStatus Types.String
tacrsStatus = Lens.field @"status"
{-# DEPRECATED tacrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The amount of time, in milliseconds, until the Trusted Advisor check is eligible for refresh.
--
-- /Note:/ Consider using 'millisUntilNextRefreshable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tacrsMillisUntilNextRefreshable :: Lens.Lens' TrustedAdvisorCheckRefreshStatus Core.Integer
tacrsMillisUntilNextRefreshable = Lens.field @"millisUntilNextRefreshable"
{-# DEPRECATED tacrsMillisUntilNextRefreshable "Use generic-lens or generic-optics with 'millisUntilNextRefreshable' instead." #-}

instance Core.FromJSON TrustedAdvisorCheckRefreshStatus where
  parseJSON =
    Core.withObject "TrustedAdvisorCheckRefreshStatus" Core.$
      \x ->
        TrustedAdvisorCheckRefreshStatus'
          Core.<$> (x Core..: "checkId")
          Core.<*> (x Core..: "status")
          Core.<*> (x Core..: "millisUntilNextRefreshable")
