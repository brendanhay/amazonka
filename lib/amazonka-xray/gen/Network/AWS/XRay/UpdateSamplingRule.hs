{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.UpdateSamplingRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies a sampling rule's configuration.
module Network.AWS.XRay.UpdateSamplingRule
  ( -- * Creating a request
    UpdateSamplingRule (..),
    mkUpdateSamplingRule,

    -- ** Request lenses
    usrSamplingRuleUpdate,

    -- * Destructuring the response
    UpdateSamplingRuleResponse (..),
    mkUpdateSamplingRuleResponse,

    -- ** Response lenses
    usrrrsSamplingRuleRecord,
    usrrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.XRay.Types as Types

-- | /See:/ 'mkUpdateSamplingRule' smart constructor.
newtype UpdateSamplingRule = UpdateSamplingRule'
  { -- | The rule and fields to change.
    samplingRuleUpdate :: Types.SamplingRuleUpdate
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateSamplingRule' value with any optional fields omitted.
mkUpdateSamplingRule ::
  -- | 'samplingRuleUpdate'
  Types.SamplingRuleUpdate ->
  UpdateSamplingRule
mkUpdateSamplingRule samplingRuleUpdate =
  UpdateSamplingRule' {samplingRuleUpdate}

-- | The rule and fields to change.
--
-- /Note:/ Consider using 'samplingRuleUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usrSamplingRuleUpdate :: Lens.Lens' UpdateSamplingRule Types.SamplingRuleUpdate
usrSamplingRuleUpdate = Lens.field @"samplingRuleUpdate"
{-# DEPRECATED usrSamplingRuleUpdate "Use generic-lens or generic-optics with 'samplingRuleUpdate' instead." #-}

instance Core.FromJSON UpdateSamplingRule where
  toJSON UpdateSamplingRule {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("SamplingRuleUpdate" Core..= samplingRuleUpdate)]
      )

instance Core.AWSRequest UpdateSamplingRule where
  type Rs UpdateSamplingRule = UpdateSamplingRuleResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/UpdateSamplingRule",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateSamplingRuleResponse'
            Core.<$> (x Core..:? "SamplingRuleRecord")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateSamplingRuleResponse' smart constructor.
data UpdateSamplingRuleResponse = UpdateSamplingRuleResponse'
  { -- | The updated rule definition and metadata.
    samplingRuleRecord :: Core.Maybe Types.SamplingRuleRecord,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'UpdateSamplingRuleResponse' value with any optional fields omitted.
mkUpdateSamplingRuleResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateSamplingRuleResponse
mkUpdateSamplingRuleResponse responseStatus =
  UpdateSamplingRuleResponse'
    { samplingRuleRecord = Core.Nothing,
      responseStatus
    }

-- | The updated rule definition and metadata.
--
-- /Note:/ Consider using 'samplingRuleRecord' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usrrrsSamplingRuleRecord :: Lens.Lens' UpdateSamplingRuleResponse (Core.Maybe Types.SamplingRuleRecord)
usrrrsSamplingRuleRecord = Lens.field @"samplingRuleRecord"
{-# DEPRECATED usrrrsSamplingRuleRecord "Use generic-lens or generic-optics with 'samplingRuleRecord' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usrrrsResponseStatus :: Lens.Lens' UpdateSamplingRuleResponse Core.Int
usrrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED usrrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
