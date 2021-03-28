{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.CreateSamplingRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a rule to control sampling behavior for instrumented applications. Services retrieve rules with 'GetSamplingRules' , and evaluate each rule in ascending order of /priority/ for each request. If a rule matches, the service records a trace, borrowing it from the reservoir size. After 10 seconds, the service reports back to X-Ray with 'GetSamplingTargets' to get updated versions of each in-use rule. The updated rule contains a trace quota that the service can use instead of borrowing from the reservoir.
module Network.AWS.XRay.CreateSamplingRule
    (
    -- * Creating a request
      CreateSamplingRule (..)
    , mkCreateSamplingRule
    -- ** Request lenses
    , csrSamplingRule
    , csrTags

    -- * Destructuring the response
    , CreateSamplingRuleResponse (..)
    , mkCreateSamplingRuleResponse
    -- ** Response lenses
    , csrrrsSamplingRuleRecord
    , csrrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.XRay.Types as Types

-- | /See:/ 'mkCreateSamplingRule' smart constructor.
data CreateSamplingRule = CreateSamplingRule'
  { samplingRule :: Types.SamplingRule
    -- ^ The rule definition.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ A map that contains one or more tag keys and tag values to attach to an X-Ray sampling rule. For more information about ways to use tags, see <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS resources> in the /AWS General Reference/ .
--
-- The following restrictions apply to tags:
--
--     * Maximum number of user-applied tags per resource: 50
--
--
--     * Maximum tag key length: 128 Unicode characters
--
--
--     * Maximum tag value length: 256 Unicode characters
--
--
--     * Valid values for key and value: a-z, A-Z, 0-9, space, and the following characters: _ . : / = + - and @
--
--
--     * Tag keys and values are case sensitive.
--
--
--     * Don't use @aws:@ as a prefix for keys; it's reserved for AWS use.
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateSamplingRule' value with any optional fields omitted.
mkCreateSamplingRule
    :: Types.SamplingRule -- ^ 'samplingRule'
    -> CreateSamplingRule
mkCreateSamplingRule samplingRule
  = CreateSamplingRule'{samplingRule, tags = Core.Nothing}

-- | The rule definition.
--
-- /Note:/ Consider using 'samplingRule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrSamplingRule :: Lens.Lens' CreateSamplingRule Types.SamplingRule
csrSamplingRule = Lens.field @"samplingRule"
{-# INLINEABLE csrSamplingRule #-}
{-# DEPRECATED samplingRule "Use generic-lens or generic-optics with 'samplingRule' instead"  #-}

-- | A map that contains one or more tag keys and tag values to attach to an X-Ray sampling rule. For more information about ways to use tags, see <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS resources> in the /AWS General Reference/ .
--
-- The following restrictions apply to tags:
--
--     * Maximum number of user-applied tags per resource: 50
--
--
--     * Maximum tag key length: 128 Unicode characters
--
--
--     * Maximum tag value length: 256 Unicode characters
--
--
--     * Valid values for key and value: a-z, A-Z, 0-9, space, and the following characters: _ . : / = + - and @
--
--
--     * Tag keys and values are case sensitive.
--
--
--     * Don't use @aws:@ as a prefix for keys; it's reserved for AWS use.
--
--
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrTags :: Lens.Lens' CreateSamplingRule (Core.Maybe [Types.Tag])
csrTags = Lens.field @"tags"
{-# INLINEABLE csrTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateSamplingRule where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateSamplingRule where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON CreateSamplingRule where
        toJSON CreateSamplingRule{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("SamplingRule" Core..= samplingRule),
                  ("Tags" Core..=) Core.<$> tags])

instance Core.AWSRequest CreateSamplingRule where
        type Rs CreateSamplingRule = CreateSamplingRuleResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath = "/CreateSamplingRule",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateSamplingRuleResponse' Core.<$>
                   (x Core..:? "SamplingRuleRecord") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateSamplingRuleResponse' smart constructor.
data CreateSamplingRuleResponse = CreateSamplingRuleResponse'
  { samplingRuleRecord :: Core.Maybe Types.SamplingRuleRecord
    -- ^ The saved rule definition and metadata.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreateSamplingRuleResponse' value with any optional fields omitted.
mkCreateSamplingRuleResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateSamplingRuleResponse
mkCreateSamplingRuleResponse responseStatus
  = CreateSamplingRuleResponse'{samplingRuleRecord = Core.Nothing,
                                responseStatus}

-- | The saved rule definition and metadata.
--
-- /Note:/ Consider using 'samplingRuleRecord' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrrrsSamplingRuleRecord :: Lens.Lens' CreateSamplingRuleResponse (Core.Maybe Types.SamplingRuleRecord)
csrrrsSamplingRuleRecord = Lens.field @"samplingRuleRecord"
{-# INLINEABLE csrrrsSamplingRuleRecord #-}
{-# DEPRECATED samplingRuleRecord "Use generic-lens or generic-optics with 'samplingRuleRecord' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrrrsResponseStatus :: Lens.Lens' CreateSamplingRuleResponse Core.Int
csrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE csrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
