{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.ValidateMatchmakingRuleSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Validates the syntax of a matchmaking rule or rule set. This operation checks that the rule set is using syntactically correct JSON and that it conforms to allowed property expressions. To validate syntax, provide a rule set JSON string.
--
-- __Learn more__ 
--
--     * <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-rulesets.html Build a Rule Set> 
--
--
-- __Related operations__ 
--
--     * 'CreateMatchmakingConfiguration' 
--
--
--     * 'DescribeMatchmakingConfigurations' 
--
--
--     * 'UpdateMatchmakingConfiguration' 
--
--
--     * 'DeleteMatchmakingConfiguration' 
--
--
--     * 'CreateMatchmakingRuleSet' 
--
--
--     * 'DescribeMatchmakingRuleSets' 
--
--
--     * 'ValidateMatchmakingRuleSet' 
--
--
--     * 'DeleteMatchmakingRuleSet' 
--
--
module Network.AWS.GameLift.ValidateMatchmakingRuleSet
    (
    -- * Creating a request
      ValidateMatchmakingRuleSet (..)
    , mkValidateMatchmakingRuleSet
    -- ** Request lenses
    , vmrsRuleSetBody

    -- * Destructuring the response
    , ValidateMatchmakingRuleSetResponse (..)
    , mkValidateMatchmakingRuleSetResponse
    -- ** Response lenses
    , vmrsrrsValid
    , vmrsrrsResponseStatus
    ) where

import qualified Network.AWS.GameLift.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'mkValidateMatchmakingRuleSet' smart constructor.
newtype ValidateMatchmakingRuleSet = ValidateMatchmakingRuleSet'
  { ruleSetBody :: Types.RuleSetBody
    -- ^ A collection of matchmaking rules to validate, formatted as a JSON string.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ValidateMatchmakingRuleSet' value with any optional fields omitted.
mkValidateMatchmakingRuleSet
    :: Types.RuleSetBody -- ^ 'ruleSetBody'
    -> ValidateMatchmakingRuleSet
mkValidateMatchmakingRuleSet ruleSetBody
  = ValidateMatchmakingRuleSet'{ruleSetBody}

-- | A collection of matchmaking rules to validate, formatted as a JSON string.
--
-- /Note:/ Consider using 'ruleSetBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vmrsRuleSetBody :: Lens.Lens' ValidateMatchmakingRuleSet Types.RuleSetBody
vmrsRuleSetBody = Lens.field @"ruleSetBody"
{-# INLINEABLE vmrsRuleSetBody #-}
{-# DEPRECATED ruleSetBody "Use generic-lens or generic-optics with 'ruleSetBody' instead"  #-}

instance Core.ToQuery ValidateMatchmakingRuleSet where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ValidateMatchmakingRuleSet where
        toHeaders ValidateMatchmakingRuleSet{..}
          = Core.pure ("X-Amz-Target", "GameLift.ValidateMatchmakingRuleSet")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ValidateMatchmakingRuleSet where
        toJSON ValidateMatchmakingRuleSet{..}
          = Core.object
              (Core.catMaybes [Core.Just ("RuleSetBody" Core..= ruleSetBody)])

instance Core.AWSRequest ValidateMatchmakingRuleSet where
        type Rs ValidateMatchmakingRuleSet =
             ValidateMatchmakingRuleSetResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ValidateMatchmakingRuleSetResponse' Core.<$>
                   (x Core..:? "Valid") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkValidateMatchmakingRuleSetResponse' smart constructor.
data ValidateMatchmakingRuleSetResponse = ValidateMatchmakingRuleSetResponse'
  { valid :: Core.Maybe Core.Bool
    -- ^ A response indicating whether the rule set is valid.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ValidateMatchmakingRuleSetResponse' value with any optional fields omitted.
mkValidateMatchmakingRuleSetResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ValidateMatchmakingRuleSetResponse
mkValidateMatchmakingRuleSetResponse responseStatus
  = ValidateMatchmakingRuleSetResponse'{valid = Core.Nothing,
                                        responseStatus}

-- | A response indicating whether the rule set is valid.
--
-- /Note:/ Consider using 'valid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vmrsrrsValid :: Lens.Lens' ValidateMatchmakingRuleSetResponse (Core.Maybe Core.Bool)
vmrsrrsValid = Lens.field @"valid"
{-# INLINEABLE vmrsrrsValid #-}
{-# DEPRECATED valid "Use generic-lens or generic-optics with 'valid' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vmrsrrsResponseStatus :: Lens.Lens' ValidateMatchmakingRuleSetResponse Core.Int
vmrsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE vmrsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
