{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.PutInsightRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Contributor Insights rule. Rules evaluate log events in a CloudWatch Logs log group, enabling you to find contributor data for the log events in that log group. For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/ContributorInsights.html Using Contributor Insights to Analyze High-Cardinality Data> .
--
-- If you create a rule, delete it, and then re-create it with the same name, historical data from the first time the rule was created might not be available.
module Network.AWS.CloudWatch.PutInsightRule
    (
    -- * Creating a request
      PutInsightRule (..)
    , mkPutInsightRule
    -- ** Request lenses
    , pirRuleName
    , pirRuleDefinition
    , pirRuleState
    , pirTags

    -- * Destructuring the response
    , PutInsightRuleResponse (..)
    , mkPutInsightRuleResponse
    -- ** Response lenses
    , pirrrsResponseStatus
    ) where

import qualified Network.AWS.CloudWatch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPutInsightRule' smart constructor.
data PutInsightRule = PutInsightRule'
  { ruleName :: Types.RuleName
    -- ^ A unique name for the rule.
  , ruleDefinition :: Types.RuleDefinition
    -- ^ The definition of the rule, as a JSON object. For details on the valid syntax, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/ContributorInsights-RuleSyntax.html Contributor Insights Rule Syntax> .
  , ruleState :: Core.Maybe Types.RuleState
    -- ^ The state of the rule. Valid values are ENABLED and DISABLED.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ A list of key-value pairs to associate with the Contributor Insights rule. You can associate as many as 50 tags with a rule.
--
-- Tags can help you organize and categorize your resources. You can also use them to scope user permissions, by granting a user permission to access or change only the resources that have certain tag values.
-- To be able to associate tags with a rule, you must have the @cloudwatch:TagResource@ permission in addition to the @cloudwatch:PutInsightRule@ permission.
-- If you are using this operation to update an existing Contributor Insights rule, any tags you specify in this parameter are ignored. To change the tags of an existing rule, use <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_TagResource.html TagResource> .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutInsightRule' value with any optional fields omitted.
mkPutInsightRule
    :: Types.RuleName -- ^ 'ruleName'
    -> Types.RuleDefinition -- ^ 'ruleDefinition'
    -> PutInsightRule
mkPutInsightRule ruleName ruleDefinition
  = PutInsightRule'{ruleName, ruleDefinition,
                    ruleState = Core.Nothing, tags = Core.Nothing}

-- | A unique name for the rule.
--
-- /Note:/ Consider using 'ruleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pirRuleName :: Lens.Lens' PutInsightRule Types.RuleName
pirRuleName = Lens.field @"ruleName"
{-# INLINEABLE pirRuleName #-}
{-# DEPRECATED ruleName "Use generic-lens or generic-optics with 'ruleName' instead"  #-}

-- | The definition of the rule, as a JSON object. For details on the valid syntax, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/ContributorInsights-RuleSyntax.html Contributor Insights Rule Syntax> .
--
-- /Note:/ Consider using 'ruleDefinition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pirRuleDefinition :: Lens.Lens' PutInsightRule Types.RuleDefinition
pirRuleDefinition = Lens.field @"ruleDefinition"
{-# INLINEABLE pirRuleDefinition #-}
{-# DEPRECATED ruleDefinition "Use generic-lens or generic-optics with 'ruleDefinition' instead"  #-}

-- | The state of the rule. Valid values are ENABLED and DISABLED.
--
-- /Note:/ Consider using 'ruleState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pirRuleState :: Lens.Lens' PutInsightRule (Core.Maybe Types.RuleState)
pirRuleState = Lens.field @"ruleState"
{-# INLINEABLE pirRuleState #-}
{-# DEPRECATED ruleState "Use generic-lens or generic-optics with 'ruleState' instead"  #-}

-- | A list of key-value pairs to associate with the Contributor Insights rule. You can associate as many as 50 tags with a rule.
--
-- Tags can help you organize and categorize your resources. You can also use them to scope user permissions, by granting a user permission to access or change only the resources that have certain tag values.
-- To be able to associate tags with a rule, you must have the @cloudwatch:TagResource@ permission in addition to the @cloudwatch:PutInsightRule@ permission.
-- If you are using this operation to update an existing Contributor Insights rule, any tags you specify in this parameter are ignored. To change the tags of an existing rule, use <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_TagResource.html TagResource> .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pirTags :: Lens.Lens' PutInsightRule (Core.Maybe [Types.Tag])
pirTags = Lens.field @"tags"
{-# INLINEABLE pirTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery PutInsightRule where
        toQuery PutInsightRule{..}
          = Core.toQueryPair "Action" ("PutInsightRule" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2010-08-01" :: Core.Text)
              Core.<> Core.toQueryPair "RuleName" ruleName
              Core.<> Core.toQueryPair "RuleDefinition" ruleDefinition
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "RuleState") ruleState
              Core.<>
              Core.toQueryPair "Tags"
                (Core.maybe Core.mempty (Core.toQueryList "member") tags)

instance Core.ToHeaders PutInsightRule where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest PutInsightRule where
        type Rs PutInsightRule = PutInsightRuleResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "PutInsightRuleResult"
              (\ s h x ->
                 PutInsightRuleResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkPutInsightRuleResponse' smart constructor.
newtype PutInsightRuleResponse = PutInsightRuleResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'PutInsightRuleResponse' value with any optional fields omitted.
mkPutInsightRuleResponse
    :: Core.Int -- ^ 'responseStatus'
    -> PutInsightRuleResponse
mkPutInsightRuleResponse responseStatus
  = PutInsightRuleResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pirrrsResponseStatus :: Lens.Lens' PutInsightRuleResponse Core.Int
pirrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE pirrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
