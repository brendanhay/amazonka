{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    PutInsightRule (..),
    mkPutInsightRule,

    -- ** Request lenses
    pirRuleName,
    pirRuleDefinition,
    pirRuleState,
    pirTags,

    -- * Destructuring the response
    PutInsightRuleResponse (..),
    mkPutInsightRuleResponse,

    -- ** Response lenses
    pirrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudWatch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkPutInsightRule' smart constructor.
data PutInsightRule = PutInsightRule'
  { -- | A unique name for the rule.
    ruleName :: Types.RuleName,
    -- | The definition of the rule, as a JSON object. For details on the valid syntax, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/ContributorInsights-RuleSyntax.html Contributor Insights Rule Syntax> .
    ruleDefinition :: Types.RuleDefinition,
    -- | The state of the rule. Valid values are ENABLED and DISABLED.
    ruleState :: Core.Maybe Types.RuleState,
    -- | A list of key-value pairs to associate with the Contributor Insights rule. You can associate as many as 50 tags with a rule.
    --
    -- Tags can help you organize and categorize your resources. You can also use them to scope user permissions, by granting a user permission to access or change only the resources that have certain tag values.
    -- To be able to associate tags with a rule, you must have the @cloudwatch:TagResource@ permission in addition to the @cloudwatch:PutInsightRule@ permission.
    -- If you are using this operation to update an existing Contributor Insights rule, any tags you specify in this parameter are ignored. To change the tags of an existing rule, use <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_TagResource.html TagResource> .
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutInsightRule' value with any optional fields omitted.
mkPutInsightRule ::
  -- | 'ruleName'
  Types.RuleName ->
  -- | 'ruleDefinition'
  Types.RuleDefinition ->
  PutInsightRule
mkPutInsightRule ruleName ruleDefinition =
  PutInsightRule'
    { ruleName,
      ruleDefinition,
      ruleState = Core.Nothing,
      tags = Core.Nothing
    }

-- | A unique name for the rule.
--
-- /Note:/ Consider using 'ruleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pirRuleName :: Lens.Lens' PutInsightRule Types.RuleName
pirRuleName = Lens.field @"ruleName"
{-# DEPRECATED pirRuleName "Use generic-lens or generic-optics with 'ruleName' instead." #-}

-- | The definition of the rule, as a JSON object. For details on the valid syntax, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/ContributorInsights-RuleSyntax.html Contributor Insights Rule Syntax> .
--
-- /Note:/ Consider using 'ruleDefinition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pirRuleDefinition :: Lens.Lens' PutInsightRule Types.RuleDefinition
pirRuleDefinition = Lens.field @"ruleDefinition"
{-# DEPRECATED pirRuleDefinition "Use generic-lens or generic-optics with 'ruleDefinition' instead." #-}

-- | The state of the rule. Valid values are ENABLED and DISABLED.
--
-- /Note:/ Consider using 'ruleState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pirRuleState :: Lens.Lens' PutInsightRule (Core.Maybe Types.RuleState)
pirRuleState = Lens.field @"ruleState"
{-# DEPRECATED pirRuleState "Use generic-lens or generic-optics with 'ruleState' instead." #-}

-- | A list of key-value pairs to associate with the Contributor Insights rule. You can associate as many as 50 tags with a rule.
--
-- Tags can help you organize and categorize your resources. You can also use them to scope user permissions, by granting a user permission to access or change only the resources that have certain tag values.
-- To be able to associate tags with a rule, you must have the @cloudwatch:TagResource@ permission in addition to the @cloudwatch:PutInsightRule@ permission.
-- If you are using this operation to update an existing Contributor Insights rule, any tags you specify in this parameter are ignored. To change the tags of an existing rule, use <https://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_TagResource.html TagResource> .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pirTags :: Lens.Lens' PutInsightRule (Core.Maybe [Types.Tag])
pirTags = Lens.field @"tags"
{-# DEPRECATED pirTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.AWSRequest PutInsightRule where
  type Rs PutInsightRule = PutInsightRuleResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "PutInsightRule")
                Core.<> (Core.pure ("Version", "2010-08-01"))
                Core.<> (Core.toQueryValue "RuleName" ruleName)
                Core.<> (Core.toQueryValue "RuleDefinition" ruleDefinition)
                Core.<> (Core.toQueryValue "RuleState" Core.<$> ruleState)
                Core.<> ( Core.toQueryValue
                            "Tags"
                            (Core.toQueryList "member" Core.<$> tags)
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "PutInsightRuleResult"
      ( \s h x ->
          PutInsightRuleResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkPutInsightRuleResponse' smart constructor.
newtype PutInsightRuleResponse = PutInsightRuleResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'PutInsightRuleResponse' value with any optional fields omitted.
mkPutInsightRuleResponse ::
  -- | 'responseStatus'
  Core.Int ->
  PutInsightRuleResponse
mkPutInsightRuleResponse responseStatus =
  PutInsightRuleResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pirrrsResponseStatus :: Lens.Lens' PutInsightRuleResponse Core.Int
pirrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED pirrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
