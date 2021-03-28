{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.RuleCondition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ELBv2.Types.RuleCondition
  ( RuleCondition (..)
  -- * Smart constructor
  , mkRuleCondition
  -- * Lenses
  , rcField
  , rcHostHeaderConfig
  , rcHttpHeaderConfig
  , rcHttpRequestMethodConfig
  , rcPathPatternConfig
  , rcQueryStringConfig
  , rcSourceIpConfig
  , rcValues
  ) where

import qualified Network.AWS.ELBv2.Types.Field as Types
import qualified Network.AWS.ELBv2.Types.HostHeaderConditionConfig as Types
import qualified Network.AWS.ELBv2.Types.HttpHeaderConditionConfig as Types
import qualified Network.AWS.ELBv2.Types.HttpRequestMethodConditionConfig as Types
import qualified Network.AWS.ELBv2.Types.PathPatternConditionConfig as Types
import qualified Network.AWS.ELBv2.Types.QueryStringConditionConfig as Types
import qualified Network.AWS.ELBv2.Types.SourceIpConditionConfig as Types
import qualified Network.AWS.ELBv2.Types.StringValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a condition for a rule.
--
-- Each rule can optionally include up to one of each of the following conditions: @http-request-method@ , @host-header@ , @path-pattern@ , and @source-ip@ . Each rule can also optionally include one or more of each of the following conditions: @http-header@ and @query-string@ .
--
-- /See:/ 'mkRuleCondition' smart constructor.
data RuleCondition = RuleCondition'
  { field :: Core.Maybe Types.Field
    -- ^ The field in the HTTP request. The following are the possible values:
--
--
--     * @http-header@ 
--
--
--     * @http-request-method@ 
--
--
--     * @host-header@ 
--
--
--     * @path-pattern@ 
--
--
--     * @query-string@ 
--
--
--     * @source-ip@ 
--
--
  , hostHeaderConfig :: Core.Maybe Types.HostHeaderConditionConfig
    -- ^ Information for a host header condition. Specify only when @Field@ is @host-header@ .
  , httpHeaderConfig :: Core.Maybe Types.HttpHeaderConditionConfig
    -- ^ Information for an HTTP header condition. Specify only when @Field@ is @http-header@ .
  , httpRequestMethodConfig :: Core.Maybe Types.HttpRequestMethodConditionConfig
    -- ^ Information for an HTTP method condition. Specify only when @Field@ is @http-request-method@ .
  , pathPatternConfig :: Core.Maybe Types.PathPatternConditionConfig
    -- ^ Information for a path pattern condition. Specify only when @Field@ is @path-pattern@ .
  , queryStringConfig :: Core.Maybe Types.QueryStringConditionConfig
    -- ^ Information for a query string condition. Specify only when @Field@ is @query-string@ .
  , sourceIpConfig :: Core.Maybe Types.SourceIpConditionConfig
    -- ^ Information for a source IP condition. Specify only when @Field@ is @source-ip@ .
  , values :: Core.Maybe [Types.StringValue]
    -- ^ The condition value. Specify only when @Field@ is @host-header@ or @path-pattern@ . Alternatively, to specify multiple host names or multiple path patterns, use @HostHeaderConfig@ or @PathPatternConfig@ .
--
-- If @Field@ is @host-header@ and you are not using @HostHeaderConfig@ , you can specify a single host name (for example, my.example.com) in @Values@ . A host name is case insensitive, can be up to 128 characters in length, and can contain any of the following characters.
--
--     * A-Z, a-z, 0-9
--
--
--     * - .
--
--
--     * * (matches 0 or more characters)
--
--
--     * ? (matches exactly 1 character)
--
--
-- If @Field@ is @path-pattern@ and you are not using @PathPatternConfig@ , you can specify a single path pattern (for example, /img/*) in @Values@ . A path pattern is case-sensitive, can be up to 128 characters in length, and can contain any of the following characters.
--
--     * A-Z, a-z, 0-9
--
--
--     * _ - . $ / ~ " ' @ : +
--
--
--     * & (using &amp;)
--
--
--     * * (matches 0 or more characters)
--
--
--     * ? (matches exactly 1 character)
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RuleCondition' value with any optional fields omitted.
mkRuleCondition
    :: RuleCondition
mkRuleCondition
  = RuleCondition'{field = Core.Nothing,
                   hostHeaderConfig = Core.Nothing, httpHeaderConfig = Core.Nothing,
                   httpRequestMethodConfig = Core.Nothing,
                   pathPatternConfig = Core.Nothing, queryStringConfig = Core.Nothing,
                   sourceIpConfig = Core.Nothing, values = Core.Nothing}

-- | The field in the HTTP request. The following are the possible values:
--
--
--     * @http-header@ 
--
--
--     * @http-request-method@ 
--
--
--     * @host-header@ 
--
--
--     * @path-pattern@ 
--
--
--     * @query-string@ 
--
--
--     * @source-ip@ 
--
--
--
-- /Note:/ Consider using 'field' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcField :: Lens.Lens' RuleCondition (Core.Maybe Types.Field)
rcField = Lens.field @"field"
{-# INLINEABLE rcField #-}
{-# DEPRECATED field "Use generic-lens or generic-optics with 'field' instead"  #-}

-- | Information for a host header condition. Specify only when @Field@ is @host-header@ .
--
-- /Note:/ Consider using 'hostHeaderConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcHostHeaderConfig :: Lens.Lens' RuleCondition (Core.Maybe Types.HostHeaderConditionConfig)
rcHostHeaderConfig = Lens.field @"hostHeaderConfig"
{-# INLINEABLE rcHostHeaderConfig #-}
{-# DEPRECATED hostHeaderConfig "Use generic-lens or generic-optics with 'hostHeaderConfig' instead"  #-}

-- | Information for an HTTP header condition. Specify only when @Field@ is @http-header@ .
--
-- /Note:/ Consider using 'httpHeaderConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcHttpHeaderConfig :: Lens.Lens' RuleCondition (Core.Maybe Types.HttpHeaderConditionConfig)
rcHttpHeaderConfig = Lens.field @"httpHeaderConfig"
{-# INLINEABLE rcHttpHeaderConfig #-}
{-# DEPRECATED httpHeaderConfig "Use generic-lens or generic-optics with 'httpHeaderConfig' instead"  #-}

-- | Information for an HTTP method condition. Specify only when @Field@ is @http-request-method@ .
--
-- /Note:/ Consider using 'httpRequestMethodConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcHttpRequestMethodConfig :: Lens.Lens' RuleCondition (Core.Maybe Types.HttpRequestMethodConditionConfig)
rcHttpRequestMethodConfig = Lens.field @"httpRequestMethodConfig"
{-# INLINEABLE rcHttpRequestMethodConfig #-}
{-# DEPRECATED httpRequestMethodConfig "Use generic-lens or generic-optics with 'httpRequestMethodConfig' instead"  #-}

-- | Information for a path pattern condition. Specify only when @Field@ is @path-pattern@ .
--
-- /Note:/ Consider using 'pathPatternConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcPathPatternConfig :: Lens.Lens' RuleCondition (Core.Maybe Types.PathPatternConditionConfig)
rcPathPatternConfig = Lens.field @"pathPatternConfig"
{-# INLINEABLE rcPathPatternConfig #-}
{-# DEPRECATED pathPatternConfig "Use generic-lens or generic-optics with 'pathPatternConfig' instead"  #-}

-- | Information for a query string condition. Specify only when @Field@ is @query-string@ .
--
-- /Note:/ Consider using 'queryStringConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcQueryStringConfig :: Lens.Lens' RuleCondition (Core.Maybe Types.QueryStringConditionConfig)
rcQueryStringConfig = Lens.field @"queryStringConfig"
{-# INLINEABLE rcQueryStringConfig #-}
{-# DEPRECATED queryStringConfig "Use generic-lens or generic-optics with 'queryStringConfig' instead"  #-}

-- | Information for a source IP condition. Specify only when @Field@ is @source-ip@ .
--
-- /Note:/ Consider using 'sourceIpConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcSourceIpConfig :: Lens.Lens' RuleCondition (Core.Maybe Types.SourceIpConditionConfig)
rcSourceIpConfig = Lens.field @"sourceIpConfig"
{-# INLINEABLE rcSourceIpConfig #-}
{-# DEPRECATED sourceIpConfig "Use generic-lens or generic-optics with 'sourceIpConfig' instead"  #-}

-- | The condition value. Specify only when @Field@ is @host-header@ or @path-pattern@ . Alternatively, to specify multiple host names or multiple path patterns, use @HostHeaderConfig@ or @PathPatternConfig@ .
--
-- If @Field@ is @host-header@ and you are not using @HostHeaderConfig@ , you can specify a single host name (for example, my.example.com) in @Values@ . A host name is case insensitive, can be up to 128 characters in length, and can contain any of the following characters.
--
--     * A-Z, a-z, 0-9
--
--
--     * - .
--
--
--     * * (matches 0 or more characters)
--
--
--     * ? (matches exactly 1 character)
--
--
-- If @Field@ is @path-pattern@ and you are not using @PathPatternConfig@ , you can specify a single path pattern (for example, /img/*) in @Values@ . A path pattern is case-sensitive, can be up to 128 characters in length, and can contain any of the following characters.
--
--     * A-Z, a-z, 0-9
--
--
--     * _ - . $ / ~ " ' @ : +
--
--
--     * & (using &amp;)
--
--
--     * * (matches 0 or more characters)
--
--
--     * ? (matches exactly 1 character)
--
--
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcValues :: Lens.Lens' RuleCondition (Core.Maybe [Types.StringValue])
rcValues = Lens.field @"values"
{-# INLINEABLE rcValues #-}
{-# DEPRECATED values "Use generic-lens or generic-optics with 'values' instead"  #-}

instance Core.ToQuery RuleCondition where
        toQuery RuleCondition{..}
          = Core.maybe Core.mempty (Core.toQueryPair "Field") field Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "HostHeaderConfig")
                hostHeaderConfig
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "HttpHeaderConfig")
                httpHeaderConfig
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "HttpRequestMethodConfig")
                httpRequestMethodConfig
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "PathPatternConfig")
                pathPatternConfig
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "QueryStringConfig")
                queryStringConfig
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "SourceIpConfig")
                sourceIpConfig
              Core.<>
              Core.toQueryPair "Values"
                (Core.maybe Core.mempty (Core.toQueryList "member") values)

instance Core.FromXML RuleCondition where
        parseXML x
          = RuleCondition' Core.<$>
              (x Core..@? "Field") Core.<*> x Core..@? "HostHeaderConfig"
                Core.<*> x Core..@? "HttpHeaderConfig"
                Core.<*> x Core..@? "HttpRequestMethodConfig"
                Core.<*> x Core..@? "PathPatternConfig"
                Core.<*> x Core..@? "QueryStringConfig"
                Core.<*> x Core..@? "SourceIpConfig"
                Core.<*> x Core..@? "Values" Core..<@> Core.parseXMLList "member"
