{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.RuleCondition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.RuleCondition
  ( RuleCondition (..),

    -- * Smart constructor
    mkRuleCondition,

    -- * Lenses
    rcField,
    rcHTTPHeaderConfig,
    rcHostHeaderConfig,
    rcValues,
    rcSourceIPConfig,
    rcHTTPRequestMethodConfig,
    rcPathPatternConfig,
    rcQueryStringConfig,
  )
where

import Network.AWS.ELBv2.Types.HTTPHeaderConditionConfig
import Network.AWS.ELBv2.Types.HTTPRequestMethodConditionConfig
import Network.AWS.ELBv2.Types.HostHeaderConditionConfig
import Network.AWS.ELBv2.Types.PathPatternConditionConfig
import Network.AWS.ELBv2.Types.QueryStringConditionConfig
import Network.AWS.ELBv2.Types.SourceIPConditionConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a condition for a rule.
--
-- Each rule can optionally include up to one of each of the following conditions: @http-request-method@ , @host-header@ , @path-pattern@ , and @source-ip@ . Each rule can also optionally include one or more of each of the following conditions: @http-header@ and @query-string@ .
--
-- /See:/ 'mkRuleCondition' smart constructor.
data RuleCondition = RuleCondition'
  { -- | The field in the HTTP request. The following are the possible values:
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
    field :: Lude.Maybe Lude.Text,
    -- | Information for an HTTP header condition. Specify only when @Field@ is @http-header@ .
    hTTPHeaderConfig :: Lude.Maybe HTTPHeaderConditionConfig,
    -- | Information for a host header condition. Specify only when @Field@ is @host-header@ .
    hostHeaderConfig :: Lude.Maybe HostHeaderConditionConfig,
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
    values :: Lude.Maybe [Lude.Text],
    -- | Information for a source IP condition. Specify only when @Field@ is @source-ip@ .
    sourceIPConfig :: Lude.Maybe SourceIPConditionConfig,
    -- | Information for an HTTP method condition. Specify only when @Field@ is @http-request-method@ .
    hTTPRequestMethodConfig :: Lude.Maybe HTTPRequestMethodConditionConfig,
    -- | Information for a path pattern condition. Specify only when @Field@ is @path-pattern@ .
    pathPatternConfig :: Lude.Maybe PathPatternConditionConfig,
    -- | Information for a query string condition. Specify only when @Field@ is @query-string@ .
    queryStringConfig :: Lude.Maybe QueryStringConditionConfig
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RuleCondition' with the minimum fields required to make a request.
--
-- * 'field' - The field in the HTTP request. The following are the possible values:
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
-- * 'hTTPHeaderConfig' - Information for an HTTP header condition. Specify only when @Field@ is @http-header@ .
-- * 'hostHeaderConfig' - Information for a host header condition. Specify only when @Field@ is @host-header@ .
-- * 'values' - The condition value. Specify only when @Field@ is @host-header@ or @path-pattern@ . Alternatively, to specify multiple host names or multiple path patterns, use @HostHeaderConfig@ or @PathPatternConfig@ .
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
-- * 'sourceIPConfig' - Information for a source IP condition. Specify only when @Field@ is @source-ip@ .
-- * 'hTTPRequestMethodConfig' - Information for an HTTP method condition. Specify only when @Field@ is @http-request-method@ .
-- * 'pathPatternConfig' - Information for a path pattern condition. Specify only when @Field@ is @path-pattern@ .
-- * 'queryStringConfig' - Information for a query string condition. Specify only when @Field@ is @query-string@ .
mkRuleCondition ::
  RuleCondition
mkRuleCondition =
  RuleCondition'
    { field = Lude.Nothing,
      hTTPHeaderConfig = Lude.Nothing,
      hostHeaderConfig = Lude.Nothing,
      values = Lude.Nothing,
      sourceIPConfig = Lude.Nothing,
      hTTPRequestMethodConfig = Lude.Nothing,
      pathPatternConfig = Lude.Nothing,
      queryStringConfig = Lude.Nothing
    }

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
rcField :: Lens.Lens' RuleCondition (Lude.Maybe Lude.Text)
rcField = Lens.lens (field :: RuleCondition -> Lude.Maybe Lude.Text) (\s a -> s {field = a} :: RuleCondition)
{-# DEPRECATED rcField "Use generic-lens or generic-optics with 'field' instead." #-}

-- | Information for an HTTP header condition. Specify only when @Field@ is @http-header@ .
--
-- /Note:/ Consider using 'hTTPHeaderConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcHTTPHeaderConfig :: Lens.Lens' RuleCondition (Lude.Maybe HTTPHeaderConditionConfig)
rcHTTPHeaderConfig = Lens.lens (hTTPHeaderConfig :: RuleCondition -> Lude.Maybe HTTPHeaderConditionConfig) (\s a -> s {hTTPHeaderConfig = a} :: RuleCondition)
{-# DEPRECATED rcHTTPHeaderConfig "Use generic-lens or generic-optics with 'hTTPHeaderConfig' instead." #-}

-- | Information for a host header condition. Specify only when @Field@ is @host-header@ .
--
-- /Note:/ Consider using 'hostHeaderConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcHostHeaderConfig :: Lens.Lens' RuleCondition (Lude.Maybe HostHeaderConditionConfig)
rcHostHeaderConfig = Lens.lens (hostHeaderConfig :: RuleCondition -> Lude.Maybe HostHeaderConditionConfig) (\s a -> s {hostHeaderConfig = a} :: RuleCondition)
{-# DEPRECATED rcHostHeaderConfig "Use generic-lens or generic-optics with 'hostHeaderConfig' instead." #-}

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
rcValues :: Lens.Lens' RuleCondition (Lude.Maybe [Lude.Text])
rcValues = Lens.lens (values :: RuleCondition -> Lude.Maybe [Lude.Text]) (\s a -> s {values = a} :: RuleCondition)
{-# DEPRECATED rcValues "Use generic-lens or generic-optics with 'values' instead." #-}

-- | Information for a source IP condition. Specify only when @Field@ is @source-ip@ .
--
-- /Note:/ Consider using 'sourceIPConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcSourceIPConfig :: Lens.Lens' RuleCondition (Lude.Maybe SourceIPConditionConfig)
rcSourceIPConfig = Lens.lens (sourceIPConfig :: RuleCondition -> Lude.Maybe SourceIPConditionConfig) (\s a -> s {sourceIPConfig = a} :: RuleCondition)
{-# DEPRECATED rcSourceIPConfig "Use generic-lens or generic-optics with 'sourceIPConfig' instead." #-}

-- | Information for an HTTP method condition. Specify only when @Field@ is @http-request-method@ .
--
-- /Note:/ Consider using 'hTTPRequestMethodConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcHTTPRequestMethodConfig :: Lens.Lens' RuleCondition (Lude.Maybe HTTPRequestMethodConditionConfig)
rcHTTPRequestMethodConfig = Lens.lens (hTTPRequestMethodConfig :: RuleCondition -> Lude.Maybe HTTPRequestMethodConditionConfig) (\s a -> s {hTTPRequestMethodConfig = a} :: RuleCondition)
{-# DEPRECATED rcHTTPRequestMethodConfig "Use generic-lens or generic-optics with 'hTTPRequestMethodConfig' instead." #-}

-- | Information for a path pattern condition. Specify only when @Field@ is @path-pattern@ .
--
-- /Note:/ Consider using 'pathPatternConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcPathPatternConfig :: Lens.Lens' RuleCondition (Lude.Maybe PathPatternConditionConfig)
rcPathPatternConfig = Lens.lens (pathPatternConfig :: RuleCondition -> Lude.Maybe PathPatternConditionConfig) (\s a -> s {pathPatternConfig = a} :: RuleCondition)
{-# DEPRECATED rcPathPatternConfig "Use generic-lens or generic-optics with 'pathPatternConfig' instead." #-}

-- | Information for a query string condition. Specify only when @Field@ is @query-string@ .
--
-- /Note:/ Consider using 'queryStringConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcQueryStringConfig :: Lens.Lens' RuleCondition (Lude.Maybe QueryStringConditionConfig)
rcQueryStringConfig = Lens.lens (queryStringConfig :: RuleCondition -> Lude.Maybe QueryStringConditionConfig) (\s a -> s {queryStringConfig = a} :: RuleCondition)
{-# DEPRECATED rcQueryStringConfig "Use generic-lens or generic-optics with 'queryStringConfig' instead." #-}

instance Lude.FromXML RuleCondition where
  parseXML x =
    RuleCondition'
      Lude.<$> (x Lude..@? "Field")
      Lude.<*> (x Lude..@? "HttpHeaderConfig")
      Lude.<*> (x Lude..@? "HostHeaderConfig")
      Lude.<*> ( x Lude..@? "Values" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> (x Lude..@? "SourceIpConfig")
      Lude.<*> (x Lude..@? "HttpRequestMethodConfig")
      Lude.<*> (x Lude..@? "PathPatternConfig")
      Lude.<*> (x Lude..@? "QueryStringConfig")

instance Lude.ToQuery RuleCondition where
  toQuery RuleCondition' {..} =
    Lude.mconcat
      [ "Field" Lude.=: field,
        "HttpHeaderConfig" Lude.=: hTTPHeaderConfig,
        "HostHeaderConfig" Lude.=: hostHeaderConfig,
        "Values"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> values),
        "SourceIpConfig" Lude.=: sourceIPConfig,
        "HttpRequestMethodConfig" Lude.=: hTTPRequestMethodConfig,
        "PathPatternConfig" Lude.=: pathPatternConfig,
        "QueryStringConfig" Lude.=: queryStringConfig
      ]
