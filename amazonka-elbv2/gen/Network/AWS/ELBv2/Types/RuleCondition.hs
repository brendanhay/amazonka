{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.RuleCondition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.RuleCondition where

import qualified Network.AWS.Core as Core
import Network.AWS.ELBv2.Types.HostHeaderConditionConfig
import Network.AWS.ELBv2.Types.HttpHeaderConditionConfig
import Network.AWS.ELBv2.Types.HttpRequestMethodConditionConfig
import Network.AWS.ELBv2.Types.PathPatternConditionConfig
import Network.AWS.ELBv2.Types.QueryStringConditionConfig
import Network.AWS.ELBv2.Types.SourceIpConditionConfig
import qualified Network.AWS.Lens as Lens

-- | Information about a condition for a rule.
--
-- Each rule can optionally include up to one of each of the following
-- conditions: @http-request-method@, @host-header@, @path-pattern@, and
-- @source-ip@. Each rule can also optionally include one or more of each
-- of the following conditions: @http-header@ and @query-string@.
--
-- /See:/ 'newRuleCondition' smart constructor.
data RuleCondition = RuleCondition'
  { -- | Information for a path pattern condition. Specify only when @Field@ is
    -- @path-pattern@.
    pathPatternConfig :: Core.Maybe PathPatternConditionConfig,
    -- | Information for an HTTP method condition. Specify only when @Field@ is
    -- @http-request-method@.
    httpRequestMethodConfig :: Core.Maybe HttpRequestMethodConditionConfig,
    -- | The condition value. Specify only when @Field@ is @host-header@ or
    -- @path-pattern@. Alternatively, to specify multiple host names or
    -- multiple path patterns, use @HostHeaderConfig@ or @PathPatternConfig@.
    --
    -- If @Field@ is @host-header@ and you are not using @HostHeaderConfig@,
    -- you can specify a single host name (for example, my.example.com) in
    -- @Values@. A host name is case insensitive, can be up to 128 characters
    -- in length, and can contain any of the following characters.
    --
    -- -   A-Z, a-z, 0-9
    --
    -- -   - .
    --
    -- -   * (matches 0 or more characters)
    --
    -- -   ? (matches exactly 1 character)
    --
    -- If @Field@ is @path-pattern@ and you are not using @PathPatternConfig@,
    -- you can specify a single path pattern (for example, \/img\/*) in
    -- @Values@. A path pattern is case-sensitive, can be up to 128 characters
    -- in length, and can contain any of the following characters.
    --
    -- -   A-Z, a-z, 0-9
    --
    -- -   _ - . $ \/ ~ \" \' \@ : +
    --
    -- -   & (using &amp;)
    --
    -- -   * (matches 0 or more characters)
    --
    -- -   ? (matches exactly 1 character)
    values :: Core.Maybe [Core.Text],
    -- | Information for a source IP condition. Specify only when @Field@ is
    -- @source-ip@.
    sourceIpConfig :: Core.Maybe SourceIpConditionConfig,
    -- | Information for an HTTP header condition. Specify only when @Field@ is
    -- @http-header@.
    httpHeaderConfig :: Core.Maybe HttpHeaderConditionConfig,
    -- | Information for a host header condition. Specify only when @Field@ is
    -- @host-header@.
    hostHeaderConfig :: Core.Maybe HostHeaderConditionConfig,
    -- | Information for a query string condition. Specify only when @Field@ is
    -- @query-string@.
    queryStringConfig :: Core.Maybe QueryStringConditionConfig,
    -- | The field in the HTTP request. The following are the possible values:
    --
    -- -   @http-header@
    --
    -- -   @http-request-method@
    --
    -- -   @host-header@
    --
    -- -   @path-pattern@
    --
    -- -   @query-string@
    --
    -- -   @source-ip@
    field :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RuleCondition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pathPatternConfig', 'ruleCondition_pathPatternConfig' - Information for a path pattern condition. Specify only when @Field@ is
-- @path-pattern@.
--
-- 'httpRequestMethodConfig', 'ruleCondition_httpRequestMethodConfig' - Information for an HTTP method condition. Specify only when @Field@ is
-- @http-request-method@.
--
-- 'values', 'ruleCondition_values' - The condition value. Specify only when @Field@ is @host-header@ or
-- @path-pattern@. Alternatively, to specify multiple host names or
-- multiple path patterns, use @HostHeaderConfig@ or @PathPatternConfig@.
--
-- If @Field@ is @host-header@ and you are not using @HostHeaderConfig@,
-- you can specify a single host name (for example, my.example.com) in
-- @Values@. A host name is case insensitive, can be up to 128 characters
-- in length, and can contain any of the following characters.
--
-- -   A-Z, a-z, 0-9
--
-- -   - .
--
-- -   * (matches 0 or more characters)
--
-- -   ? (matches exactly 1 character)
--
-- If @Field@ is @path-pattern@ and you are not using @PathPatternConfig@,
-- you can specify a single path pattern (for example, \/img\/*) in
-- @Values@. A path pattern is case-sensitive, can be up to 128 characters
-- in length, and can contain any of the following characters.
--
-- -   A-Z, a-z, 0-9
--
-- -   _ - . $ \/ ~ \" \' \@ : +
--
-- -   & (using &amp;)
--
-- -   * (matches 0 or more characters)
--
-- -   ? (matches exactly 1 character)
--
-- 'sourceIpConfig', 'ruleCondition_sourceIpConfig' - Information for a source IP condition. Specify only when @Field@ is
-- @source-ip@.
--
-- 'httpHeaderConfig', 'ruleCondition_httpHeaderConfig' - Information for an HTTP header condition. Specify only when @Field@ is
-- @http-header@.
--
-- 'hostHeaderConfig', 'ruleCondition_hostHeaderConfig' - Information for a host header condition. Specify only when @Field@ is
-- @host-header@.
--
-- 'queryStringConfig', 'ruleCondition_queryStringConfig' - Information for a query string condition. Specify only when @Field@ is
-- @query-string@.
--
-- 'field', 'ruleCondition_field' - The field in the HTTP request. The following are the possible values:
--
-- -   @http-header@
--
-- -   @http-request-method@
--
-- -   @host-header@
--
-- -   @path-pattern@
--
-- -   @query-string@
--
-- -   @source-ip@
newRuleCondition ::
  RuleCondition
newRuleCondition =
  RuleCondition'
    { pathPatternConfig = Core.Nothing,
      httpRequestMethodConfig = Core.Nothing,
      values = Core.Nothing,
      sourceIpConfig = Core.Nothing,
      httpHeaderConfig = Core.Nothing,
      hostHeaderConfig = Core.Nothing,
      queryStringConfig = Core.Nothing,
      field = Core.Nothing
    }

-- | Information for a path pattern condition. Specify only when @Field@ is
-- @path-pattern@.
ruleCondition_pathPatternConfig :: Lens.Lens' RuleCondition (Core.Maybe PathPatternConditionConfig)
ruleCondition_pathPatternConfig = Lens.lens (\RuleCondition' {pathPatternConfig} -> pathPatternConfig) (\s@RuleCondition' {} a -> s {pathPatternConfig = a} :: RuleCondition)

-- | Information for an HTTP method condition. Specify only when @Field@ is
-- @http-request-method@.
ruleCondition_httpRequestMethodConfig :: Lens.Lens' RuleCondition (Core.Maybe HttpRequestMethodConditionConfig)
ruleCondition_httpRequestMethodConfig = Lens.lens (\RuleCondition' {httpRequestMethodConfig} -> httpRequestMethodConfig) (\s@RuleCondition' {} a -> s {httpRequestMethodConfig = a} :: RuleCondition)

-- | The condition value. Specify only when @Field@ is @host-header@ or
-- @path-pattern@. Alternatively, to specify multiple host names or
-- multiple path patterns, use @HostHeaderConfig@ or @PathPatternConfig@.
--
-- If @Field@ is @host-header@ and you are not using @HostHeaderConfig@,
-- you can specify a single host name (for example, my.example.com) in
-- @Values@. A host name is case insensitive, can be up to 128 characters
-- in length, and can contain any of the following characters.
--
-- -   A-Z, a-z, 0-9
--
-- -   - .
--
-- -   * (matches 0 or more characters)
--
-- -   ? (matches exactly 1 character)
--
-- If @Field@ is @path-pattern@ and you are not using @PathPatternConfig@,
-- you can specify a single path pattern (for example, \/img\/*) in
-- @Values@. A path pattern is case-sensitive, can be up to 128 characters
-- in length, and can contain any of the following characters.
--
-- -   A-Z, a-z, 0-9
--
-- -   _ - . $ \/ ~ \" \' \@ : +
--
-- -   & (using &amp;)
--
-- -   * (matches 0 or more characters)
--
-- -   ? (matches exactly 1 character)
ruleCondition_values :: Lens.Lens' RuleCondition (Core.Maybe [Core.Text])
ruleCondition_values = Lens.lens (\RuleCondition' {values} -> values) (\s@RuleCondition' {} a -> s {values = a} :: RuleCondition) Core.. Lens.mapping Lens._Coerce

-- | Information for a source IP condition. Specify only when @Field@ is
-- @source-ip@.
ruleCondition_sourceIpConfig :: Lens.Lens' RuleCondition (Core.Maybe SourceIpConditionConfig)
ruleCondition_sourceIpConfig = Lens.lens (\RuleCondition' {sourceIpConfig} -> sourceIpConfig) (\s@RuleCondition' {} a -> s {sourceIpConfig = a} :: RuleCondition)

-- | Information for an HTTP header condition. Specify only when @Field@ is
-- @http-header@.
ruleCondition_httpHeaderConfig :: Lens.Lens' RuleCondition (Core.Maybe HttpHeaderConditionConfig)
ruleCondition_httpHeaderConfig = Lens.lens (\RuleCondition' {httpHeaderConfig} -> httpHeaderConfig) (\s@RuleCondition' {} a -> s {httpHeaderConfig = a} :: RuleCondition)

-- | Information for a host header condition. Specify only when @Field@ is
-- @host-header@.
ruleCondition_hostHeaderConfig :: Lens.Lens' RuleCondition (Core.Maybe HostHeaderConditionConfig)
ruleCondition_hostHeaderConfig = Lens.lens (\RuleCondition' {hostHeaderConfig} -> hostHeaderConfig) (\s@RuleCondition' {} a -> s {hostHeaderConfig = a} :: RuleCondition)

-- | Information for a query string condition. Specify only when @Field@ is
-- @query-string@.
ruleCondition_queryStringConfig :: Lens.Lens' RuleCondition (Core.Maybe QueryStringConditionConfig)
ruleCondition_queryStringConfig = Lens.lens (\RuleCondition' {queryStringConfig} -> queryStringConfig) (\s@RuleCondition' {} a -> s {queryStringConfig = a} :: RuleCondition)

-- | The field in the HTTP request. The following are the possible values:
--
-- -   @http-header@
--
-- -   @http-request-method@
--
-- -   @host-header@
--
-- -   @path-pattern@
--
-- -   @query-string@
--
-- -   @source-ip@
ruleCondition_field :: Lens.Lens' RuleCondition (Core.Maybe Core.Text)
ruleCondition_field = Lens.lens (\RuleCondition' {field} -> field) (\s@RuleCondition' {} a -> s {field = a} :: RuleCondition)

instance Core.FromXML RuleCondition where
  parseXML x =
    RuleCondition'
      Core.<$> (x Core..@? "PathPatternConfig")
      Core.<*> (x Core..@? "HttpRequestMethodConfig")
      Core.<*> ( x Core..@? "Values" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )
      Core.<*> (x Core..@? "SourceIpConfig")
      Core.<*> (x Core..@? "HttpHeaderConfig")
      Core.<*> (x Core..@? "HostHeaderConfig")
      Core.<*> (x Core..@? "QueryStringConfig")
      Core.<*> (x Core..@? "Field")

instance Core.Hashable RuleCondition

instance Core.NFData RuleCondition

instance Core.ToQuery RuleCondition where
  toQuery RuleCondition' {..} =
    Core.mconcat
      [ "PathPatternConfig" Core.=: pathPatternConfig,
        "HttpRequestMethodConfig"
          Core.=: httpRequestMethodConfig,
        "Values"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Core.<$> values),
        "SourceIpConfig" Core.=: sourceIpConfig,
        "HttpHeaderConfig" Core.=: httpHeaderConfig,
        "HostHeaderConfig" Core.=: hostHeaderConfig,
        "QueryStringConfig" Core.=: queryStringConfig,
        "Field" Core.=: field
      ]
