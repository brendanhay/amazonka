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
-- Module      : Amazonka.ELBV2.Types.RuleCondition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ELBV2.Types.RuleCondition where

import qualified Amazonka.Core as Core
import Amazonka.ELBV2.Types.HostHeaderConditionConfig
import Amazonka.ELBV2.Types.HttpHeaderConditionConfig
import Amazonka.ELBV2.Types.HttpRequestMethodConditionConfig
import Amazonka.ELBV2.Types.PathPatternConditionConfig
import Amazonka.ELBV2.Types.QueryStringConditionConfig
import Amazonka.ELBV2.Types.SourceIpConditionConfig
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about a condition for a rule.
--
-- Each rule can optionally include up to one of each of the following
-- conditions: @http-request-method@, @host-header@, @path-pattern@, and
-- @source-ip@. Each rule can also optionally include one or more of each
-- of the following conditions: @http-header@ and @query-string@.
--
-- /See:/ 'newRuleCondition' smart constructor.
data RuleCondition = RuleCondition'
  { -- | The field in the HTTP request. The following are the possible values:
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
    field :: Prelude.Maybe Prelude.Text,
    -- | Information for an HTTP header condition. Specify only when @Field@ is
    -- @http-header@.
    httpHeaderConfig :: Prelude.Maybe HttpHeaderConditionConfig,
    -- | Information for a host header condition. Specify only when @Field@ is
    -- @host-header@.
    hostHeaderConfig :: Prelude.Maybe HostHeaderConditionConfig,
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
    values :: Prelude.Maybe [Prelude.Text],
    -- | Information for a source IP condition. Specify only when @Field@ is
    -- @source-ip@.
    sourceIpConfig :: Prelude.Maybe SourceIpConditionConfig,
    -- | Information for an HTTP method condition. Specify only when @Field@ is
    -- @http-request-method@.
    httpRequestMethodConfig :: Prelude.Maybe HttpRequestMethodConditionConfig,
    -- | Information for a path pattern condition. Specify only when @Field@ is
    -- @path-pattern@.
    pathPatternConfig :: Prelude.Maybe PathPatternConditionConfig,
    -- | Information for a query string condition. Specify only when @Field@ is
    -- @query-string@.
    queryStringConfig :: Prelude.Maybe QueryStringConditionConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RuleCondition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
--
-- 'httpHeaderConfig', 'ruleCondition_httpHeaderConfig' - Information for an HTTP header condition. Specify only when @Field@ is
-- @http-header@.
--
-- 'hostHeaderConfig', 'ruleCondition_hostHeaderConfig' - Information for a host header condition. Specify only when @Field@ is
-- @host-header@.
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
-- 'httpRequestMethodConfig', 'ruleCondition_httpRequestMethodConfig' - Information for an HTTP method condition. Specify only when @Field@ is
-- @http-request-method@.
--
-- 'pathPatternConfig', 'ruleCondition_pathPatternConfig' - Information for a path pattern condition. Specify only when @Field@ is
-- @path-pattern@.
--
-- 'queryStringConfig', 'ruleCondition_queryStringConfig' - Information for a query string condition. Specify only when @Field@ is
-- @query-string@.
newRuleCondition ::
  RuleCondition
newRuleCondition =
  RuleCondition'
    { field = Prelude.Nothing,
      httpHeaderConfig = Prelude.Nothing,
      hostHeaderConfig = Prelude.Nothing,
      values = Prelude.Nothing,
      sourceIpConfig = Prelude.Nothing,
      httpRequestMethodConfig = Prelude.Nothing,
      pathPatternConfig = Prelude.Nothing,
      queryStringConfig = Prelude.Nothing
    }

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
ruleCondition_field :: Lens.Lens' RuleCondition (Prelude.Maybe Prelude.Text)
ruleCondition_field = Lens.lens (\RuleCondition' {field} -> field) (\s@RuleCondition' {} a -> s {field = a} :: RuleCondition)

-- | Information for an HTTP header condition. Specify only when @Field@ is
-- @http-header@.
ruleCondition_httpHeaderConfig :: Lens.Lens' RuleCondition (Prelude.Maybe HttpHeaderConditionConfig)
ruleCondition_httpHeaderConfig = Lens.lens (\RuleCondition' {httpHeaderConfig} -> httpHeaderConfig) (\s@RuleCondition' {} a -> s {httpHeaderConfig = a} :: RuleCondition)

-- | Information for a host header condition. Specify only when @Field@ is
-- @host-header@.
ruleCondition_hostHeaderConfig :: Lens.Lens' RuleCondition (Prelude.Maybe HostHeaderConditionConfig)
ruleCondition_hostHeaderConfig = Lens.lens (\RuleCondition' {hostHeaderConfig} -> hostHeaderConfig) (\s@RuleCondition' {} a -> s {hostHeaderConfig = a} :: RuleCondition)

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
ruleCondition_values :: Lens.Lens' RuleCondition (Prelude.Maybe [Prelude.Text])
ruleCondition_values = Lens.lens (\RuleCondition' {values} -> values) (\s@RuleCondition' {} a -> s {values = a} :: RuleCondition) Prelude.. Lens.mapping Lens.coerced

-- | Information for a source IP condition. Specify only when @Field@ is
-- @source-ip@.
ruleCondition_sourceIpConfig :: Lens.Lens' RuleCondition (Prelude.Maybe SourceIpConditionConfig)
ruleCondition_sourceIpConfig = Lens.lens (\RuleCondition' {sourceIpConfig} -> sourceIpConfig) (\s@RuleCondition' {} a -> s {sourceIpConfig = a} :: RuleCondition)

-- | Information for an HTTP method condition. Specify only when @Field@ is
-- @http-request-method@.
ruleCondition_httpRequestMethodConfig :: Lens.Lens' RuleCondition (Prelude.Maybe HttpRequestMethodConditionConfig)
ruleCondition_httpRequestMethodConfig = Lens.lens (\RuleCondition' {httpRequestMethodConfig} -> httpRequestMethodConfig) (\s@RuleCondition' {} a -> s {httpRequestMethodConfig = a} :: RuleCondition)

-- | Information for a path pattern condition. Specify only when @Field@ is
-- @path-pattern@.
ruleCondition_pathPatternConfig :: Lens.Lens' RuleCondition (Prelude.Maybe PathPatternConditionConfig)
ruleCondition_pathPatternConfig = Lens.lens (\RuleCondition' {pathPatternConfig} -> pathPatternConfig) (\s@RuleCondition' {} a -> s {pathPatternConfig = a} :: RuleCondition)

-- | Information for a query string condition. Specify only when @Field@ is
-- @query-string@.
ruleCondition_queryStringConfig :: Lens.Lens' RuleCondition (Prelude.Maybe QueryStringConditionConfig)
ruleCondition_queryStringConfig = Lens.lens (\RuleCondition' {queryStringConfig} -> queryStringConfig) (\s@RuleCondition' {} a -> s {queryStringConfig = a} :: RuleCondition)

instance Core.FromXML RuleCondition where
  parseXML x =
    RuleCondition'
      Prelude.<$> (x Core..@? "Field")
      Prelude.<*> (x Core..@? "HttpHeaderConfig")
      Prelude.<*> (x Core..@? "HostHeaderConfig")
      Prelude.<*> ( x Core..@? "Values" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> (x Core..@? "SourceIpConfig")
      Prelude.<*> (x Core..@? "HttpRequestMethodConfig")
      Prelude.<*> (x Core..@? "PathPatternConfig")
      Prelude.<*> (x Core..@? "QueryStringConfig")

instance Prelude.Hashable RuleCondition where
  hashWithSalt _salt RuleCondition' {..} =
    _salt `Prelude.hashWithSalt` field
      `Prelude.hashWithSalt` httpHeaderConfig
      `Prelude.hashWithSalt` hostHeaderConfig
      `Prelude.hashWithSalt` values
      `Prelude.hashWithSalt` sourceIpConfig
      `Prelude.hashWithSalt` httpRequestMethodConfig
      `Prelude.hashWithSalt` pathPatternConfig
      `Prelude.hashWithSalt` queryStringConfig

instance Prelude.NFData RuleCondition where
  rnf RuleCondition' {..} =
    Prelude.rnf field
      `Prelude.seq` Prelude.rnf httpHeaderConfig
      `Prelude.seq` Prelude.rnf hostHeaderConfig
      `Prelude.seq` Prelude.rnf values
      `Prelude.seq` Prelude.rnf sourceIpConfig
      `Prelude.seq` Prelude.rnf httpRequestMethodConfig
      `Prelude.seq` Prelude.rnf pathPatternConfig
      `Prelude.seq` Prelude.rnf queryStringConfig

instance Core.ToQuery RuleCondition where
  toQuery RuleCondition' {..} =
    Prelude.mconcat
      [ "Field" Core.=: field,
        "HttpHeaderConfig" Core.=: httpHeaderConfig,
        "HostHeaderConfig" Core.=: hostHeaderConfig,
        "Values"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Prelude.<$> values),
        "SourceIpConfig" Core.=: sourceIpConfig,
        "HttpRequestMethodConfig"
          Core.=: httpRequestMethodConfig,
        "PathPatternConfig" Core.=: pathPatternConfig,
        "QueryStringConfig" Core.=: queryStringConfig
      ]
