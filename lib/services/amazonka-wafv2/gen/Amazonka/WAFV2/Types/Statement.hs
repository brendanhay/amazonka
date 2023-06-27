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
-- Module      : Amazonka.WAFV2.Types.Statement
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.Statement where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import {-# SOURCE #-} Amazonka.WAFV2.Types.AndStatement
import Amazonka.WAFV2.Types.ByteMatchStatement
import Amazonka.WAFV2.Types.GeoMatchStatement
import Amazonka.WAFV2.Types.IPSetReferenceStatement
import Amazonka.WAFV2.Types.LabelMatchStatement
import {-# SOURCE #-} Amazonka.WAFV2.Types.ManagedRuleGroupStatement
import {-# SOURCE #-} Amazonka.WAFV2.Types.NotStatement
import {-# SOURCE #-} Amazonka.WAFV2.Types.OrStatement
import {-# SOURCE #-} Amazonka.WAFV2.Types.RateBasedStatement
import Amazonka.WAFV2.Types.RegexMatchStatement
import Amazonka.WAFV2.Types.RegexPatternSetReferenceStatement
import Amazonka.WAFV2.Types.RuleGroupReferenceStatement
import Amazonka.WAFV2.Types.SizeConstraintStatement
import Amazonka.WAFV2.Types.SqliMatchStatement
import Amazonka.WAFV2.Types.XssMatchStatement

-- | The processing guidance for a Rule, used by WAF to determine whether a
-- web request matches the rule.
--
-- For example specifications, see the examples section of CreateWebACL.
--
-- /See:/ 'newStatement' smart constructor.
data Statement = Statement'
  { -- | A logical rule statement used to combine other rule statements with AND
    -- logic. You provide more than one Statement within the @AndStatement@.
    andStatement :: Prelude.Maybe AndStatement,
    -- | A rule statement that defines a string match search for WAF to apply to
    -- web requests. The byte match statement provides the bytes to search for,
    -- the location in requests that you want WAF to search, and other
    -- settings. The bytes to search for are typically a string that
    -- corresponds with ASCII characters. In the WAF console and the developer
    -- guide, this is called a string match statement.
    byteMatchStatement :: Prelude.Maybe ByteMatchStatement,
    -- | A rule statement that labels web requests by country and region and that
    -- matches against web requests based on country code. A geo match rule
    -- labels every request that it inspects regardless of whether it finds a
    -- match.
    --
    -- -   To manage requests only by country, you can use this statement by
    --     itself and specify the countries that you want to match against in
    --     the @CountryCodes@ array.
    --
    -- -   Otherwise, configure your geo match rule with Count action so that
    --     it only labels requests. Then, add one or more label match rules to
    --     run after the geo match rule and configure them to match against the
    --     geographic labels and handle the requests as needed.
    --
    -- WAF labels requests using the alpha-2 country and region codes from the
    -- International Organization for Standardization (ISO) 3166 standard. WAF
    -- determines the codes using either the IP address in the web request
    -- origin or, if you specify it, the address in the geo match
    -- @ForwardedIPConfig@.
    --
    -- If you use the web request origin, the label formats are
    -- @awswaf:clientip:geo:region:\<ISO country code>-\<ISO region code>@ and
    -- @awswaf:clientip:geo:country:\<ISO country code>@.
    --
    -- If you use a forwarded IP address, the label formats are
    -- @awswaf:forwardedip:geo:region:\<ISO country code>-\<ISO region code>@
    -- and @awswaf:forwardedip:geo:country:\<ISO country code>@.
    --
    -- For additional details, see
    -- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-rule-statement-type-geo-match.html Geographic match rule statement>
    -- in the
    -- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html WAF Developer Guide>.
    geoMatchStatement :: Prelude.Maybe GeoMatchStatement,
    -- | A rule statement used to detect web requests coming from particular IP
    -- addresses or address ranges. To use this, create an IPSet that specifies
    -- the addresses you want to detect, then use the ARN of that set in this
    -- statement. To create an IP set, see CreateIPSet.
    --
    -- Each IP set rule statement references an IP set. You create and maintain
    -- the set independent of your rules. This allows you to use the single set
    -- in multiple rules. When you update the referenced set, WAF automatically
    -- updates all rules that reference it.
    iPSetReferenceStatement :: Prelude.Maybe IPSetReferenceStatement,
    -- | A rule statement to match against labels that have been added to the web
    -- request by rules that have already run in the web ACL.
    --
    -- The label match statement provides the label or namespace string to
    -- search for. The label string can represent a part or all of the fully
    -- qualified label name that had been added to the web request. Fully
    -- qualified labels have a prefix, optional namespaces, and label name. The
    -- prefix identifies the rule group or web ACL context of the rule that
    -- added the label. If you do not provide the fully qualified name in your
    -- label match string, WAF performs the search for labels that were added
    -- in the same context as the label match statement.
    labelMatchStatement :: Prelude.Maybe LabelMatchStatement,
    -- | A rule statement used to run the rules that are defined in a managed
    -- rule group. To use this, provide the vendor name and the name of the
    -- rule group in this statement. You can retrieve the required names by
    -- calling ListAvailableManagedRuleGroups.
    --
    -- You cannot nest a @ManagedRuleGroupStatement@, for example for use
    -- inside a @NotStatement@ or @OrStatement@. It can only be referenced as a
    -- top-level statement within a rule.
    --
    -- You are charged additional fees when you use the WAF Bot Control managed
    -- rule group @AWSManagedRulesBotControlRuleSet@, the WAF Fraud Control
    -- account takeover prevention (ATP) managed rule group
    -- @AWSManagedRulesATPRuleSet@, or the WAF Fraud Control account creation
    -- fraud prevention (ACFP) managed rule group @AWSManagedRulesACFPRuleSet@.
    -- For more information, see
    -- <http://aws.amazon.com/waf/pricing/ WAF Pricing>.
    managedRuleGroupStatement :: Prelude.Maybe ManagedRuleGroupStatement,
    -- | A logical rule statement used to negate the results of another rule
    -- statement. You provide one Statement within the @NotStatement@.
    notStatement :: Prelude.Maybe NotStatement,
    -- | A logical rule statement used to combine other rule statements with OR
    -- logic. You provide more than one Statement within the @OrStatement@.
    orStatement :: Prelude.Maybe OrStatement,
    -- | A rate-based rule counts incoming requests and rate limits requests when
    -- they are coming at too fast a rate. The rule categorizes requests
    -- according to your aggregation criteria, collects them into aggregation
    -- instances, and counts and rate limits the requests for each instance.
    --
    -- You can specify individual aggregation keys, like IP address or HTTP
    -- method. You can also specify aggregation key combinations, like IP
    -- address and HTTP method, or HTTP method, query argument, and cookie.
    --
    -- Each unique set of values for the aggregation keys that you specify is a
    -- separate aggregation instance, with the value from each key contributing
    -- to the aggregation instance definition.
    --
    -- For example, assume the rule evaluates web requests with the following
    -- IP address and HTTP method values:
    --
    -- -   IP address 10.1.1.1, HTTP method POST
    --
    -- -   IP address 10.1.1.1, HTTP method GET
    --
    -- -   IP address 127.0.0.0, HTTP method POST
    --
    -- -   IP address 10.1.1.1, HTTP method GET
    --
    -- The rule would create different aggregation instances according to your
    -- aggregation criteria, for example:
    --
    -- -   If the aggregation criteria is just the IP address, then each
    --     individual address is an aggregation instance, and WAF counts
    --     requests separately for each. The aggregation instances and request
    --     counts for our example would be the following:
    --
    --     -   IP address 10.1.1.1: count 3
    --
    --     -   IP address 127.0.0.0: count 1
    --
    -- -   If the aggregation criteria is HTTP method, then each individual
    --     HTTP method is an aggregation instance. The aggregation instances
    --     and request counts for our example would be the following:
    --
    --     -   HTTP method POST: count 2
    --
    --     -   HTTP method GET: count 2
    --
    -- -   If the aggregation criteria is IP address and HTTP method, then each
    --     IP address and each HTTP method would contribute to the combined
    --     aggregation instance. The aggregation instances and request counts
    --     for our example would be the following:
    --
    --     -   IP address 10.1.1.1, HTTP method POST: count 1
    --
    --     -   IP address 10.1.1.1, HTTP method GET: count 2
    --
    --     -   IP address 127.0.0.0, HTTP method POST: count 1
    --
    -- For any n-tuple of aggregation keys, each unique combination of values
    -- for the keys defines a separate aggregation instance, which WAF counts
    -- and rate-limits individually.
    --
    -- You can optionally nest another statement inside the rate-based
    -- statement, to narrow the scope of the rule so that it only counts and
    -- rate limits requests that match the nested statement. You can use this
    -- nested scope-down statement in conjunction with your aggregation key
    -- specifications or you can just count and rate limit all requests that
    -- match the scope-down statement, without additional aggregation. When you
    -- choose to just manage all requests that match a scope-down statement,
    -- the aggregation instance is singular for the rule.
    --
    -- You cannot nest a @RateBasedStatement@ inside another statement, for
    -- example inside a @NotStatement@ or @OrStatement@. You can define a
    -- @RateBasedStatement@ inside a web ACL and inside a rule group.
    --
    -- For additional information about the options, see
    -- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-rate-based-rules.html Rate limiting web requests using rate-based rules>
    -- in the /WAF Developer Guide/.
    --
    -- If you only aggregate on the individual IP address or forwarded IP
    -- address, you can retrieve the list of IP addresses that WAF is currently
    -- rate limiting for a rule through the API call
    -- @GetRateBasedStatementManagedKeys@. This option is not available for
    -- other aggregation configurations.
    --
    -- WAF tracks and manages web requests separately for each instance of a
    -- rate-based rule that you use. For example, if you provide the same
    -- rate-based rule settings in two web ACLs, each of the two rule
    -- statements represents a separate instance of the rate-based rule and
    -- gets its own tracking and management by WAF. If you define a rate-based
    -- rule inside a rule group, and then use that rule group in multiple
    -- places, each use creates a separate instance of the rate-based rule that
    -- gets its own tracking and management by WAF.
    rateBasedStatement :: Prelude.Maybe RateBasedStatement,
    -- | A rule statement used to search web request components for a match
    -- against a single regular expression.
    regexMatchStatement :: Prelude.Maybe RegexMatchStatement,
    -- | A rule statement used to search web request components for matches with
    -- regular expressions. To use this, create a RegexPatternSet that
    -- specifies the expressions that you want to detect, then use the ARN of
    -- that set in this statement. A web request matches the pattern set rule
    -- statement if the request component matches any of the patterns in the
    -- set. To create a regex pattern set, see CreateRegexPatternSet.
    --
    -- Each regex pattern set rule statement references a regex pattern set.
    -- You create and maintain the set independent of your rules. This allows
    -- you to use the single set in multiple rules. When you update the
    -- referenced set, WAF automatically updates all rules that reference it.
    regexPatternSetReferenceStatement :: Prelude.Maybe RegexPatternSetReferenceStatement,
    -- | A rule statement used to run the rules that are defined in a RuleGroup.
    -- To use this, create a rule group with your rules, then provide the ARN
    -- of the rule group in this statement.
    --
    -- You cannot nest a @RuleGroupReferenceStatement@, for example for use
    -- inside a @NotStatement@ or @OrStatement@. You can only use a rule group
    -- reference statement at the top level inside a web ACL.
    ruleGroupReferenceStatement :: Prelude.Maybe RuleGroupReferenceStatement,
    -- | A rule statement that compares a number of bytes against the size of a
    -- request component, using a comparison operator, such as greater than (>)
    -- or less than (\<). For example, you can use a size constraint statement
    -- to look for query strings that are longer than 100 bytes.
    --
    -- If you configure WAF to inspect the request body, WAF inspects only the
    -- number of bytes of the body up to the limit for the web ACL. By default,
    -- for regional web ACLs, this limit is 8 KB (8,192 kilobytes) and for
    -- CloudFront web ACLs, this limit is 16 KB (16,384 kilobytes). For
    -- CloudFront web ACLs, you can increase the limit in the web ACL
    -- @AssociationConfig@, for additional fees. If you know that the request
    -- body for your web requests should never exceed the inspection limit, you
    -- could use a size constraint statement to block requests that have a
    -- larger request body size.
    --
    -- If you choose URI for the value of Part of the request to filter on, the
    -- slash (\/) in the URI counts as one character. For example, the URI
    -- @\/logo.jpg@ is nine characters long.
    sizeConstraintStatement :: Prelude.Maybe SizeConstraintStatement,
    -- | A rule statement that inspects for malicious SQL code. Attackers insert
    -- malicious SQL code into web requests to do things like modify your
    -- database or extract data from it.
    sqliMatchStatement :: Prelude.Maybe SqliMatchStatement,
    -- | A rule statement that inspects for cross-site scripting (XSS) attacks.
    -- In XSS attacks, the attacker uses vulnerabilities in a benign website as
    -- a vehicle to inject malicious client-site scripts into other legitimate
    -- web browsers.
    xssMatchStatement :: Prelude.Maybe XssMatchStatement
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Statement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'andStatement', 'andStatement' - A logical rule statement used to combine other rule statements with AND
-- logic. You provide more than one Statement within the @AndStatement@.
--
-- 'byteMatchStatement', 'byteMatchStatement' - A rule statement that defines a string match search for WAF to apply to
-- web requests. The byte match statement provides the bytes to search for,
-- the location in requests that you want WAF to search, and other
-- settings. The bytes to search for are typically a string that
-- corresponds with ASCII characters. In the WAF console and the developer
-- guide, this is called a string match statement.
--
-- 'geoMatchStatement', 'geoMatchStatement' - A rule statement that labels web requests by country and region and that
-- matches against web requests based on country code. A geo match rule
-- labels every request that it inspects regardless of whether it finds a
-- match.
--
-- -   To manage requests only by country, you can use this statement by
--     itself and specify the countries that you want to match against in
--     the @CountryCodes@ array.
--
-- -   Otherwise, configure your geo match rule with Count action so that
--     it only labels requests. Then, add one or more label match rules to
--     run after the geo match rule and configure them to match against the
--     geographic labels and handle the requests as needed.
--
-- WAF labels requests using the alpha-2 country and region codes from the
-- International Organization for Standardization (ISO) 3166 standard. WAF
-- determines the codes using either the IP address in the web request
-- origin or, if you specify it, the address in the geo match
-- @ForwardedIPConfig@.
--
-- If you use the web request origin, the label formats are
-- @awswaf:clientip:geo:region:\<ISO country code>-\<ISO region code>@ and
-- @awswaf:clientip:geo:country:\<ISO country code>@.
--
-- If you use a forwarded IP address, the label formats are
-- @awswaf:forwardedip:geo:region:\<ISO country code>-\<ISO region code>@
-- and @awswaf:forwardedip:geo:country:\<ISO country code>@.
--
-- For additional details, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-rule-statement-type-geo-match.html Geographic match rule statement>
-- in the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html WAF Developer Guide>.
--
-- 'iPSetReferenceStatement', 'iPSetReferenceStatement' - A rule statement used to detect web requests coming from particular IP
-- addresses or address ranges. To use this, create an IPSet that specifies
-- the addresses you want to detect, then use the ARN of that set in this
-- statement. To create an IP set, see CreateIPSet.
--
-- Each IP set rule statement references an IP set. You create and maintain
-- the set independent of your rules. This allows you to use the single set
-- in multiple rules. When you update the referenced set, WAF automatically
-- updates all rules that reference it.
--
-- 'labelMatchStatement', 'labelMatchStatement' - A rule statement to match against labels that have been added to the web
-- request by rules that have already run in the web ACL.
--
-- The label match statement provides the label or namespace string to
-- search for. The label string can represent a part or all of the fully
-- qualified label name that had been added to the web request. Fully
-- qualified labels have a prefix, optional namespaces, and label name. The
-- prefix identifies the rule group or web ACL context of the rule that
-- added the label. If you do not provide the fully qualified name in your
-- label match string, WAF performs the search for labels that were added
-- in the same context as the label match statement.
--
-- 'managedRuleGroupStatement', 'managedRuleGroupStatement' - A rule statement used to run the rules that are defined in a managed
-- rule group. To use this, provide the vendor name and the name of the
-- rule group in this statement. You can retrieve the required names by
-- calling ListAvailableManagedRuleGroups.
--
-- You cannot nest a @ManagedRuleGroupStatement@, for example for use
-- inside a @NotStatement@ or @OrStatement@. It can only be referenced as a
-- top-level statement within a rule.
--
-- You are charged additional fees when you use the WAF Bot Control managed
-- rule group @AWSManagedRulesBotControlRuleSet@, the WAF Fraud Control
-- account takeover prevention (ATP) managed rule group
-- @AWSManagedRulesATPRuleSet@, or the WAF Fraud Control account creation
-- fraud prevention (ACFP) managed rule group @AWSManagedRulesACFPRuleSet@.
-- For more information, see
-- <http://aws.amazon.com/waf/pricing/ WAF Pricing>.
--
-- 'notStatement', 'notStatement' - A logical rule statement used to negate the results of another rule
-- statement. You provide one Statement within the @NotStatement@.
--
-- 'orStatement', 'orStatement' - A logical rule statement used to combine other rule statements with OR
-- logic. You provide more than one Statement within the @OrStatement@.
--
-- 'rateBasedStatement', 'rateBasedStatement' - A rate-based rule counts incoming requests and rate limits requests when
-- they are coming at too fast a rate. The rule categorizes requests
-- according to your aggregation criteria, collects them into aggregation
-- instances, and counts and rate limits the requests for each instance.
--
-- You can specify individual aggregation keys, like IP address or HTTP
-- method. You can also specify aggregation key combinations, like IP
-- address and HTTP method, or HTTP method, query argument, and cookie.
--
-- Each unique set of values for the aggregation keys that you specify is a
-- separate aggregation instance, with the value from each key contributing
-- to the aggregation instance definition.
--
-- For example, assume the rule evaluates web requests with the following
-- IP address and HTTP method values:
--
-- -   IP address 10.1.1.1, HTTP method POST
--
-- -   IP address 10.1.1.1, HTTP method GET
--
-- -   IP address 127.0.0.0, HTTP method POST
--
-- -   IP address 10.1.1.1, HTTP method GET
--
-- The rule would create different aggregation instances according to your
-- aggregation criteria, for example:
--
-- -   If the aggregation criteria is just the IP address, then each
--     individual address is an aggregation instance, and WAF counts
--     requests separately for each. The aggregation instances and request
--     counts for our example would be the following:
--
--     -   IP address 10.1.1.1: count 3
--
--     -   IP address 127.0.0.0: count 1
--
-- -   If the aggregation criteria is HTTP method, then each individual
--     HTTP method is an aggregation instance. The aggregation instances
--     and request counts for our example would be the following:
--
--     -   HTTP method POST: count 2
--
--     -   HTTP method GET: count 2
--
-- -   If the aggregation criteria is IP address and HTTP method, then each
--     IP address and each HTTP method would contribute to the combined
--     aggregation instance. The aggregation instances and request counts
--     for our example would be the following:
--
--     -   IP address 10.1.1.1, HTTP method POST: count 1
--
--     -   IP address 10.1.1.1, HTTP method GET: count 2
--
--     -   IP address 127.0.0.0, HTTP method POST: count 1
--
-- For any n-tuple of aggregation keys, each unique combination of values
-- for the keys defines a separate aggregation instance, which WAF counts
-- and rate-limits individually.
--
-- You can optionally nest another statement inside the rate-based
-- statement, to narrow the scope of the rule so that it only counts and
-- rate limits requests that match the nested statement. You can use this
-- nested scope-down statement in conjunction with your aggregation key
-- specifications or you can just count and rate limit all requests that
-- match the scope-down statement, without additional aggregation. When you
-- choose to just manage all requests that match a scope-down statement,
-- the aggregation instance is singular for the rule.
--
-- You cannot nest a @RateBasedStatement@ inside another statement, for
-- example inside a @NotStatement@ or @OrStatement@. You can define a
-- @RateBasedStatement@ inside a web ACL and inside a rule group.
--
-- For additional information about the options, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-rate-based-rules.html Rate limiting web requests using rate-based rules>
-- in the /WAF Developer Guide/.
--
-- If you only aggregate on the individual IP address or forwarded IP
-- address, you can retrieve the list of IP addresses that WAF is currently
-- rate limiting for a rule through the API call
-- @GetRateBasedStatementManagedKeys@. This option is not available for
-- other aggregation configurations.
--
-- WAF tracks and manages web requests separately for each instance of a
-- rate-based rule that you use. For example, if you provide the same
-- rate-based rule settings in two web ACLs, each of the two rule
-- statements represents a separate instance of the rate-based rule and
-- gets its own tracking and management by WAF. If you define a rate-based
-- rule inside a rule group, and then use that rule group in multiple
-- places, each use creates a separate instance of the rate-based rule that
-- gets its own tracking and management by WAF.
--
-- 'regexMatchStatement', 'regexMatchStatement' - A rule statement used to search web request components for a match
-- against a single regular expression.
--
-- 'regexPatternSetReferenceStatement', 'regexPatternSetReferenceStatement' - A rule statement used to search web request components for matches with
-- regular expressions. To use this, create a RegexPatternSet that
-- specifies the expressions that you want to detect, then use the ARN of
-- that set in this statement. A web request matches the pattern set rule
-- statement if the request component matches any of the patterns in the
-- set. To create a regex pattern set, see CreateRegexPatternSet.
--
-- Each regex pattern set rule statement references a regex pattern set.
-- You create and maintain the set independent of your rules. This allows
-- you to use the single set in multiple rules. When you update the
-- referenced set, WAF automatically updates all rules that reference it.
--
-- 'ruleGroupReferenceStatement', 'ruleGroupReferenceStatement' - A rule statement used to run the rules that are defined in a RuleGroup.
-- To use this, create a rule group with your rules, then provide the ARN
-- of the rule group in this statement.
--
-- You cannot nest a @RuleGroupReferenceStatement@, for example for use
-- inside a @NotStatement@ or @OrStatement@. You can only use a rule group
-- reference statement at the top level inside a web ACL.
--
-- 'sizeConstraintStatement', 'sizeConstraintStatement' - A rule statement that compares a number of bytes against the size of a
-- request component, using a comparison operator, such as greater than (>)
-- or less than (\<). For example, you can use a size constraint statement
-- to look for query strings that are longer than 100 bytes.
--
-- If you configure WAF to inspect the request body, WAF inspects only the
-- number of bytes of the body up to the limit for the web ACL. By default,
-- for regional web ACLs, this limit is 8 KB (8,192 kilobytes) and for
-- CloudFront web ACLs, this limit is 16 KB (16,384 kilobytes). For
-- CloudFront web ACLs, you can increase the limit in the web ACL
-- @AssociationConfig@, for additional fees. If you know that the request
-- body for your web requests should never exceed the inspection limit, you
-- could use a size constraint statement to block requests that have a
-- larger request body size.
--
-- If you choose URI for the value of Part of the request to filter on, the
-- slash (\/) in the URI counts as one character. For example, the URI
-- @\/logo.jpg@ is nine characters long.
--
-- 'sqliMatchStatement', 'sqliMatchStatement' - A rule statement that inspects for malicious SQL code. Attackers insert
-- malicious SQL code into web requests to do things like modify your
-- database or extract data from it.
--
-- 'xssMatchStatement', 'xssMatchStatement' - A rule statement that inspects for cross-site scripting (XSS) attacks.
-- In XSS attacks, the attacker uses vulnerabilities in a benign website as
-- a vehicle to inject malicious client-site scripts into other legitimate
-- web browsers.
newStatement ::
  Statement
newStatement =
  Statement'
    { andStatement = Prelude.Nothing,
      byteMatchStatement = Prelude.Nothing,
      geoMatchStatement = Prelude.Nothing,
      iPSetReferenceStatement = Prelude.Nothing,
      labelMatchStatement = Prelude.Nothing,
      managedRuleGroupStatement = Prelude.Nothing,
      notStatement = Prelude.Nothing,
      orStatement = Prelude.Nothing,
      rateBasedStatement = Prelude.Nothing,
      regexMatchStatement = Prelude.Nothing,
      regexPatternSetReferenceStatement = Prelude.Nothing,
      ruleGroupReferenceStatement = Prelude.Nothing,
      sizeConstraintStatement = Prelude.Nothing,
      sqliMatchStatement = Prelude.Nothing,
      xssMatchStatement = Prelude.Nothing
    }

-- | A logical rule statement used to combine other rule statements with AND
-- logic. You provide more than one Statement within the @AndStatement@.
andStatement :: Lens.Lens' Statement (Prelude.Maybe AndStatement)
andStatement = Lens.lens (\Statement' {andStatement} -> andStatement) (\s@Statement' {} a -> s {andStatement = a} :: Statement)

-- | A rule statement that defines a string match search for WAF to apply to
-- web requests. The byte match statement provides the bytes to search for,
-- the location in requests that you want WAF to search, and other
-- settings. The bytes to search for are typically a string that
-- corresponds with ASCII characters. In the WAF console and the developer
-- guide, this is called a string match statement.
byteMatchStatement :: Lens.Lens' Statement (Prelude.Maybe ByteMatchStatement)
byteMatchStatement = Lens.lens (\Statement' {byteMatchStatement} -> byteMatchStatement) (\s@Statement' {} a -> s {byteMatchStatement = a} :: Statement)

-- | A rule statement that labels web requests by country and region and that
-- matches against web requests based on country code. A geo match rule
-- labels every request that it inspects regardless of whether it finds a
-- match.
--
-- -   To manage requests only by country, you can use this statement by
--     itself and specify the countries that you want to match against in
--     the @CountryCodes@ array.
--
-- -   Otherwise, configure your geo match rule with Count action so that
--     it only labels requests. Then, add one or more label match rules to
--     run after the geo match rule and configure them to match against the
--     geographic labels and handle the requests as needed.
--
-- WAF labels requests using the alpha-2 country and region codes from the
-- International Organization for Standardization (ISO) 3166 standard. WAF
-- determines the codes using either the IP address in the web request
-- origin or, if you specify it, the address in the geo match
-- @ForwardedIPConfig@.
--
-- If you use the web request origin, the label formats are
-- @awswaf:clientip:geo:region:\<ISO country code>-\<ISO region code>@ and
-- @awswaf:clientip:geo:country:\<ISO country code>@.
--
-- If you use a forwarded IP address, the label formats are
-- @awswaf:forwardedip:geo:region:\<ISO country code>-\<ISO region code>@
-- and @awswaf:forwardedip:geo:country:\<ISO country code>@.
--
-- For additional details, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-rule-statement-type-geo-match.html Geographic match rule statement>
-- in the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html WAF Developer Guide>.
geoMatchStatement :: Lens.Lens' Statement (Prelude.Maybe GeoMatchStatement)
geoMatchStatement = Lens.lens (\Statement' {geoMatchStatement} -> geoMatchStatement) (\s@Statement' {} a -> s {geoMatchStatement = a} :: Statement)

-- | A rule statement used to detect web requests coming from particular IP
-- addresses or address ranges. To use this, create an IPSet that specifies
-- the addresses you want to detect, then use the ARN of that set in this
-- statement. To create an IP set, see CreateIPSet.
--
-- Each IP set rule statement references an IP set. You create and maintain
-- the set independent of your rules. This allows you to use the single set
-- in multiple rules. When you update the referenced set, WAF automatically
-- updates all rules that reference it.
iPSetReferenceStatement :: Lens.Lens' Statement (Prelude.Maybe IPSetReferenceStatement)
iPSetReferenceStatement = Lens.lens (\Statement' {iPSetReferenceStatement} -> iPSetReferenceStatement) (\s@Statement' {} a -> s {iPSetReferenceStatement = a} :: Statement)

-- | A rule statement to match against labels that have been added to the web
-- request by rules that have already run in the web ACL.
--
-- The label match statement provides the label or namespace string to
-- search for. The label string can represent a part or all of the fully
-- qualified label name that had been added to the web request. Fully
-- qualified labels have a prefix, optional namespaces, and label name. The
-- prefix identifies the rule group or web ACL context of the rule that
-- added the label. If you do not provide the fully qualified name in your
-- label match string, WAF performs the search for labels that were added
-- in the same context as the label match statement.
labelMatchStatement :: Lens.Lens' Statement (Prelude.Maybe LabelMatchStatement)
labelMatchStatement = Lens.lens (\Statement' {labelMatchStatement} -> labelMatchStatement) (\s@Statement' {} a -> s {labelMatchStatement = a} :: Statement)

-- | A rule statement used to run the rules that are defined in a managed
-- rule group. To use this, provide the vendor name and the name of the
-- rule group in this statement. You can retrieve the required names by
-- calling ListAvailableManagedRuleGroups.
--
-- You cannot nest a @ManagedRuleGroupStatement@, for example for use
-- inside a @NotStatement@ or @OrStatement@. It can only be referenced as a
-- top-level statement within a rule.
--
-- You are charged additional fees when you use the WAF Bot Control managed
-- rule group @AWSManagedRulesBotControlRuleSet@, the WAF Fraud Control
-- account takeover prevention (ATP) managed rule group
-- @AWSManagedRulesATPRuleSet@, or the WAF Fraud Control account creation
-- fraud prevention (ACFP) managed rule group @AWSManagedRulesACFPRuleSet@.
-- For more information, see
-- <http://aws.amazon.com/waf/pricing/ WAF Pricing>.
managedRuleGroupStatement :: Lens.Lens' Statement (Prelude.Maybe ManagedRuleGroupStatement)
managedRuleGroupStatement = Lens.lens (\Statement' {managedRuleGroupStatement} -> managedRuleGroupStatement) (\s@Statement' {} a -> s {managedRuleGroupStatement = a} :: Statement)

-- | A logical rule statement used to negate the results of another rule
-- statement. You provide one Statement within the @NotStatement@.
notStatement :: Lens.Lens' Statement (Prelude.Maybe NotStatement)
notStatement = Lens.lens (\Statement' {notStatement} -> notStatement) (\s@Statement' {} a -> s {notStatement = a} :: Statement)

-- | A logical rule statement used to combine other rule statements with OR
-- logic. You provide more than one Statement within the @OrStatement@.
orStatement :: Lens.Lens' Statement (Prelude.Maybe OrStatement)
orStatement = Lens.lens (\Statement' {orStatement} -> orStatement) (\s@Statement' {} a -> s {orStatement = a} :: Statement)

-- | A rate-based rule counts incoming requests and rate limits requests when
-- they are coming at too fast a rate. The rule categorizes requests
-- according to your aggregation criteria, collects them into aggregation
-- instances, and counts and rate limits the requests for each instance.
--
-- You can specify individual aggregation keys, like IP address or HTTP
-- method. You can also specify aggregation key combinations, like IP
-- address and HTTP method, or HTTP method, query argument, and cookie.
--
-- Each unique set of values for the aggregation keys that you specify is a
-- separate aggregation instance, with the value from each key contributing
-- to the aggregation instance definition.
--
-- For example, assume the rule evaluates web requests with the following
-- IP address and HTTP method values:
--
-- -   IP address 10.1.1.1, HTTP method POST
--
-- -   IP address 10.1.1.1, HTTP method GET
--
-- -   IP address 127.0.0.0, HTTP method POST
--
-- -   IP address 10.1.1.1, HTTP method GET
--
-- The rule would create different aggregation instances according to your
-- aggregation criteria, for example:
--
-- -   If the aggregation criteria is just the IP address, then each
--     individual address is an aggregation instance, and WAF counts
--     requests separately for each. The aggregation instances and request
--     counts for our example would be the following:
--
--     -   IP address 10.1.1.1: count 3
--
--     -   IP address 127.0.0.0: count 1
--
-- -   If the aggregation criteria is HTTP method, then each individual
--     HTTP method is an aggregation instance. The aggregation instances
--     and request counts for our example would be the following:
--
--     -   HTTP method POST: count 2
--
--     -   HTTP method GET: count 2
--
-- -   If the aggregation criteria is IP address and HTTP method, then each
--     IP address and each HTTP method would contribute to the combined
--     aggregation instance. The aggregation instances and request counts
--     for our example would be the following:
--
--     -   IP address 10.1.1.1, HTTP method POST: count 1
--
--     -   IP address 10.1.1.1, HTTP method GET: count 2
--
--     -   IP address 127.0.0.0, HTTP method POST: count 1
--
-- For any n-tuple of aggregation keys, each unique combination of values
-- for the keys defines a separate aggregation instance, which WAF counts
-- and rate-limits individually.
--
-- You can optionally nest another statement inside the rate-based
-- statement, to narrow the scope of the rule so that it only counts and
-- rate limits requests that match the nested statement. You can use this
-- nested scope-down statement in conjunction with your aggregation key
-- specifications or you can just count and rate limit all requests that
-- match the scope-down statement, without additional aggregation. When you
-- choose to just manage all requests that match a scope-down statement,
-- the aggregation instance is singular for the rule.
--
-- You cannot nest a @RateBasedStatement@ inside another statement, for
-- example inside a @NotStatement@ or @OrStatement@. You can define a
-- @RateBasedStatement@ inside a web ACL and inside a rule group.
--
-- For additional information about the options, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-rate-based-rules.html Rate limiting web requests using rate-based rules>
-- in the /WAF Developer Guide/.
--
-- If you only aggregate on the individual IP address or forwarded IP
-- address, you can retrieve the list of IP addresses that WAF is currently
-- rate limiting for a rule through the API call
-- @GetRateBasedStatementManagedKeys@. This option is not available for
-- other aggregation configurations.
--
-- WAF tracks and manages web requests separately for each instance of a
-- rate-based rule that you use. For example, if you provide the same
-- rate-based rule settings in two web ACLs, each of the two rule
-- statements represents a separate instance of the rate-based rule and
-- gets its own tracking and management by WAF. If you define a rate-based
-- rule inside a rule group, and then use that rule group in multiple
-- places, each use creates a separate instance of the rate-based rule that
-- gets its own tracking and management by WAF.
rateBasedStatement :: Lens.Lens' Statement (Prelude.Maybe RateBasedStatement)
rateBasedStatement = Lens.lens (\Statement' {rateBasedStatement} -> rateBasedStatement) (\s@Statement' {} a -> s {rateBasedStatement = a} :: Statement)

-- | A rule statement used to search web request components for a match
-- against a single regular expression.
regexMatchStatement :: Lens.Lens' Statement (Prelude.Maybe RegexMatchStatement)
regexMatchStatement = Lens.lens (\Statement' {regexMatchStatement} -> regexMatchStatement) (\s@Statement' {} a -> s {regexMatchStatement = a} :: Statement)

-- | A rule statement used to search web request components for matches with
-- regular expressions. To use this, create a RegexPatternSet that
-- specifies the expressions that you want to detect, then use the ARN of
-- that set in this statement. A web request matches the pattern set rule
-- statement if the request component matches any of the patterns in the
-- set. To create a regex pattern set, see CreateRegexPatternSet.
--
-- Each regex pattern set rule statement references a regex pattern set.
-- You create and maintain the set independent of your rules. This allows
-- you to use the single set in multiple rules. When you update the
-- referenced set, WAF automatically updates all rules that reference it.
regexPatternSetReferenceStatement :: Lens.Lens' Statement (Prelude.Maybe RegexPatternSetReferenceStatement)
regexPatternSetReferenceStatement = Lens.lens (\Statement' {regexPatternSetReferenceStatement} -> regexPatternSetReferenceStatement) (\s@Statement' {} a -> s {regexPatternSetReferenceStatement = a} :: Statement)

-- | A rule statement used to run the rules that are defined in a RuleGroup.
-- To use this, create a rule group with your rules, then provide the ARN
-- of the rule group in this statement.
--
-- You cannot nest a @RuleGroupReferenceStatement@, for example for use
-- inside a @NotStatement@ or @OrStatement@. You can only use a rule group
-- reference statement at the top level inside a web ACL.
ruleGroupReferenceStatement :: Lens.Lens' Statement (Prelude.Maybe RuleGroupReferenceStatement)
ruleGroupReferenceStatement = Lens.lens (\Statement' {ruleGroupReferenceStatement} -> ruleGroupReferenceStatement) (\s@Statement' {} a -> s {ruleGroupReferenceStatement = a} :: Statement)

-- | A rule statement that compares a number of bytes against the size of a
-- request component, using a comparison operator, such as greater than (>)
-- or less than (\<). For example, you can use a size constraint statement
-- to look for query strings that are longer than 100 bytes.
--
-- If you configure WAF to inspect the request body, WAF inspects only the
-- number of bytes of the body up to the limit for the web ACL. By default,
-- for regional web ACLs, this limit is 8 KB (8,192 kilobytes) and for
-- CloudFront web ACLs, this limit is 16 KB (16,384 kilobytes). For
-- CloudFront web ACLs, you can increase the limit in the web ACL
-- @AssociationConfig@, for additional fees. If you know that the request
-- body for your web requests should never exceed the inspection limit, you
-- could use a size constraint statement to block requests that have a
-- larger request body size.
--
-- If you choose URI for the value of Part of the request to filter on, the
-- slash (\/) in the URI counts as one character. For example, the URI
-- @\/logo.jpg@ is nine characters long.
sizeConstraintStatement :: Lens.Lens' Statement (Prelude.Maybe SizeConstraintStatement)
sizeConstraintStatement = Lens.lens (\Statement' {sizeConstraintStatement} -> sizeConstraintStatement) (\s@Statement' {} a -> s {sizeConstraintStatement = a} :: Statement)

-- | A rule statement that inspects for malicious SQL code. Attackers insert
-- malicious SQL code into web requests to do things like modify your
-- database or extract data from it.
sqliMatchStatement :: Lens.Lens' Statement (Prelude.Maybe SqliMatchStatement)
sqliMatchStatement = Lens.lens (\Statement' {sqliMatchStatement} -> sqliMatchStatement) (\s@Statement' {} a -> s {sqliMatchStatement = a} :: Statement)

-- | A rule statement that inspects for cross-site scripting (XSS) attacks.
-- In XSS attacks, the attacker uses vulnerabilities in a benign website as
-- a vehicle to inject malicious client-site scripts into other legitimate
-- web browsers.
xssMatchStatement :: Lens.Lens' Statement (Prelude.Maybe XssMatchStatement)
xssMatchStatement = Lens.lens (\Statement' {xssMatchStatement} -> xssMatchStatement) (\s@Statement' {} a -> s {xssMatchStatement = a} :: Statement)

instance Data.FromJSON Statement where
  parseJSON =
    Data.withObject
      "Statement"
      ( \x ->
          Statement'
            Prelude.<$> (x Data..:? "AndStatement")
            Prelude.<*> (x Data..:? "ByteMatchStatement")
            Prelude.<*> (x Data..:? "GeoMatchStatement")
            Prelude.<*> (x Data..:? "IPSetReferenceStatement")
            Prelude.<*> (x Data..:? "LabelMatchStatement")
            Prelude.<*> (x Data..:? "ManagedRuleGroupStatement")
            Prelude.<*> (x Data..:? "NotStatement")
            Prelude.<*> (x Data..:? "OrStatement")
            Prelude.<*> (x Data..:? "RateBasedStatement")
            Prelude.<*> (x Data..:? "RegexMatchStatement")
            Prelude.<*> (x Data..:? "RegexPatternSetReferenceStatement")
            Prelude.<*> (x Data..:? "RuleGroupReferenceStatement")
            Prelude.<*> (x Data..:? "SizeConstraintStatement")
            Prelude.<*> (x Data..:? "SqliMatchStatement")
            Prelude.<*> (x Data..:? "XssMatchStatement")
      )

instance Prelude.Hashable Statement where
  hashWithSalt _salt Statement' {..} =
    _salt
      `Prelude.hashWithSalt` andStatement
      `Prelude.hashWithSalt` byteMatchStatement
      `Prelude.hashWithSalt` geoMatchStatement
      `Prelude.hashWithSalt` iPSetReferenceStatement
      `Prelude.hashWithSalt` labelMatchStatement
      `Prelude.hashWithSalt` managedRuleGroupStatement
      `Prelude.hashWithSalt` notStatement
      `Prelude.hashWithSalt` orStatement
      `Prelude.hashWithSalt` rateBasedStatement
      `Prelude.hashWithSalt` regexMatchStatement
      `Prelude.hashWithSalt` regexPatternSetReferenceStatement
      `Prelude.hashWithSalt` ruleGroupReferenceStatement
      `Prelude.hashWithSalt` sizeConstraintStatement
      `Prelude.hashWithSalt` sqliMatchStatement
      `Prelude.hashWithSalt` xssMatchStatement

instance Prelude.NFData Statement where
  rnf Statement' {..} =
    Prelude.rnf andStatement
      `Prelude.seq` Prelude.rnf byteMatchStatement
      `Prelude.seq` Prelude.rnf geoMatchStatement
      `Prelude.seq` Prelude.rnf iPSetReferenceStatement
      `Prelude.seq` Prelude.rnf labelMatchStatement
      `Prelude.seq` Prelude.rnf managedRuleGroupStatement
      `Prelude.seq` Prelude.rnf notStatement
      `Prelude.seq` Prelude.rnf orStatement
      `Prelude.seq` Prelude.rnf rateBasedStatement
      `Prelude.seq` Prelude.rnf regexMatchStatement
      `Prelude.seq` Prelude.rnf regexPatternSetReferenceStatement
      `Prelude.seq` Prelude.rnf ruleGroupReferenceStatement
      `Prelude.seq` Prelude.rnf sizeConstraintStatement
      `Prelude.seq` Prelude.rnf sqliMatchStatement
      `Prelude.seq` Prelude.rnf xssMatchStatement

instance Data.ToJSON Statement where
  toJSON Statement' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AndStatement" Data..=) Prelude.<$> andStatement,
            ("ByteMatchStatement" Data..=)
              Prelude.<$> byteMatchStatement,
            ("GeoMatchStatement" Data..=)
              Prelude.<$> geoMatchStatement,
            ("IPSetReferenceStatement" Data..=)
              Prelude.<$> iPSetReferenceStatement,
            ("LabelMatchStatement" Data..=)
              Prelude.<$> labelMatchStatement,
            ("ManagedRuleGroupStatement" Data..=)
              Prelude.<$> managedRuleGroupStatement,
            ("NotStatement" Data..=) Prelude.<$> notStatement,
            ("OrStatement" Data..=) Prelude.<$> orStatement,
            ("RateBasedStatement" Data..=)
              Prelude.<$> rateBasedStatement,
            ("RegexMatchStatement" Data..=)
              Prelude.<$> regexMatchStatement,
            ("RegexPatternSetReferenceStatement" Data..=)
              Prelude.<$> regexPatternSetReferenceStatement,
            ("RuleGroupReferenceStatement" Data..=)
              Prelude.<$> ruleGroupReferenceStatement,
            ("SizeConstraintStatement" Data..=)
              Prelude.<$> sizeConstraintStatement,
            ("SqliMatchStatement" Data..=)
              Prelude.<$> sqliMatchStatement,
            ("XssMatchStatement" Data..=)
              Prelude.<$> xssMatchStatement
          ]
      )
