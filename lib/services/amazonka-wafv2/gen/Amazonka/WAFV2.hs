{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.WAFV2
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2019-07-29@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- WAF
--
-- This is the latest version of the __WAF__ API, released in November,
-- 2019. The names of the entities that you use to access this API, like
-- endpoints and namespaces, all have the versioning information added,
-- like \"V2\" or \"v2\", to distinguish from the prior version. We
-- recommend migrating your resources to this version, because it has a
-- number of significant improvements.
--
-- If you used WAF prior to this release, you can\'t use this WAFV2 API to
-- access any WAF resources that you created before. You can access your
-- old rules, web ACLs, and other WAF resources only through the WAF
-- Classic APIs. The WAF Classic APIs have retained the prior names,
-- endpoints, and namespaces.
--
-- For information, including how to migrate your WAF resources to this
-- version, see the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html WAF Developer Guide>.
--
-- WAF is a web application firewall that lets you monitor the HTTP and
-- HTTPS requests that are forwarded to Amazon CloudFront, an Amazon API
-- Gateway REST API, an Application Load Balancer, an AppSync GraphQL API,
-- or an Amazon Cognito user pool. WAF also lets you control access to your
-- content. Based on conditions that you specify, such as the IP addresses
-- that requests originate from or the values of query strings, the Amazon
-- API Gateway REST API, CloudFront distribution, the Application Load
-- Balancer, the AppSync GraphQL API, or the Amazon Cognito user pool
-- responds to requests either with the requested content or with an HTTP
-- 403 status code (Forbidden). You also can configure CloudFront to return
-- a custom error page when a request is blocked.
--
-- This API guide is for developers who need detailed information about WAF
-- API actions, data types, and errors. For detailed information about WAF
-- features and an overview of how to use WAF, see the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/what-is-aws-waf.html WAF Developer Guide>.
--
-- You can make calls using the endpoints listed in
-- <https://docs.aws.amazon.com/general/latest/gr/waf.html WAF endpoints and quotas>.
--
-- -   For regional applications, you can use any of the endpoints in the
--     list. A regional application can be an Application Load Balancer
--     (ALB), an Amazon API Gateway REST API, an AppSync GraphQL API, or an
--     Amazon Cognito user pool.
--
-- -   For Amazon CloudFront applications, you must use the API endpoint
--     listed for US East (N. Virginia): us-east-1.
--
-- Alternatively, you can use one of the Amazon Web Services SDKs to access
-- an API that\'s tailored to the programming language or platform that
-- you\'re using. For more information, see
-- <http://aws.amazon.com/tools/#SDKs Amazon Web Services SDKs>.
--
-- We currently provide two versions of the WAF API: this API and the prior
-- versions, the classic WAF APIs. This new API provides the same
-- functionality as the older versions, with the following major
-- improvements:
--
-- -   You use one API for both global and regional applications. Where you
--     need to distinguish the scope, you specify a @Scope@ parameter and
--     set it to @CLOUDFRONT@ or @REGIONAL@.
--
-- -   You can define a web ACL or rule group with a single call, and
--     update it with a single call. You define all rule specifications in
--     JSON format, and pass them to your rule group or web ACL calls.
--
-- -   The limits WAF places on the use of rules more closely reflects the
--     cost of running each type of rule. Rule groups include capacity
--     settings, so you know the maximum cost of a rule group when you use
--     it.
module Amazonka.WAFV2
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** WAFAssociatedItemException
    _WAFAssociatedItemException,

    -- ** WAFConfigurationWarningException
    _WAFConfigurationWarningException,

    -- ** WAFDuplicateItemException
    _WAFDuplicateItemException,

    -- ** WAFExpiredManagedRuleGroupVersionException
    _WAFExpiredManagedRuleGroupVersionException,

    -- ** WAFInternalErrorException
    _WAFInternalErrorException,

    -- ** WAFInvalidOperationException
    _WAFInvalidOperationException,

    -- ** WAFInvalidParameterException
    _WAFInvalidParameterException,

    -- ** WAFInvalidPermissionPolicyException
    _WAFInvalidPermissionPolicyException,

    -- ** WAFInvalidResourceException
    _WAFInvalidResourceException,

    -- ** WAFLimitsExceededException
    _WAFLimitsExceededException,

    -- ** WAFLogDestinationPermissionIssueException
    _WAFLogDestinationPermissionIssueException,

    -- ** WAFNonexistentItemException
    _WAFNonexistentItemException,

    -- ** WAFOptimisticLockException
    _WAFOptimisticLockException,

    -- ** WAFServiceLinkedRoleErrorException
    _WAFServiceLinkedRoleErrorException,

    -- ** WAFSubscriptionNotFoundException
    _WAFSubscriptionNotFoundException,

    -- ** WAFTagOperationException
    _WAFTagOperationException,

    -- ** WAFTagOperationInternalErrorException
    _WAFTagOperationInternalErrorException,

    -- ** WAFUnavailableEntityException
    _WAFUnavailableEntityException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** AssociateWebACL
    AssociateWebACL (AssociateWebACL'),
    newAssociateWebACL,
    AssociateWebACLResponse (AssociateWebACLResponse'),
    newAssociateWebACLResponse,

    -- ** CheckCapacity
    CheckCapacity (CheckCapacity'),
    newCheckCapacity,
    CheckCapacityResponse (CheckCapacityResponse'),
    newCheckCapacityResponse,

    -- ** CreateIPSet
    CreateIPSet (CreateIPSet'),
    newCreateIPSet,
    CreateIPSetResponse (CreateIPSetResponse'),
    newCreateIPSetResponse,

    -- ** CreateRegexPatternSet
    CreateRegexPatternSet (CreateRegexPatternSet'),
    newCreateRegexPatternSet,
    CreateRegexPatternSetResponse (CreateRegexPatternSetResponse'),
    newCreateRegexPatternSetResponse,

    -- ** CreateRuleGroup
    CreateRuleGroup (CreateRuleGroup'),
    newCreateRuleGroup,
    CreateRuleGroupResponse (CreateRuleGroupResponse'),
    newCreateRuleGroupResponse,

    -- ** CreateWebACL
    CreateWebACL (CreateWebACL'),
    newCreateWebACL,
    CreateWebACLResponse (CreateWebACLResponse'),
    newCreateWebACLResponse,

    -- ** DeleteFirewallManagerRuleGroups
    DeleteFirewallManagerRuleGroups (DeleteFirewallManagerRuleGroups'),
    newDeleteFirewallManagerRuleGroups,
    DeleteFirewallManagerRuleGroupsResponse (DeleteFirewallManagerRuleGroupsResponse'),
    newDeleteFirewallManagerRuleGroupsResponse,

    -- ** DeleteIPSet
    DeleteIPSet (DeleteIPSet'),
    newDeleteIPSet,
    DeleteIPSetResponse (DeleteIPSetResponse'),
    newDeleteIPSetResponse,

    -- ** DeleteLoggingConfiguration
    DeleteLoggingConfiguration (DeleteLoggingConfiguration'),
    newDeleteLoggingConfiguration,
    DeleteLoggingConfigurationResponse (DeleteLoggingConfigurationResponse'),
    newDeleteLoggingConfigurationResponse,

    -- ** DeletePermissionPolicy
    DeletePermissionPolicy (DeletePermissionPolicy'),
    newDeletePermissionPolicy,
    DeletePermissionPolicyResponse (DeletePermissionPolicyResponse'),
    newDeletePermissionPolicyResponse,

    -- ** DeleteRegexPatternSet
    DeleteRegexPatternSet (DeleteRegexPatternSet'),
    newDeleteRegexPatternSet,
    DeleteRegexPatternSetResponse (DeleteRegexPatternSetResponse'),
    newDeleteRegexPatternSetResponse,

    -- ** DeleteRuleGroup
    DeleteRuleGroup (DeleteRuleGroup'),
    newDeleteRuleGroup,
    DeleteRuleGroupResponse (DeleteRuleGroupResponse'),
    newDeleteRuleGroupResponse,

    -- ** DeleteWebACL
    DeleteWebACL (DeleteWebACL'),
    newDeleteWebACL,
    DeleteWebACLResponse (DeleteWebACLResponse'),
    newDeleteWebACLResponse,

    -- ** DescribeManagedRuleGroup
    DescribeManagedRuleGroup (DescribeManagedRuleGroup'),
    newDescribeManagedRuleGroup,
    DescribeManagedRuleGroupResponse (DescribeManagedRuleGroupResponse'),
    newDescribeManagedRuleGroupResponse,

    -- ** DisassociateWebACL
    DisassociateWebACL (DisassociateWebACL'),
    newDisassociateWebACL,
    DisassociateWebACLResponse (DisassociateWebACLResponse'),
    newDisassociateWebACLResponse,

    -- ** GenerateMobileSdkReleaseUrl
    GenerateMobileSdkReleaseUrl (GenerateMobileSdkReleaseUrl'),
    newGenerateMobileSdkReleaseUrl,
    GenerateMobileSdkReleaseUrlResponse (GenerateMobileSdkReleaseUrlResponse'),
    newGenerateMobileSdkReleaseUrlResponse,

    -- ** GetIPSet
    GetIPSet (GetIPSet'),
    newGetIPSet,
    GetIPSetResponse (GetIPSetResponse'),
    newGetIPSetResponse,

    -- ** GetLoggingConfiguration
    GetLoggingConfiguration (GetLoggingConfiguration'),
    newGetLoggingConfiguration,
    GetLoggingConfigurationResponse (GetLoggingConfigurationResponse'),
    newGetLoggingConfigurationResponse,

    -- ** GetManagedRuleSet
    GetManagedRuleSet (GetManagedRuleSet'),
    newGetManagedRuleSet,
    GetManagedRuleSetResponse (GetManagedRuleSetResponse'),
    newGetManagedRuleSetResponse,

    -- ** GetMobileSdkRelease
    GetMobileSdkRelease (GetMobileSdkRelease'),
    newGetMobileSdkRelease,
    GetMobileSdkReleaseResponse (GetMobileSdkReleaseResponse'),
    newGetMobileSdkReleaseResponse,

    -- ** GetPermissionPolicy
    GetPermissionPolicy (GetPermissionPolicy'),
    newGetPermissionPolicy,
    GetPermissionPolicyResponse (GetPermissionPolicyResponse'),
    newGetPermissionPolicyResponse,

    -- ** GetRateBasedStatementManagedKeys
    GetRateBasedStatementManagedKeys (GetRateBasedStatementManagedKeys'),
    newGetRateBasedStatementManagedKeys,
    GetRateBasedStatementManagedKeysResponse (GetRateBasedStatementManagedKeysResponse'),
    newGetRateBasedStatementManagedKeysResponse,

    -- ** GetRegexPatternSet
    GetRegexPatternSet (GetRegexPatternSet'),
    newGetRegexPatternSet,
    GetRegexPatternSetResponse (GetRegexPatternSetResponse'),
    newGetRegexPatternSetResponse,

    -- ** GetRuleGroup
    GetRuleGroup (GetRuleGroup'),
    newGetRuleGroup,
    GetRuleGroupResponse (GetRuleGroupResponse'),
    newGetRuleGroupResponse,

    -- ** GetSampledRequests
    GetSampledRequests (GetSampledRequests'),
    newGetSampledRequests,
    GetSampledRequestsResponse (GetSampledRequestsResponse'),
    newGetSampledRequestsResponse,

    -- ** GetWebACL
    GetWebACL (GetWebACL'),
    newGetWebACL,
    GetWebACLResponse (GetWebACLResponse'),
    newGetWebACLResponse,

    -- ** GetWebACLForResource
    GetWebACLForResource (GetWebACLForResource'),
    newGetWebACLForResource,
    GetWebACLForResourceResponse (GetWebACLForResourceResponse'),
    newGetWebACLForResourceResponse,

    -- ** ListAvailableManagedRuleGroupVersions
    ListAvailableManagedRuleGroupVersions (ListAvailableManagedRuleGroupVersions'),
    newListAvailableManagedRuleGroupVersions,
    ListAvailableManagedRuleGroupVersionsResponse (ListAvailableManagedRuleGroupVersionsResponse'),
    newListAvailableManagedRuleGroupVersionsResponse,

    -- ** ListAvailableManagedRuleGroups
    ListAvailableManagedRuleGroups (ListAvailableManagedRuleGroups'),
    newListAvailableManagedRuleGroups,
    ListAvailableManagedRuleGroupsResponse (ListAvailableManagedRuleGroupsResponse'),
    newListAvailableManagedRuleGroupsResponse,

    -- ** ListIPSets
    ListIPSets (ListIPSets'),
    newListIPSets,
    ListIPSetsResponse (ListIPSetsResponse'),
    newListIPSetsResponse,

    -- ** ListLoggingConfigurations
    ListLoggingConfigurations (ListLoggingConfigurations'),
    newListLoggingConfigurations,
    ListLoggingConfigurationsResponse (ListLoggingConfigurationsResponse'),
    newListLoggingConfigurationsResponse,

    -- ** ListManagedRuleSets
    ListManagedRuleSets (ListManagedRuleSets'),
    newListManagedRuleSets,
    ListManagedRuleSetsResponse (ListManagedRuleSetsResponse'),
    newListManagedRuleSetsResponse,

    -- ** ListMobileSdkReleases
    ListMobileSdkReleases (ListMobileSdkReleases'),
    newListMobileSdkReleases,
    ListMobileSdkReleasesResponse (ListMobileSdkReleasesResponse'),
    newListMobileSdkReleasesResponse,

    -- ** ListRegexPatternSets
    ListRegexPatternSets (ListRegexPatternSets'),
    newListRegexPatternSets,
    ListRegexPatternSetsResponse (ListRegexPatternSetsResponse'),
    newListRegexPatternSetsResponse,

    -- ** ListResourcesForWebACL
    ListResourcesForWebACL (ListResourcesForWebACL'),
    newListResourcesForWebACL,
    ListResourcesForWebACLResponse (ListResourcesForWebACLResponse'),
    newListResourcesForWebACLResponse,

    -- ** ListRuleGroups
    ListRuleGroups (ListRuleGroups'),
    newListRuleGroups,
    ListRuleGroupsResponse (ListRuleGroupsResponse'),
    newListRuleGroupsResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** ListWebACLs
    ListWebACLs (ListWebACLs'),
    newListWebACLs,
    ListWebACLsResponse (ListWebACLsResponse'),
    newListWebACLsResponse,

    -- ** PutLoggingConfiguration
    PutLoggingConfiguration (PutLoggingConfiguration'),
    newPutLoggingConfiguration,
    PutLoggingConfigurationResponse (PutLoggingConfigurationResponse'),
    newPutLoggingConfigurationResponse,

    -- ** PutManagedRuleSetVersions
    PutManagedRuleSetVersions (PutManagedRuleSetVersions'),
    newPutManagedRuleSetVersions,
    PutManagedRuleSetVersionsResponse (PutManagedRuleSetVersionsResponse'),
    newPutManagedRuleSetVersionsResponse,

    -- ** PutPermissionPolicy
    PutPermissionPolicy (PutPermissionPolicy'),
    newPutPermissionPolicy,
    PutPermissionPolicyResponse (PutPermissionPolicyResponse'),
    newPutPermissionPolicyResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** UpdateIPSet
    UpdateIPSet (UpdateIPSet'),
    newUpdateIPSet,
    UpdateIPSetResponse (UpdateIPSetResponse'),
    newUpdateIPSetResponse,

    -- ** UpdateManagedRuleSetVersionExpiryDate
    UpdateManagedRuleSetVersionExpiryDate (UpdateManagedRuleSetVersionExpiryDate'),
    newUpdateManagedRuleSetVersionExpiryDate,
    UpdateManagedRuleSetVersionExpiryDateResponse (UpdateManagedRuleSetVersionExpiryDateResponse'),
    newUpdateManagedRuleSetVersionExpiryDateResponse,

    -- ** UpdateRegexPatternSet
    UpdateRegexPatternSet (UpdateRegexPatternSet'),
    newUpdateRegexPatternSet,
    UpdateRegexPatternSetResponse (UpdateRegexPatternSetResponse'),
    newUpdateRegexPatternSetResponse,

    -- ** UpdateRuleGroup
    UpdateRuleGroup (UpdateRuleGroup'),
    newUpdateRuleGroup,
    UpdateRuleGroupResponse (UpdateRuleGroupResponse'),
    newUpdateRuleGroupResponse,

    -- ** UpdateWebACL
    UpdateWebACL (UpdateWebACL'),
    newUpdateWebACL,
    UpdateWebACLResponse (UpdateWebACLResponse'),
    newUpdateWebACLResponse,

    -- * Types

    -- ** ActionValue
    ActionValue (..),

    -- ** BodyParsingFallbackBehavior
    BodyParsingFallbackBehavior (..),

    -- ** ComparisonOperator
    ComparisonOperator (..),

    -- ** CountryCode
    CountryCode (..),

    -- ** FailureReason
    FailureReason (..),

    -- ** FallbackBehavior
    FallbackBehavior (..),

    -- ** FilterBehavior
    FilterBehavior (..),

    -- ** FilterRequirement
    FilterRequirement (..),

    -- ** ForwardedIPPosition
    ForwardedIPPosition (..),

    -- ** IPAddressVersion
    IPAddressVersion (..),

    -- ** InspectionLevel
    InspectionLevel (..),

    -- ** JsonMatchScope
    JsonMatchScope (..),

    -- ** LabelMatchScope
    LabelMatchScope (..),

    -- ** MapMatchScope
    MapMatchScope (..),

    -- ** OversizeHandling
    OversizeHandling (..),

    -- ** PayloadType
    PayloadType (..),

    -- ** Platform
    Platform (..),

    -- ** PositionalConstraint
    PositionalConstraint (..),

    -- ** RateBasedStatementAggregateKeyType
    RateBasedStatementAggregateKeyType (..),

    -- ** ResourceType
    ResourceType (..),

    -- ** ResponseContentType
    ResponseContentType (..),

    -- ** Scope
    Scope (..),

    -- ** SensitivityLevel
    SensitivityLevel (..),

    -- ** TextTransformationType
    TextTransformationType (..),

    -- ** AWSManagedRulesBotControlRuleSet
    AWSManagedRulesBotControlRuleSet (AWSManagedRulesBotControlRuleSet'),
    newAWSManagedRulesBotControlRuleSet,

    -- ** ActionCondition
    ActionCondition (ActionCondition'),
    newActionCondition,

    -- ** All
    All (All'),
    newAll,

    -- ** AllQueryArguments
    AllQueryArguments (AllQueryArguments'),
    newAllQueryArguments,

    -- ** AllowAction
    AllowAction (AllowAction'),
    newAllowAction,

    -- ** AndStatement
    AndStatement (AndStatement'),
    newAndStatement,

    -- ** BlockAction
    BlockAction (BlockAction'),
    newBlockAction,

    -- ** Body
    Body (Body'),
    newBody,

    -- ** ByteMatchStatement
    ByteMatchStatement (ByteMatchStatement'),
    newByteMatchStatement,

    -- ** CaptchaAction
    CaptchaAction (CaptchaAction'),
    newCaptchaAction,

    -- ** CaptchaConfig
    CaptchaConfig (CaptchaConfig'),
    newCaptchaConfig,

    -- ** CaptchaResponse
    CaptchaResponse (CaptchaResponse'),
    newCaptchaResponse,

    -- ** ChallengeAction
    ChallengeAction (ChallengeAction'),
    newChallengeAction,

    -- ** ChallengeConfig
    ChallengeConfig (ChallengeConfig'),
    newChallengeConfig,

    -- ** ChallengeResponse
    ChallengeResponse (ChallengeResponse'),
    newChallengeResponse,

    -- ** Condition
    Condition (Condition'),
    newCondition,

    -- ** CookieMatchPattern
    CookieMatchPattern (CookieMatchPattern'),
    newCookieMatchPattern,

    -- ** Cookies
    Cookies (Cookies'),
    newCookies,

    -- ** CountAction
    CountAction (CountAction'),
    newCountAction,

    -- ** CustomHTTPHeader
    CustomHTTPHeader (CustomHTTPHeader'),
    newCustomHTTPHeader,

    -- ** CustomRequestHandling
    CustomRequestHandling (CustomRequestHandling'),
    newCustomRequestHandling,

    -- ** CustomResponse
    CustomResponse (CustomResponse'),
    newCustomResponse,

    -- ** CustomResponseBody
    CustomResponseBody (CustomResponseBody'),
    newCustomResponseBody,

    -- ** DefaultAction
    DefaultAction (DefaultAction'),
    newDefaultAction,

    -- ** ExcludedRule
    ExcludedRule (ExcludedRule'),
    newExcludedRule,

    -- ** FieldToMatch
    FieldToMatch (FieldToMatch'),
    newFieldToMatch,

    -- ** Filter
    Filter (Filter'),
    newFilter,

    -- ** FirewallManagerRuleGroup
    FirewallManagerRuleGroup (FirewallManagerRuleGroup'),
    newFirewallManagerRuleGroup,

    -- ** FirewallManagerStatement
    FirewallManagerStatement (FirewallManagerStatement'),
    newFirewallManagerStatement,

    -- ** ForwardedIPConfig
    ForwardedIPConfig (ForwardedIPConfig'),
    newForwardedIPConfig,

    -- ** GeoMatchStatement
    GeoMatchStatement (GeoMatchStatement'),
    newGeoMatchStatement,

    -- ** HTTPHeader
    HTTPHeader (HTTPHeader'),
    newHTTPHeader,

    -- ** HTTPRequest
    HTTPRequest (HTTPRequest'),
    newHTTPRequest,

    -- ** HeaderMatchPattern
    HeaderMatchPattern (HeaderMatchPattern'),
    newHeaderMatchPattern,

    -- ** Headers
    Headers (Headers'),
    newHeaders,

    -- ** IPSet
    IPSet (IPSet'),
    newIPSet,

    -- ** IPSetForwardedIPConfig
    IPSetForwardedIPConfig (IPSetForwardedIPConfig'),
    newIPSetForwardedIPConfig,

    -- ** IPSetReferenceStatement
    IPSetReferenceStatement (IPSetReferenceStatement'),
    newIPSetReferenceStatement,

    -- ** IPSetSummary
    IPSetSummary (IPSetSummary'),
    newIPSetSummary,

    -- ** ImmunityTimeProperty
    ImmunityTimeProperty (ImmunityTimeProperty'),
    newImmunityTimeProperty,

    -- ** JsonBody
    JsonBody (JsonBody'),
    newJsonBody,

    -- ** JsonMatchPattern
    JsonMatchPattern (JsonMatchPattern'),
    newJsonMatchPattern,

    -- ** Label
    Label (Label'),
    newLabel,

    -- ** LabelMatchStatement
    LabelMatchStatement (LabelMatchStatement'),
    newLabelMatchStatement,

    -- ** LabelNameCondition
    LabelNameCondition (LabelNameCondition'),
    newLabelNameCondition,

    -- ** LabelSummary
    LabelSummary (LabelSummary'),
    newLabelSummary,

    -- ** LoggingConfiguration
    LoggingConfiguration (LoggingConfiguration'),
    newLoggingConfiguration,

    -- ** LoggingFilter
    LoggingFilter (LoggingFilter'),
    newLoggingFilter,

    -- ** ManagedRuleGroupConfig
    ManagedRuleGroupConfig (ManagedRuleGroupConfig'),
    newManagedRuleGroupConfig,

    -- ** ManagedRuleGroupStatement
    ManagedRuleGroupStatement (ManagedRuleGroupStatement'),
    newManagedRuleGroupStatement,

    -- ** ManagedRuleGroupSummary
    ManagedRuleGroupSummary (ManagedRuleGroupSummary'),
    newManagedRuleGroupSummary,

    -- ** ManagedRuleGroupVersion
    ManagedRuleGroupVersion (ManagedRuleGroupVersion'),
    newManagedRuleGroupVersion,

    -- ** ManagedRuleSet
    ManagedRuleSet (ManagedRuleSet'),
    newManagedRuleSet,

    -- ** ManagedRuleSetSummary
    ManagedRuleSetSummary (ManagedRuleSetSummary'),
    newManagedRuleSetSummary,

    -- ** ManagedRuleSetVersion
    ManagedRuleSetVersion (ManagedRuleSetVersion'),
    newManagedRuleSetVersion,

    -- ** Method
    Method (Method'),
    newMethod,

    -- ** MobileSdkRelease
    MobileSdkRelease (MobileSdkRelease'),
    newMobileSdkRelease,

    -- ** NoneAction
    NoneAction (NoneAction'),
    newNoneAction,

    -- ** NotStatement
    NotStatement (NotStatement'),
    newNotStatement,

    -- ** OrStatement
    OrStatement (OrStatement'),
    newOrStatement,

    -- ** OverrideAction
    OverrideAction (OverrideAction'),
    newOverrideAction,

    -- ** PasswordField
    PasswordField (PasswordField'),
    newPasswordField,

    -- ** QueryString
    QueryString (QueryString'),
    newQueryString,

    -- ** RateBasedStatement
    RateBasedStatement (RateBasedStatement'),
    newRateBasedStatement,

    -- ** RateBasedStatementManagedKeysIPSet
    RateBasedStatementManagedKeysIPSet (RateBasedStatementManagedKeysIPSet'),
    newRateBasedStatementManagedKeysIPSet,

    -- ** Regex
    Regex (Regex'),
    newRegex,

    -- ** RegexMatchStatement
    RegexMatchStatement (RegexMatchStatement'),
    newRegexMatchStatement,

    -- ** RegexPatternSet
    RegexPatternSet (RegexPatternSet'),
    newRegexPatternSet,

    -- ** RegexPatternSetReferenceStatement
    RegexPatternSetReferenceStatement (RegexPatternSetReferenceStatement'),
    newRegexPatternSetReferenceStatement,

    -- ** RegexPatternSetSummary
    RegexPatternSetSummary (RegexPatternSetSummary'),
    newRegexPatternSetSummary,

    -- ** ReleaseSummary
    ReleaseSummary (ReleaseSummary'),
    newReleaseSummary,

    -- ** Rule
    Rule (Rule'),
    newRule,

    -- ** RuleAction
    RuleAction (RuleAction'),
    newRuleAction,

    -- ** RuleActionOverride
    RuleActionOverride (RuleActionOverride'),
    newRuleActionOverride,

    -- ** RuleGroup
    RuleGroup (RuleGroup'),
    newRuleGroup,

    -- ** RuleGroupReferenceStatement
    RuleGroupReferenceStatement (RuleGroupReferenceStatement'),
    newRuleGroupReferenceStatement,

    -- ** RuleGroupSummary
    RuleGroupSummary (RuleGroupSummary'),
    newRuleGroupSummary,

    -- ** RuleSummary
    RuleSummary (RuleSummary'),
    newRuleSummary,

    -- ** SampledHTTPRequest
    SampledHTTPRequest (SampledHTTPRequest'),
    newSampledHTTPRequest,

    -- ** SingleHeader
    SingleHeader (SingleHeader'),
    newSingleHeader,

    -- ** SingleQueryArgument
    SingleQueryArgument (SingleQueryArgument'),
    newSingleQueryArgument,

    -- ** SizeConstraintStatement
    SizeConstraintStatement (SizeConstraintStatement'),
    newSizeConstraintStatement,

    -- ** SqliMatchStatement
    SqliMatchStatement (SqliMatchStatement'),
    newSqliMatchStatement,

    -- ** Statement
    Statement (Statement'),
    newStatement,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** TagInfoForResource
    TagInfoForResource (TagInfoForResource'),
    newTagInfoForResource,

    -- ** TextTransformation
    TextTransformation (TextTransformation'),
    newTextTransformation,

    -- ** TimeWindow
    TimeWindow (TimeWindow'),
    newTimeWindow,

    -- ** UriPath
    UriPath (UriPath'),
    newUriPath,

    -- ** UsernameField
    UsernameField (UsernameField'),
    newUsernameField,

    -- ** VersionToPublish
    VersionToPublish (VersionToPublish'),
    newVersionToPublish,

    -- ** VisibilityConfig
    VisibilityConfig (VisibilityConfig'),
    newVisibilityConfig,

    -- ** WebACL
    WebACL (WebACL'),
    newWebACL,

    -- ** WebACLSummary
    WebACLSummary (WebACLSummary'),
    newWebACLSummary,

    -- ** XssMatchStatement
    XssMatchStatement (XssMatchStatement'),
    newXssMatchStatement,
  )
where

import Amazonka.WAFV2.AssociateWebACL
import Amazonka.WAFV2.CheckCapacity
import Amazonka.WAFV2.CreateIPSet
import Amazonka.WAFV2.CreateRegexPatternSet
import Amazonka.WAFV2.CreateRuleGroup
import Amazonka.WAFV2.CreateWebACL
import Amazonka.WAFV2.DeleteFirewallManagerRuleGroups
import Amazonka.WAFV2.DeleteIPSet
import Amazonka.WAFV2.DeleteLoggingConfiguration
import Amazonka.WAFV2.DeletePermissionPolicy
import Amazonka.WAFV2.DeleteRegexPatternSet
import Amazonka.WAFV2.DeleteRuleGroup
import Amazonka.WAFV2.DeleteWebACL
import Amazonka.WAFV2.DescribeManagedRuleGroup
import Amazonka.WAFV2.DisassociateWebACL
import Amazonka.WAFV2.GenerateMobileSdkReleaseUrl
import Amazonka.WAFV2.GetIPSet
import Amazonka.WAFV2.GetLoggingConfiguration
import Amazonka.WAFV2.GetManagedRuleSet
import Amazonka.WAFV2.GetMobileSdkRelease
import Amazonka.WAFV2.GetPermissionPolicy
import Amazonka.WAFV2.GetRateBasedStatementManagedKeys
import Amazonka.WAFV2.GetRegexPatternSet
import Amazonka.WAFV2.GetRuleGroup
import Amazonka.WAFV2.GetSampledRequests
import Amazonka.WAFV2.GetWebACL
import Amazonka.WAFV2.GetWebACLForResource
import Amazonka.WAFV2.Lens
import Amazonka.WAFV2.ListAvailableManagedRuleGroupVersions
import Amazonka.WAFV2.ListAvailableManagedRuleGroups
import Amazonka.WAFV2.ListIPSets
import Amazonka.WAFV2.ListLoggingConfigurations
import Amazonka.WAFV2.ListManagedRuleSets
import Amazonka.WAFV2.ListMobileSdkReleases
import Amazonka.WAFV2.ListRegexPatternSets
import Amazonka.WAFV2.ListResourcesForWebACL
import Amazonka.WAFV2.ListRuleGroups
import Amazonka.WAFV2.ListTagsForResource
import Amazonka.WAFV2.ListWebACLs
import Amazonka.WAFV2.PutLoggingConfiguration
import Amazonka.WAFV2.PutManagedRuleSetVersions
import Amazonka.WAFV2.PutPermissionPolicy
import Amazonka.WAFV2.TagResource
import Amazonka.WAFV2.Types
import Amazonka.WAFV2.UntagResource
import Amazonka.WAFV2.UpdateIPSet
import Amazonka.WAFV2.UpdateManagedRuleSetVersionExpiryDate
import Amazonka.WAFV2.UpdateRegexPatternSet
import Amazonka.WAFV2.UpdateRuleGroup
import Amazonka.WAFV2.UpdateWebACL
import Amazonka.WAFV2.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'WAFV2'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
