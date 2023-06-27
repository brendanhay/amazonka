{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.RolesAnywhere
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2018-05-10@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Identity and Access Management Roles Anywhere provides a secure way for
-- your workloads such as servers, containers, and applications that run
-- outside of Amazon Web Services to obtain temporary Amazon Web Services
-- credentials. Your workloads can use the same IAM policies and roles you
-- have for native Amazon Web Services applications to access Amazon Web
-- Services resources. Using IAM Roles Anywhere eliminates the need to
-- manage long-term credentials for workloads running outside of Amazon Web
-- Services.
--
-- To use IAM Roles Anywhere, your workloads must use X.509 certificates
-- issued by their certificate authority (CA). You register the CA with IAM
-- Roles Anywhere as a trust anchor to establish trust between your public
-- key infrastructure (PKI) and IAM Roles Anywhere. If you don\'t manage
-- your own PKI system, you can use Private Certificate Authority to create
-- a CA and then use that to establish trust with IAM Roles Anywhere.
--
-- This guide describes the IAM Roles Anywhere operations that you can call
-- programmatically. For more information about IAM Roles Anywhere, see the
-- <https://docs.aws.amazon.com/rolesanywhere/latest/userguide/introduction.html IAM Roles Anywhere User Guide>.
module Amazonka.RolesAnywhere
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** TooManyTagsException
    _TooManyTagsException,

    -- ** ValidationException
    _ValidationException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CreateProfile
    CreateProfile (CreateProfile'),
    newCreateProfile,
    ProfileDetailResponse (ProfileDetailResponse'),
    newProfileDetailResponse,

    -- ** CreateTrustAnchor
    CreateTrustAnchor (CreateTrustAnchor'),
    newCreateTrustAnchor,
    TrustAnchorDetailResponse (TrustAnchorDetailResponse'),
    newTrustAnchorDetailResponse,

    -- ** DeleteCrl
    DeleteCrl (DeleteCrl'),
    newDeleteCrl,
    CrlDetailResponse (CrlDetailResponse'),
    newCrlDetailResponse,

    -- ** DeleteProfile
    DeleteProfile (DeleteProfile'),
    newDeleteProfile,
    ProfileDetailResponse (ProfileDetailResponse'),
    newProfileDetailResponse,

    -- ** DeleteTrustAnchor
    DeleteTrustAnchor (DeleteTrustAnchor'),
    newDeleteTrustAnchor,
    TrustAnchorDetailResponse (TrustAnchorDetailResponse'),
    newTrustAnchorDetailResponse,

    -- ** DisableCrl
    DisableCrl (DisableCrl'),
    newDisableCrl,
    CrlDetailResponse (CrlDetailResponse'),
    newCrlDetailResponse,

    -- ** DisableProfile
    DisableProfile (DisableProfile'),
    newDisableProfile,
    ProfileDetailResponse (ProfileDetailResponse'),
    newProfileDetailResponse,

    -- ** DisableTrustAnchor
    DisableTrustAnchor (DisableTrustAnchor'),
    newDisableTrustAnchor,
    TrustAnchorDetailResponse (TrustAnchorDetailResponse'),
    newTrustAnchorDetailResponse,

    -- ** EnableCrl
    EnableCrl (EnableCrl'),
    newEnableCrl,
    CrlDetailResponse (CrlDetailResponse'),
    newCrlDetailResponse,

    -- ** EnableProfile
    EnableProfile (EnableProfile'),
    newEnableProfile,
    ProfileDetailResponse (ProfileDetailResponse'),
    newProfileDetailResponse,

    -- ** EnableTrustAnchor
    EnableTrustAnchor (EnableTrustAnchor'),
    newEnableTrustAnchor,
    TrustAnchorDetailResponse (TrustAnchorDetailResponse'),
    newTrustAnchorDetailResponse,

    -- ** GetCrl
    GetCrl (GetCrl'),
    newGetCrl,
    CrlDetailResponse (CrlDetailResponse'),
    newCrlDetailResponse,

    -- ** GetProfile
    GetProfile (GetProfile'),
    newGetProfile,
    ProfileDetailResponse (ProfileDetailResponse'),
    newProfileDetailResponse,

    -- ** GetSubject
    GetSubject (GetSubject'),
    newGetSubject,
    GetSubjectResponse (GetSubjectResponse'),
    newGetSubjectResponse,

    -- ** GetTrustAnchor
    GetTrustAnchor (GetTrustAnchor'),
    newGetTrustAnchor,
    TrustAnchorDetailResponse (TrustAnchorDetailResponse'),
    newTrustAnchorDetailResponse,

    -- ** ImportCrl
    ImportCrl (ImportCrl'),
    newImportCrl,
    CrlDetailResponse (CrlDetailResponse'),
    newCrlDetailResponse,

    -- ** ListCrls (Paginated)
    ListCrls (ListCrls'),
    newListCrls,
    ListCrlsResponse (ListCrlsResponse'),
    newListCrlsResponse,

    -- ** ListProfiles (Paginated)
    ListProfiles (ListProfiles'),
    newListProfiles,
    ListProfilesResponse (ListProfilesResponse'),
    newListProfilesResponse,

    -- ** ListSubjects (Paginated)
    ListSubjects (ListSubjects'),
    newListSubjects,
    ListSubjectsResponse (ListSubjectsResponse'),
    newListSubjectsResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** ListTrustAnchors (Paginated)
    ListTrustAnchors (ListTrustAnchors'),
    newListTrustAnchors,
    ListTrustAnchorsResponse (ListTrustAnchorsResponse'),
    newListTrustAnchorsResponse,

    -- ** PutNotificationSettings
    PutNotificationSettings (PutNotificationSettings'),
    newPutNotificationSettings,
    PutNotificationSettingsResponse (PutNotificationSettingsResponse'),
    newPutNotificationSettingsResponse,

    -- ** ResetNotificationSettings
    ResetNotificationSettings (ResetNotificationSettings'),
    newResetNotificationSettings,
    ResetNotificationSettingsResponse (ResetNotificationSettingsResponse'),
    newResetNotificationSettingsResponse,

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

    -- ** UpdateCrl
    UpdateCrl (UpdateCrl'),
    newUpdateCrl,
    CrlDetailResponse (CrlDetailResponse'),
    newCrlDetailResponse,

    -- ** UpdateProfile
    UpdateProfile (UpdateProfile'),
    newUpdateProfile,
    ProfileDetailResponse (ProfileDetailResponse'),
    newProfileDetailResponse,

    -- ** UpdateTrustAnchor
    UpdateTrustAnchor (UpdateTrustAnchor'),
    newUpdateTrustAnchor,
    TrustAnchorDetailResponse (TrustAnchorDetailResponse'),
    newTrustAnchorDetailResponse,

    -- * Types

    -- ** NotificationChannel
    NotificationChannel (..),

    -- ** NotificationEvent
    NotificationEvent (..),

    -- ** TrustAnchorType
    TrustAnchorType (..),

    -- ** CredentialSummary
    CredentialSummary (CredentialSummary'),
    newCredentialSummary,

    -- ** CrlDetail
    CrlDetail (CrlDetail'),
    newCrlDetail,

    -- ** CrlDetailResponse
    CrlDetailResponse (CrlDetailResponse'),
    newCrlDetailResponse,

    -- ** InstanceProperty
    InstanceProperty (InstanceProperty'),
    newInstanceProperty,

    -- ** ListRequest
    ListRequest (ListRequest'),
    newListRequest,

    -- ** NotificationSetting
    NotificationSetting (NotificationSetting'),
    newNotificationSetting,

    -- ** NotificationSettingDetail
    NotificationSettingDetail (NotificationSettingDetail'),
    newNotificationSettingDetail,

    -- ** NotificationSettingKey
    NotificationSettingKey (NotificationSettingKey'),
    newNotificationSettingKey,

    -- ** ProfileDetail
    ProfileDetail (ProfileDetail'),
    newProfileDetail,

    -- ** ProfileDetailResponse
    ProfileDetailResponse (ProfileDetailResponse'),
    newProfileDetailResponse,

    -- ** ScalarCrlRequest
    ScalarCrlRequest (ScalarCrlRequest'),
    newScalarCrlRequest,

    -- ** ScalarProfileRequest
    ScalarProfileRequest (ScalarProfileRequest'),
    newScalarProfileRequest,

    -- ** ScalarTrustAnchorRequest
    ScalarTrustAnchorRequest (ScalarTrustAnchorRequest'),
    newScalarTrustAnchorRequest,

    -- ** Source
    Source (Source'),
    newSource,

    -- ** SourceData
    SourceData (SourceData'),
    newSourceData,

    -- ** SubjectDetail
    SubjectDetail (SubjectDetail'),
    newSubjectDetail,

    -- ** SubjectSummary
    SubjectSummary (SubjectSummary'),
    newSubjectSummary,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** TrustAnchorDetail
    TrustAnchorDetail (TrustAnchorDetail'),
    newTrustAnchorDetail,

    -- ** TrustAnchorDetailResponse
    TrustAnchorDetailResponse (TrustAnchorDetailResponse'),
    newTrustAnchorDetailResponse,
  )
where

import Amazonka.RolesAnywhere.CreateProfile
import Amazonka.RolesAnywhere.CreateTrustAnchor
import Amazonka.RolesAnywhere.DeleteCrl
import Amazonka.RolesAnywhere.DeleteProfile
import Amazonka.RolesAnywhere.DeleteTrustAnchor
import Amazonka.RolesAnywhere.DisableCrl
import Amazonka.RolesAnywhere.DisableProfile
import Amazonka.RolesAnywhere.DisableTrustAnchor
import Amazonka.RolesAnywhere.EnableCrl
import Amazonka.RolesAnywhere.EnableProfile
import Amazonka.RolesAnywhere.EnableTrustAnchor
import Amazonka.RolesAnywhere.GetCrl
import Amazonka.RolesAnywhere.GetProfile
import Amazonka.RolesAnywhere.GetSubject
import Amazonka.RolesAnywhere.GetTrustAnchor
import Amazonka.RolesAnywhere.ImportCrl
import Amazonka.RolesAnywhere.Lens
import Amazonka.RolesAnywhere.ListCrls
import Amazonka.RolesAnywhere.ListProfiles
import Amazonka.RolesAnywhere.ListSubjects
import Amazonka.RolesAnywhere.ListTagsForResource
import Amazonka.RolesAnywhere.ListTrustAnchors
import Amazonka.RolesAnywhere.PutNotificationSettings
import Amazonka.RolesAnywhere.ResetNotificationSettings
import Amazonka.RolesAnywhere.TagResource
import Amazonka.RolesAnywhere.Types
import Amazonka.RolesAnywhere.UntagResource
import Amazonka.RolesAnywhere.UpdateCrl
import Amazonka.RolesAnywhere.UpdateProfile
import Amazonka.RolesAnywhere.UpdateTrustAnchor
import Amazonka.RolesAnywhere.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'RolesAnywhere'.

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
