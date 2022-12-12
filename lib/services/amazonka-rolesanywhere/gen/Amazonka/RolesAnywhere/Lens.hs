{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.RolesAnywhere.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RolesAnywhere.Lens
  ( -- * Operations

    -- ** CreateProfile
    createProfile_durationSeconds,
    createProfile_enabled,
    createProfile_managedPolicyArns,
    createProfile_requireInstanceProperties,
    createProfile_sessionPolicy,
    createProfile_tags,
    createProfile_name,
    createProfile_roleArns,
    profileDetailResponse_profile,

    -- ** CreateTrustAnchor
    createTrustAnchor_enabled,
    createTrustAnchor_tags,
    createTrustAnchor_name,
    createTrustAnchor_source,
    trustAnchorDetailResponse_trustAnchor,

    -- ** DeleteCrl
    deleteCrl_crlId,
    crlDetailResponse_crl,

    -- ** DeleteProfile
    deleteProfile_profileId,
    profileDetailResponse_profile,

    -- ** DeleteTrustAnchor
    deleteTrustAnchor_trustAnchorId,
    trustAnchorDetailResponse_trustAnchor,

    -- ** DisableCrl
    disableCrl_crlId,
    crlDetailResponse_crl,

    -- ** DisableProfile
    disableProfile_profileId,
    profileDetailResponse_profile,

    -- ** DisableTrustAnchor
    disableTrustAnchor_trustAnchorId,
    trustAnchorDetailResponse_trustAnchor,

    -- ** EnableCrl
    enableCrl_crlId,
    crlDetailResponse_crl,

    -- ** EnableProfile
    enableProfile_profileId,
    profileDetailResponse_profile,

    -- ** EnableTrustAnchor
    enableTrustAnchor_trustAnchorId,
    trustAnchorDetailResponse_trustAnchor,

    -- ** GetCrl
    getCrl_crlId,
    crlDetailResponse_crl,

    -- ** GetProfile
    getProfile_profileId,
    profileDetailResponse_profile,

    -- ** GetSubject
    getSubject_subjectId,
    getSubjectResponse_subject,
    getSubjectResponse_httpStatus,

    -- ** GetTrustAnchor
    getTrustAnchor_trustAnchorId,
    trustAnchorDetailResponse_trustAnchor,

    -- ** ImportCrl
    importCrl_enabled,
    importCrl_tags,
    importCrl_crlData,
    importCrl_name,
    importCrl_trustAnchorArn,
    crlDetailResponse_crl,

    -- ** ListCrls
    listCrls_nextToken,
    listCrls_pageSize,
    listCrlsResponse_crls,
    listCrlsResponse_nextToken,
    listCrlsResponse_httpStatus,

    -- ** ListProfiles
    listProfiles_nextToken,
    listProfiles_pageSize,
    listProfilesResponse_nextToken,
    listProfilesResponse_profiles,
    listProfilesResponse_httpStatus,

    -- ** ListSubjects
    listSubjects_nextToken,
    listSubjects_pageSize,
    listSubjectsResponse_nextToken,
    listSubjectsResponse_subjects,
    listSubjectsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** ListTrustAnchors
    listTrustAnchors_nextToken,
    listTrustAnchors_pageSize,
    listTrustAnchorsResponse_nextToken,
    listTrustAnchorsResponse_trustAnchors,
    listTrustAnchorsResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateCrl
    updateCrl_crlData,
    updateCrl_name,
    updateCrl_crlId,
    crlDetailResponse_crl,

    -- ** UpdateProfile
    updateProfile_durationSeconds,
    updateProfile_managedPolicyArns,
    updateProfile_name,
    updateProfile_roleArns,
    updateProfile_sessionPolicy,
    updateProfile_profileId,
    profileDetailResponse_profile,

    -- ** UpdateTrustAnchor
    updateTrustAnchor_name,
    updateTrustAnchor_source,
    updateTrustAnchor_trustAnchorId,
    trustAnchorDetailResponse_trustAnchor,

    -- * Types

    -- ** CredentialSummary
    credentialSummary_enabled,
    credentialSummary_failed,
    credentialSummary_issuer,
    credentialSummary_seenAt,
    credentialSummary_serialNumber,
    credentialSummary_x509CertificateData,

    -- ** CrlDetail
    crlDetail_createdAt,
    crlDetail_crlArn,
    crlDetail_crlData,
    crlDetail_crlId,
    crlDetail_enabled,
    crlDetail_name,
    crlDetail_trustAnchorArn,
    crlDetail_updatedAt,

    -- ** CrlDetailResponse
    crlDetailResponse_crl,

    -- ** InstanceProperty
    instanceProperty_failed,
    instanceProperty_properties,
    instanceProperty_seenAt,

    -- ** ListRequest
    listRequest_nextToken,
    listRequest_pageSize,

    -- ** ProfileDetail
    profileDetail_createdAt,
    profileDetail_createdBy,
    profileDetail_durationSeconds,
    profileDetail_enabled,
    profileDetail_managedPolicyArns,
    profileDetail_name,
    profileDetail_profileArn,
    profileDetail_profileId,
    profileDetail_requireInstanceProperties,
    profileDetail_roleArns,
    profileDetail_sessionPolicy,
    profileDetail_updatedAt,

    -- ** ProfileDetailResponse
    profileDetailResponse_profile,

    -- ** ScalarCrlRequest
    scalarCrlRequest_crlId,

    -- ** ScalarProfileRequest
    scalarProfileRequest_profileId,

    -- ** ScalarTrustAnchorRequest
    scalarTrustAnchorRequest_trustAnchorId,

    -- ** Source
    source_sourceData,
    source_sourceType,

    -- ** SourceData
    sourceData_acmPcaArn,
    sourceData_x509CertificateData,

    -- ** SubjectDetail
    subjectDetail_createdAt,
    subjectDetail_credentials,
    subjectDetail_enabled,
    subjectDetail_instanceProperties,
    subjectDetail_lastSeenAt,
    subjectDetail_subjectArn,
    subjectDetail_subjectId,
    subjectDetail_updatedAt,
    subjectDetail_x509Subject,

    -- ** SubjectSummary
    subjectSummary_createdAt,
    subjectSummary_enabled,
    subjectSummary_lastSeenAt,
    subjectSummary_subjectArn,
    subjectSummary_subjectId,
    subjectSummary_updatedAt,
    subjectSummary_x509Subject,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** TrustAnchorDetail
    trustAnchorDetail_createdAt,
    trustAnchorDetail_enabled,
    trustAnchorDetail_name,
    trustAnchorDetail_source,
    trustAnchorDetail_trustAnchorArn,
    trustAnchorDetail_trustAnchorId,
    trustAnchorDetail_updatedAt,

    -- ** TrustAnchorDetailResponse
    trustAnchorDetailResponse_trustAnchor,
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
import Amazonka.RolesAnywhere.ListCrls
import Amazonka.RolesAnywhere.ListProfiles
import Amazonka.RolesAnywhere.ListSubjects
import Amazonka.RolesAnywhere.ListTagsForResource
import Amazonka.RolesAnywhere.ListTrustAnchors
import Amazonka.RolesAnywhere.TagResource
import Amazonka.RolesAnywhere.Types.CredentialSummary
import Amazonka.RolesAnywhere.Types.CrlDetail
import Amazonka.RolesAnywhere.Types.CrlDetailResponse
import Amazonka.RolesAnywhere.Types.InstanceProperty
import Amazonka.RolesAnywhere.Types.ListRequest
import Amazonka.RolesAnywhere.Types.ProfileDetail
import Amazonka.RolesAnywhere.Types.ProfileDetailResponse
import Amazonka.RolesAnywhere.Types.ScalarCrlRequest
import Amazonka.RolesAnywhere.Types.ScalarProfileRequest
import Amazonka.RolesAnywhere.Types.ScalarTrustAnchorRequest
import Amazonka.RolesAnywhere.Types.Source
import Amazonka.RolesAnywhere.Types.SourceData
import Amazonka.RolesAnywhere.Types.SubjectDetail
import Amazonka.RolesAnywhere.Types.SubjectSummary
import Amazonka.RolesAnywhere.Types.Tag
import Amazonka.RolesAnywhere.Types.TrustAnchorDetail
import Amazonka.RolesAnywhere.Types.TrustAnchorDetailResponse
import Amazonka.RolesAnywhere.UntagResource
import Amazonka.RolesAnywhere.UpdateCrl
import Amazonka.RolesAnywhere.UpdateProfile
import Amazonka.RolesAnywhere.UpdateTrustAnchor
