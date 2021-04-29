{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Cloud9.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Cloud9.Lens
  ( -- * Operations

    -- ** ListEnvironments
    listEnvironments_nextToken,
    listEnvironments_maxResults,
    listEnvironmentsResponse_nextToken,
    listEnvironmentsResponse_environmentIds,
    listEnvironmentsResponse_httpStatus,

    -- ** CreateEnvironmentMembership
    createEnvironmentMembership_environmentId,
    createEnvironmentMembership_userArn,
    createEnvironmentMembership_permissions,
    createEnvironmentMembershipResponse_membership,
    createEnvironmentMembershipResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceARN,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceARN,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** DescribeEnvironmentStatus
    describeEnvironmentStatus_environmentId,
    describeEnvironmentStatusResponse_status,
    describeEnvironmentStatusResponse_message,
    describeEnvironmentStatusResponse_httpStatus,

    -- ** UpdateEnvironment
    updateEnvironment_name,
    updateEnvironment_description,
    updateEnvironment_environmentId,
    updateEnvironmentResponse_httpStatus,

    -- ** DeleteEnvironment
    deleteEnvironment_environmentId,
    deleteEnvironmentResponse_httpStatus,

    -- ** DescribeEnvironmentMemberships
    describeEnvironmentMemberships_nextToken,
    describeEnvironmentMemberships_userArn,
    describeEnvironmentMemberships_maxResults,
    describeEnvironmentMemberships_permissions,
    describeEnvironmentMemberships_environmentId,
    describeEnvironmentMembershipsResponse_nextToken,
    describeEnvironmentMembershipsResponse_memberships,
    describeEnvironmentMembershipsResponse_httpStatus,

    -- ** UpdateEnvironmentMembership
    updateEnvironmentMembership_environmentId,
    updateEnvironmentMembership_userArn,
    updateEnvironmentMembership_permissions,
    updateEnvironmentMembershipResponse_membership,
    updateEnvironmentMembershipResponse_httpStatus,

    -- ** DeleteEnvironmentMembership
    deleteEnvironmentMembership_environmentId,
    deleteEnvironmentMembership_userArn,
    deleteEnvironmentMembershipResponse_httpStatus,

    -- ** DescribeEnvironments
    describeEnvironments_environmentIds,
    describeEnvironmentsResponse_environments,
    describeEnvironmentsResponse_httpStatus,

    -- ** CreateEnvironmentEC
    createEnvironmentEC_connectionType,
    createEnvironmentEC_ownerArn,
    createEnvironmentEC_tags,
    createEnvironmentEC_subnetId,
    createEnvironmentEC_description,
    createEnvironmentEC_clientRequestToken,
    createEnvironmentEC_automaticStopTimeMinutes,
    createEnvironmentEC_name,
    createEnvironmentEC_instanceType,
    createEnvironmentECResponse_environmentId,
    createEnvironmentECResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceARN,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- * Types

    -- ** Environment
    environment_lifecycle,
    environment_connectionType,
    environment_id,
    environment_arn,
    environment_name,
    environment_ownerArn,
    environment_description,
    environment_type,

    -- ** EnvironmentLifecycle
    environmentLifecycle_status,
    environmentLifecycle_reason,
    environmentLifecycle_failureResource,

    -- ** EnvironmentMember
    environmentMember_userArn,
    environmentMember_permissions,
    environmentMember_environmentId,
    environmentMember_userId,
    environmentMember_lastAccess,

    -- ** Tag
    tag_key,
    tag_value,
  )
where

import Network.AWS.Cloud9.CreateEnvironmentEC
import Network.AWS.Cloud9.CreateEnvironmentMembership
import Network.AWS.Cloud9.DeleteEnvironment
import Network.AWS.Cloud9.DeleteEnvironmentMembership
import Network.AWS.Cloud9.DescribeEnvironmentMemberships
import Network.AWS.Cloud9.DescribeEnvironmentStatus
import Network.AWS.Cloud9.DescribeEnvironments
import Network.AWS.Cloud9.ListEnvironments
import Network.AWS.Cloud9.ListTagsForResource
import Network.AWS.Cloud9.TagResource
import Network.AWS.Cloud9.Types.Environment
import Network.AWS.Cloud9.Types.EnvironmentLifecycle
import Network.AWS.Cloud9.Types.EnvironmentMember
import Network.AWS.Cloud9.Types.Tag
import Network.AWS.Cloud9.UntagResource
import Network.AWS.Cloud9.UpdateEnvironment
import Network.AWS.Cloud9.UpdateEnvironmentMembership
