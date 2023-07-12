{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Cloud9.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Cloud9.Lens
  ( -- * Operations

    -- ** CreateEnvironmentEC2
    createEnvironmentEC2_automaticStopTimeMinutes,
    createEnvironmentEC2_clientRequestToken,
    createEnvironmentEC2_connectionType,
    createEnvironmentEC2_description,
    createEnvironmentEC2_dryRun,
    createEnvironmentEC2_imageId,
    createEnvironmentEC2_ownerArn,
    createEnvironmentEC2_subnetId,
    createEnvironmentEC2_tags,
    createEnvironmentEC2_name,
    createEnvironmentEC2_instanceType,
    createEnvironmentEC2Response_environmentId,
    createEnvironmentEC2Response_httpStatus,

    -- ** CreateEnvironmentMembership
    createEnvironmentMembership_environmentId,
    createEnvironmentMembership_userArn,
    createEnvironmentMembership_permissions,
    createEnvironmentMembershipResponse_httpStatus,
    createEnvironmentMembershipResponse_membership,

    -- ** DeleteEnvironment
    deleteEnvironment_environmentId,
    deleteEnvironmentResponse_httpStatus,

    -- ** DeleteEnvironmentMembership
    deleteEnvironmentMembership_environmentId,
    deleteEnvironmentMembership_userArn,
    deleteEnvironmentMembershipResponse_httpStatus,

    -- ** DescribeEnvironmentMemberships
    describeEnvironmentMemberships_environmentId,
    describeEnvironmentMemberships_maxResults,
    describeEnvironmentMemberships_nextToken,
    describeEnvironmentMemberships_permissions,
    describeEnvironmentMemberships_userArn,
    describeEnvironmentMembershipsResponse_memberships,
    describeEnvironmentMembershipsResponse_nextToken,
    describeEnvironmentMembershipsResponse_httpStatus,

    -- ** DescribeEnvironmentStatus
    describeEnvironmentStatus_environmentId,
    describeEnvironmentStatusResponse_httpStatus,
    describeEnvironmentStatusResponse_status,
    describeEnvironmentStatusResponse_message,

    -- ** DescribeEnvironments
    describeEnvironments_environmentIds,
    describeEnvironmentsResponse_environments,
    describeEnvironmentsResponse_httpStatus,

    -- ** ListEnvironments
    listEnvironments_maxResults,
    listEnvironments_nextToken,
    listEnvironmentsResponse_environmentIds,
    listEnvironmentsResponse_nextToken,
    listEnvironmentsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceARN,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceARN,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceARN,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateEnvironment
    updateEnvironment_description,
    updateEnvironment_managedCredentialsAction,
    updateEnvironment_name,
    updateEnvironment_environmentId,
    updateEnvironmentResponse_httpStatus,

    -- ** UpdateEnvironmentMembership
    updateEnvironmentMembership_environmentId,
    updateEnvironmentMembership_userArn,
    updateEnvironmentMembership_permissions,
    updateEnvironmentMembershipResponse_membership,
    updateEnvironmentMembershipResponse_httpStatus,

    -- * Types

    -- ** Environment
    environment_connectionType,
    environment_description,
    environment_id,
    environment_lifecycle,
    environment_managedCredentialsStatus,
    environment_name,
    environment_type,
    environment_arn,
    environment_ownerArn,

    -- ** EnvironmentLifecycle
    environmentLifecycle_failureResource,
    environmentLifecycle_reason,
    environmentLifecycle_status,

    -- ** EnvironmentMember
    environmentMember_lastAccess,
    environmentMember_permissions,
    environmentMember_userId,
    environmentMember_userArn,
    environmentMember_environmentId,

    -- ** Tag
    tag_key,
    tag_value,
  )
where

import Amazonka.Cloud9.CreateEnvironmentEC2
import Amazonka.Cloud9.CreateEnvironmentMembership
import Amazonka.Cloud9.DeleteEnvironment
import Amazonka.Cloud9.DeleteEnvironmentMembership
import Amazonka.Cloud9.DescribeEnvironmentMemberships
import Amazonka.Cloud9.DescribeEnvironmentStatus
import Amazonka.Cloud9.DescribeEnvironments
import Amazonka.Cloud9.ListEnvironments
import Amazonka.Cloud9.ListTagsForResource
import Amazonka.Cloud9.TagResource
import Amazonka.Cloud9.Types.Environment
import Amazonka.Cloud9.Types.EnvironmentLifecycle
import Amazonka.Cloud9.Types.EnvironmentMember
import Amazonka.Cloud9.Types.Tag
import Amazonka.Cloud9.UntagResource
import Amazonka.Cloud9.UpdateEnvironment
import Amazonka.Cloud9.UpdateEnvironmentMembership
