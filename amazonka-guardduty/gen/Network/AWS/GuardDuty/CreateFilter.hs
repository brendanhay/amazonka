{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.CreateFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a filter using the specified finding criteria.
module Network.AWS.GuardDuty.CreateFilter
  ( -- * Creating a Request
    CreateFilter (..),
    newCreateFilter,

    -- * Request Lenses
    createFilter_rank,
    createFilter_tags,
    createFilter_action,
    createFilter_description,
    createFilter_clientToken,
    createFilter_detectorId,
    createFilter_name,
    createFilter_findingCriteria,

    -- * Destructuring the Response
    CreateFilterResponse (..),
    newCreateFilterResponse,

    -- * Response Lenses
    createFilterResponse_httpStatus,
    createFilterResponse_name,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateFilter' smart constructor.
data CreateFilter = CreateFilter'
  { -- | Specifies the position of the filter in the list of current filters.
    -- Also specifies the order in which this filter is applied to the
    -- findings.
    rank :: Prelude.Maybe Prelude.Natural,
    -- | The tags to be added to a new filter resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Specifies the action that is to be applied to the findings that match
    -- the filter.
    action :: Prelude.Maybe FilterAction,
    -- | The description of the filter.
    description :: Prelude.Maybe Prelude.Text,
    -- | The idempotency token for the create request.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the detector belonging to the GuardDuty account that you want
    -- to create a filter for.
    detectorId :: Prelude.Text,
    -- | The name of the filter. Minimum length of 3. Maximum length of 64. Valid
    -- characters include alphanumeric characters, dot (.), underscore (_), and
    -- dash (-). Spaces are not allowed.
    name :: Prelude.Text,
    -- | Represents the criteria to be used in the filter for querying findings.
    --
    -- You can only use the following attributes to query findings:
    --
    -- -   accountId
    --
    -- -   region
    --
    -- -   confidence
    --
    -- -   id
    --
    -- -   resource.accessKeyDetails.accessKeyId
    --
    -- -   resource.accessKeyDetails.principalId
    --
    -- -   resource.accessKeyDetails.userName
    --
    -- -   resource.accessKeyDetails.userType
    --
    -- -   resource.instanceDetails.iamInstanceProfile.id
    --
    -- -   resource.instanceDetails.imageId
    --
    -- -   resource.instanceDetails.instanceId
    --
    -- -   resource.instanceDetails.outpostArn
    --
    -- -   resource.instanceDetails.networkInterfaces.ipv6Addresses
    --
    -- -   resource.instanceDetails.networkInterfaces.privateIpAddresses.privateIpAddress
    --
    -- -   resource.instanceDetails.networkInterfaces.publicDnsName
    --
    -- -   resource.instanceDetails.networkInterfaces.publicIp
    --
    -- -   resource.instanceDetails.networkInterfaces.securityGroups.groupId
    --
    -- -   resource.instanceDetails.networkInterfaces.securityGroups.groupName
    --
    -- -   resource.instanceDetails.networkInterfaces.subnetId
    --
    -- -   resource.instanceDetails.networkInterfaces.vpcId
    --
    -- -   resource.instanceDetails.tags.key
    --
    -- -   resource.instanceDetails.tags.value
    --
    -- -   resource.resourceType
    --
    -- -   service.action.actionType
    --
    -- -   service.action.awsApiCallAction.api
    --
    -- -   service.action.awsApiCallAction.callerType
    --
    -- -   service.action.awsApiCallAction.errorCode
    --
    -- -   service.action.awsApiCallAction.remoteIpDetails.city.cityName
    --
    -- -   service.action.awsApiCallAction.remoteIpDetails.country.countryName
    --
    -- -   service.action.awsApiCallAction.remoteIpDetails.ipAddressV4
    --
    -- -   service.action.awsApiCallAction.remoteIpDetails.organization.asn
    --
    -- -   service.action.awsApiCallAction.remoteIpDetails.organization.asnOrg
    --
    -- -   service.action.awsApiCallAction.serviceName
    --
    -- -   service.action.dnsRequestAction.domain
    --
    -- -   service.action.networkConnectionAction.blocked
    --
    -- -   service.action.networkConnectionAction.connectionDirection
    --
    -- -   service.action.networkConnectionAction.localPortDetails.port
    --
    -- -   service.action.networkConnectionAction.protocol
    --
    -- -   service.action.networkConnectionAction.localIpDetails.ipAddressV4
    --
    -- -   service.action.networkConnectionAction.remoteIpDetails.city.cityName
    --
    -- -   service.action.networkConnectionAction.remoteIpDetails.country.countryName
    --
    -- -   service.action.networkConnectionAction.remoteIpDetails.ipAddressV4
    --
    -- -   service.action.networkConnectionAction.remoteIpDetails.organization.asn
    --
    -- -   service.action.networkConnectionAction.remoteIpDetails.organization.asnOrg
    --
    -- -   service.action.networkConnectionAction.remotePortDetails.port
    --
    -- -   service.additionalInfo.threatListName
    --
    -- -   service.archived
    --
    --     When this attribute is set to TRUE, only archived findings are
    --     listed. When it\'s set to FALSE, only unarchived findings are
    --     listed. When this attribute is not set, all existing findings are
    --     listed.
    --
    -- -   service.resourceRole
    --
    -- -   severity
    --
    -- -   type
    --
    -- -   updatedAt
    --
    --     Type: ISO 8601 string format: YYYY-MM-DDTHH:MM:SS.SSSZ or
    --     YYYY-MM-DDTHH:MM:SSZ depending on whether the value contains
    --     milliseconds.
    findingCriteria :: FindingCriteria
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rank', 'createFilter_rank' - Specifies the position of the filter in the list of current filters.
-- Also specifies the order in which this filter is applied to the
-- findings.
--
-- 'tags', 'createFilter_tags' - The tags to be added to a new filter resource.
--
-- 'action', 'createFilter_action' - Specifies the action that is to be applied to the findings that match
-- the filter.
--
-- 'description', 'createFilter_description' - The description of the filter.
--
-- 'clientToken', 'createFilter_clientToken' - The idempotency token for the create request.
--
-- 'detectorId', 'createFilter_detectorId' - The ID of the detector belonging to the GuardDuty account that you want
-- to create a filter for.
--
-- 'name', 'createFilter_name' - The name of the filter. Minimum length of 3. Maximum length of 64. Valid
-- characters include alphanumeric characters, dot (.), underscore (_), and
-- dash (-). Spaces are not allowed.
--
-- 'findingCriteria', 'createFilter_findingCriteria' - Represents the criteria to be used in the filter for querying findings.
--
-- You can only use the following attributes to query findings:
--
-- -   accountId
--
-- -   region
--
-- -   confidence
--
-- -   id
--
-- -   resource.accessKeyDetails.accessKeyId
--
-- -   resource.accessKeyDetails.principalId
--
-- -   resource.accessKeyDetails.userName
--
-- -   resource.accessKeyDetails.userType
--
-- -   resource.instanceDetails.iamInstanceProfile.id
--
-- -   resource.instanceDetails.imageId
--
-- -   resource.instanceDetails.instanceId
--
-- -   resource.instanceDetails.outpostArn
--
-- -   resource.instanceDetails.networkInterfaces.ipv6Addresses
--
-- -   resource.instanceDetails.networkInterfaces.privateIpAddresses.privateIpAddress
--
-- -   resource.instanceDetails.networkInterfaces.publicDnsName
--
-- -   resource.instanceDetails.networkInterfaces.publicIp
--
-- -   resource.instanceDetails.networkInterfaces.securityGroups.groupId
--
-- -   resource.instanceDetails.networkInterfaces.securityGroups.groupName
--
-- -   resource.instanceDetails.networkInterfaces.subnetId
--
-- -   resource.instanceDetails.networkInterfaces.vpcId
--
-- -   resource.instanceDetails.tags.key
--
-- -   resource.instanceDetails.tags.value
--
-- -   resource.resourceType
--
-- -   service.action.actionType
--
-- -   service.action.awsApiCallAction.api
--
-- -   service.action.awsApiCallAction.callerType
--
-- -   service.action.awsApiCallAction.errorCode
--
-- -   service.action.awsApiCallAction.remoteIpDetails.city.cityName
--
-- -   service.action.awsApiCallAction.remoteIpDetails.country.countryName
--
-- -   service.action.awsApiCallAction.remoteIpDetails.ipAddressV4
--
-- -   service.action.awsApiCallAction.remoteIpDetails.organization.asn
--
-- -   service.action.awsApiCallAction.remoteIpDetails.organization.asnOrg
--
-- -   service.action.awsApiCallAction.serviceName
--
-- -   service.action.dnsRequestAction.domain
--
-- -   service.action.networkConnectionAction.blocked
--
-- -   service.action.networkConnectionAction.connectionDirection
--
-- -   service.action.networkConnectionAction.localPortDetails.port
--
-- -   service.action.networkConnectionAction.protocol
--
-- -   service.action.networkConnectionAction.localIpDetails.ipAddressV4
--
-- -   service.action.networkConnectionAction.remoteIpDetails.city.cityName
--
-- -   service.action.networkConnectionAction.remoteIpDetails.country.countryName
--
-- -   service.action.networkConnectionAction.remoteIpDetails.ipAddressV4
--
-- -   service.action.networkConnectionAction.remoteIpDetails.organization.asn
--
-- -   service.action.networkConnectionAction.remoteIpDetails.organization.asnOrg
--
-- -   service.action.networkConnectionAction.remotePortDetails.port
--
-- -   service.additionalInfo.threatListName
--
-- -   service.archived
--
--     When this attribute is set to TRUE, only archived findings are
--     listed. When it\'s set to FALSE, only unarchived findings are
--     listed. When this attribute is not set, all existing findings are
--     listed.
--
-- -   service.resourceRole
--
-- -   severity
--
-- -   type
--
-- -   updatedAt
--
--     Type: ISO 8601 string format: YYYY-MM-DDTHH:MM:SS.SSSZ or
--     YYYY-MM-DDTHH:MM:SSZ depending on whether the value contains
--     milliseconds.
newCreateFilter ::
  -- | 'detectorId'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'findingCriteria'
  FindingCriteria ->
  CreateFilter
newCreateFilter pDetectorId_ pName_ pFindingCriteria_ =
  CreateFilter'
    { rank = Prelude.Nothing,
      tags = Prelude.Nothing,
      action = Prelude.Nothing,
      description = Prelude.Nothing,
      clientToken = Prelude.Nothing,
      detectorId = pDetectorId_,
      name = pName_,
      findingCriteria = pFindingCriteria_
    }

-- | Specifies the position of the filter in the list of current filters.
-- Also specifies the order in which this filter is applied to the
-- findings.
createFilter_rank :: Lens.Lens' CreateFilter (Prelude.Maybe Prelude.Natural)
createFilter_rank = Lens.lens (\CreateFilter' {rank} -> rank) (\s@CreateFilter' {} a -> s {rank = a} :: CreateFilter)

-- | The tags to be added to a new filter resource.
createFilter_tags :: Lens.Lens' CreateFilter (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createFilter_tags = Lens.lens (\CreateFilter' {tags} -> tags) (\s@CreateFilter' {} a -> s {tags = a} :: CreateFilter) Prelude.. Lens.mapping Lens._Coerce

-- | Specifies the action that is to be applied to the findings that match
-- the filter.
createFilter_action :: Lens.Lens' CreateFilter (Prelude.Maybe FilterAction)
createFilter_action = Lens.lens (\CreateFilter' {action} -> action) (\s@CreateFilter' {} a -> s {action = a} :: CreateFilter)

-- | The description of the filter.
createFilter_description :: Lens.Lens' CreateFilter (Prelude.Maybe Prelude.Text)
createFilter_description = Lens.lens (\CreateFilter' {description} -> description) (\s@CreateFilter' {} a -> s {description = a} :: CreateFilter)

-- | The idempotency token for the create request.
createFilter_clientToken :: Lens.Lens' CreateFilter (Prelude.Maybe Prelude.Text)
createFilter_clientToken = Lens.lens (\CreateFilter' {clientToken} -> clientToken) (\s@CreateFilter' {} a -> s {clientToken = a} :: CreateFilter)

-- | The ID of the detector belonging to the GuardDuty account that you want
-- to create a filter for.
createFilter_detectorId :: Lens.Lens' CreateFilter Prelude.Text
createFilter_detectorId = Lens.lens (\CreateFilter' {detectorId} -> detectorId) (\s@CreateFilter' {} a -> s {detectorId = a} :: CreateFilter)

-- | The name of the filter. Minimum length of 3. Maximum length of 64. Valid
-- characters include alphanumeric characters, dot (.), underscore (_), and
-- dash (-). Spaces are not allowed.
createFilter_name :: Lens.Lens' CreateFilter Prelude.Text
createFilter_name = Lens.lens (\CreateFilter' {name} -> name) (\s@CreateFilter' {} a -> s {name = a} :: CreateFilter)

-- | Represents the criteria to be used in the filter for querying findings.
--
-- You can only use the following attributes to query findings:
--
-- -   accountId
--
-- -   region
--
-- -   confidence
--
-- -   id
--
-- -   resource.accessKeyDetails.accessKeyId
--
-- -   resource.accessKeyDetails.principalId
--
-- -   resource.accessKeyDetails.userName
--
-- -   resource.accessKeyDetails.userType
--
-- -   resource.instanceDetails.iamInstanceProfile.id
--
-- -   resource.instanceDetails.imageId
--
-- -   resource.instanceDetails.instanceId
--
-- -   resource.instanceDetails.outpostArn
--
-- -   resource.instanceDetails.networkInterfaces.ipv6Addresses
--
-- -   resource.instanceDetails.networkInterfaces.privateIpAddresses.privateIpAddress
--
-- -   resource.instanceDetails.networkInterfaces.publicDnsName
--
-- -   resource.instanceDetails.networkInterfaces.publicIp
--
-- -   resource.instanceDetails.networkInterfaces.securityGroups.groupId
--
-- -   resource.instanceDetails.networkInterfaces.securityGroups.groupName
--
-- -   resource.instanceDetails.networkInterfaces.subnetId
--
-- -   resource.instanceDetails.networkInterfaces.vpcId
--
-- -   resource.instanceDetails.tags.key
--
-- -   resource.instanceDetails.tags.value
--
-- -   resource.resourceType
--
-- -   service.action.actionType
--
-- -   service.action.awsApiCallAction.api
--
-- -   service.action.awsApiCallAction.callerType
--
-- -   service.action.awsApiCallAction.errorCode
--
-- -   service.action.awsApiCallAction.remoteIpDetails.city.cityName
--
-- -   service.action.awsApiCallAction.remoteIpDetails.country.countryName
--
-- -   service.action.awsApiCallAction.remoteIpDetails.ipAddressV4
--
-- -   service.action.awsApiCallAction.remoteIpDetails.organization.asn
--
-- -   service.action.awsApiCallAction.remoteIpDetails.organization.asnOrg
--
-- -   service.action.awsApiCallAction.serviceName
--
-- -   service.action.dnsRequestAction.domain
--
-- -   service.action.networkConnectionAction.blocked
--
-- -   service.action.networkConnectionAction.connectionDirection
--
-- -   service.action.networkConnectionAction.localPortDetails.port
--
-- -   service.action.networkConnectionAction.protocol
--
-- -   service.action.networkConnectionAction.localIpDetails.ipAddressV4
--
-- -   service.action.networkConnectionAction.remoteIpDetails.city.cityName
--
-- -   service.action.networkConnectionAction.remoteIpDetails.country.countryName
--
-- -   service.action.networkConnectionAction.remoteIpDetails.ipAddressV4
--
-- -   service.action.networkConnectionAction.remoteIpDetails.organization.asn
--
-- -   service.action.networkConnectionAction.remoteIpDetails.organization.asnOrg
--
-- -   service.action.networkConnectionAction.remotePortDetails.port
--
-- -   service.additionalInfo.threatListName
--
-- -   service.archived
--
--     When this attribute is set to TRUE, only archived findings are
--     listed. When it\'s set to FALSE, only unarchived findings are
--     listed. When this attribute is not set, all existing findings are
--     listed.
--
-- -   service.resourceRole
--
-- -   severity
--
-- -   type
--
-- -   updatedAt
--
--     Type: ISO 8601 string format: YYYY-MM-DDTHH:MM:SS.SSSZ or
--     YYYY-MM-DDTHH:MM:SSZ depending on whether the value contains
--     milliseconds.
createFilter_findingCriteria :: Lens.Lens' CreateFilter FindingCriteria
createFilter_findingCriteria = Lens.lens (\CreateFilter' {findingCriteria} -> findingCriteria) (\s@CreateFilter' {} a -> s {findingCriteria = a} :: CreateFilter)

instance Core.AWSRequest CreateFilter where
  type AWSResponse CreateFilter = CreateFilterResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateFilterResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "name")
      )

instance Prelude.Hashable CreateFilter

instance Prelude.NFData CreateFilter

instance Core.ToHeaders CreateFilter where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateFilter where
  toJSON CreateFilter' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("rank" Core..=) Prelude.<$> rank,
            ("tags" Core..=) Prelude.<$> tags,
            ("action" Core..=) Prelude.<$> action,
            ("description" Core..=) Prelude.<$> description,
            ("clientToken" Core..=) Prelude.<$> clientToken,
            Prelude.Just ("name" Core..= name),
            Prelude.Just
              ("findingCriteria" Core..= findingCriteria)
          ]
      )

instance Core.ToPath CreateFilter where
  toPath CreateFilter' {..} =
    Prelude.mconcat
      ["/detector/", Core.toBS detectorId, "/filter"]

instance Core.ToQuery CreateFilter where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateFilterResponse' smart constructor.
data CreateFilterResponse = CreateFilterResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The name of the successfully created filter.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFilterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createFilterResponse_httpStatus' - The response's http status code.
--
-- 'name', 'createFilterResponse_name' - The name of the successfully created filter.
newCreateFilterResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'name'
  Prelude.Text ->
  CreateFilterResponse
newCreateFilterResponse pHttpStatus_ pName_ =
  CreateFilterResponse'
    { httpStatus = pHttpStatus_,
      name = pName_
    }

-- | The response's http status code.
createFilterResponse_httpStatus :: Lens.Lens' CreateFilterResponse Prelude.Int
createFilterResponse_httpStatus = Lens.lens (\CreateFilterResponse' {httpStatus} -> httpStatus) (\s@CreateFilterResponse' {} a -> s {httpStatus = a} :: CreateFilterResponse)

-- | The name of the successfully created filter.
createFilterResponse_name :: Lens.Lens' CreateFilterResponse Prelude.Text
createFilterResponse_name = Lens.lens (\CreateFilterResponse' {name} -> name) (\s@CreateFilterResponse' {} a -> s {name = a} :: CreateFilterResponse)

instance Prelude.NFData CreateFilterResponse
