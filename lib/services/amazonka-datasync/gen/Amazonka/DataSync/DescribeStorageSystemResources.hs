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
-- Module      : Amazonka.DataSync.DescribeStorageSystemResources
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information that DataSync Discovery collects about resources in
-- your on-premises storage system.
module Amazonka.DataSync.DescribeStorageSystemResources
  ( -- * Creating a Request
    DescribeStorageSystemResources (..),
    newDescribeStorageSystemResources,

    -- * Request Lenses
    describeStorageSystemResources_filter,
    describeStorageSystemResources_maxResults,
    describeStorageSystemResources_nextToken,
    describeStorageSystemResources_resourceIds,
    describeStorageSystemResources_discoveryJobArn,
    describeStorageSystemResources_resourceType,

    -- * Destructuring the Response
    DescribeStorageSystemResourcesResponse (..),
    newDescribeStorageSystemResourcesResponse,

    -- * Response Lenses
    describeStorageSystemResourcesResponse_nextToken,
    describeStorageSystemResourcesResponse_resourceDetails,
    describeStorageSystemResourcesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataSync.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeStorageSystemResources' smart constructor.
data DescribeStorageSystemResources = DescribeStorageSystemResources'
  { -- | Filters the storage system resources that you want returned. For
    -- example, this might be volumes associated with a specific storage
    -- virtual machine (SVM).
    filter' :: Prelude.Maybe (Prelude.HashMap DiscoveryResourceFilter [Prelude.Text]),
    -- | Specifies the maximum number of storage system resources that you want
    -- to list in a response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Specifies an opaque string that indicates the position to begin the next
    -- list of results in the response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Specifies the universally unique identifiers (UUIDs) of the storage
    -- system resources that you want information about. You can\'t use this
    -- parameter in combination with the @Filter@ parameter.
    resourceIds :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | Specifies the Amazon Resource Name (ARN) of the discovery job that\'s
    -- collecting data from your on-premises storage system.
    discoveryJobArn :: Prelude.Text,
    -- | Specifies what kind of storage system resources that you want
    -- information about.
    resourceType :: DiscoveryResourceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeStorageSystemResources' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filter'', 'describeStorageSystemResources_filter' - Filters the storage system resources that you want returned. For
-- example, this might be volumes associated with a specific storage
-- virtual machine (SVM).
--
-- 'maxResults', 'describeStorageSystemResources_maxResults' - Specifies the maximum number of storage system resources that you want
-- to list in a response.
--
-- 'nextToken', 'describeStorageSystemResources_nextToken' - Specifies an opaque string that indicates the position to begin the next
-- list of results in the response.
--
-- 'resourceIds', 'describeStorageSystemResources_resourceIds' - Specifies the universally unique identifiers (UUIDs) of the storage
-- system resources that you want information about. You can\'t use this
-- parameter in combination with the @Filter@ parameter.
--
-- 'discoveryJobArn', 'describeStorageSystemResources_discoveryJobArn' - Specifies the Amazon Resource Name (ARN) of the discovery job that\'s
-- collecting data from your on-premises storage system.
--
-- 'resourceType', 'describeStorageSystemResources_resourceType' - Specifies what kind of storage system resources that you want
-- information about.
newDescribeStorageSystemResources ::
  -- | 'discoveryJobArn'
  Prelude.Text ->
  -- | 'resourceType'
  DiscoveryResourceType ->
  DescribeStorageSystemResources
newDescribeStorageSystemResources
  pDiscoveryJobArn_
  pResourceType_ =
    DescribeStorageSystemResources'
      { filter' =
          Prelude.Nothing,
        maxResults = Prelude.Nothing,
        nextToken = Prelude.Nothing,
        resourceIds = Prelude.Nothing,
        discoveryJobArn = pDiscoveryJobArn_,
        resourceType = pResourceType_
      }

-- | Filters the storage system resources that you want returned. For
-- example, this might be volumes associated with a specific storage
-- virtual machine (SVM).
describeStorageSystemResources_filter :: Lens.Lens' DescribeStorageSystemResources (Prelude.Maybe (Prelude.HashMap DiscoveryResourceFilter [Prelude.Text]))
describeStorageSystemResources_filter = Lens.lens (\DescribeStorageSystemResources' {filter'} -> filter') (\s@DescribeStorageSystemResources' {} a -> s {filter' = a} :: DescribeStorageSystemResources) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the maximum number of storage system resources that you want
-- to list in a response.
describeStorageSystemResources_maxResults :: Lens.Lens' DescribeStorageSystemResources (Prelude.Maybe Prelude.Natural)
describeStorageSystemResources_maxResults = Lens.lens (\DescribeStorageSystemResources' {maxResults} -> maxResults) (\s@DescribeStorageSystemResources' {} a -> s {maxResults = a} :: DescribeStorageSystemResources)

-- | Specifies an opaque string that indicates the position to begin the next
-- list of results in the response.
describeStorageSystemResources_nextToken :: Lens.Lens' DescribeStorageSystemResources (Prelude.Maybe Prelude.Text)
describeStorageSystemResources_nextToken = Lens.lens (\DescribeStorageSystemResources' {nextToken} -> nextToken) (\s@DescribeStorageSystemResources' {} a -> s {nextToken = a} :: DescribeStorageSystemResources)

-- | Specifies the universally unique identifiers (UUIDs) of the storage
-- system resources that you want information about. You can\'t use this
-- parameter in combination with the @Filter@ parameter.
describeStorageSystemResources_resourceIds :: Lens.Lens' DescribeStorageSystemResources (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
describeStorageSystemResources_resourceIds = Lens.lens (\DescribeStorageSystemResources' {resourceIds} -> resourceIds) (\s@DescribeStorageSystemResources' {} a -> s {resourceIds = a} :: DescribeStorageSystemResources) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the Amazon Resource Name (ARN) of the discovery job that\'s
-- collecting data from your on-premises storage system.
describeStorageSystemResources_discoveryJobArn :: Lens.Lens' DescribeStorageSystemResources Prelude.Text
describeStorageSystemResources_discoveryJobArn = Lens.lens (\DescribeStorageSystemResources' {discoveryJobArn} -> discoveryJobArn) (\s@DescribeStorageSystemResources' {} a -> s {discoveryJobArn = a} :: DescribeStorageSystemResources)

-- | Specifies what kind of storage system resources that you want
-- information about.
describeStorageSystemResources_resourceType :: Lens.Lens' DescribeStorageSystemResources DiscoveryResourceType
describeStorageSystemResources_resourceType = Lens.lens (\DescribeStorageSystemResources' {resourceType} -> resourceType) (\s@DescribeStorageSystemResources' {} a -> s {resourceType = a} :: DescribeStorageSystemResources)

instance
  Core.AWSRequest
    DescribeStorageSystemResources
  where
  type
    AWSResponse DescribeStorageSystemResources =
      DescribeStorageSystemResourcesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeStorageSystemResourcesResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "ResourceDetails")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeStorageSystemResources
  where
  hashWithSalt
    _salt
    DescribeStorageSystemResources' {..} =
      _salt
        `Prelude.hashWithSalt` filter'
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` resourceIds
        `Prelude.hashWithSalt` discoveryJobArn
        `Prelude.hashWithSalt` resourceType

instance
  Prelude.NFData
    DescribeStorageSystemResources
  where
  rnf DescribeStorageSystemResources' {..} =
    Prelude.rnf filter'
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf resourceIds
      `Prelude.seq` Prelude.rnf discoveryJobArn
      `Prelude.seq` Prelude.rnf resourceType

instance
  Data.ToHeaders
    DescribeStorageSystemResources
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "FmrsService.DescribeStorageSystemResources" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeStorageSystemResources where
  toJSON DescribeStorageSystemResources' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Filter" Data..=) Prelude.<$> filter',
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("ResourceIds" Data..=) Prelude.<$> resourceIds,
            Prelude.Just
              ("DiscoveryJobArn" Data..= discoveryJobArn),
            Prelude.Just ("ResourceType" Data..= resourceType)
          ]
      )

instance Data.ToPath DescribeStorageSystemResources where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeStorageSystemResources where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeStorageSystemResourcesResponse' smart constructor.
data DescribeStorageSystemResourcesResponse = DescribeStorageSystemResourcesResponse'
  { -- | The opaque string that indicates the position to begin the next list of
    -- results in the response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The information collected about your storage system\'s resources. A
    -- response can also include Amazon Web Services storage service
    -- recommendations.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/datasync/latest/userguide/discovery-understand-findings.html storage resource information>
    -- collected by and
    -- <https://docs.aws.amazon.com/datasync/latest/userguide/discovery-understand-recommendations.html recommendations>
    -- provided by DataSync Discovery.
    resourceDetails :: Prelude.Maybe ResourceDetails,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeStorageSystemResourcesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeStorageSystemResourcesResponse_nextToken' - The opaque string that indicates the position to begin the next list of
-- results in the response.
--
-- 'resourceDetails', 'describeStorageSystemResourcesResponse_resourceDetails' - The information collected about your storage system\'s resources. A
-- response can also include Amazon Web Services storage service
-- recommendations.
--
-- For more information, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/discovery-understand-findings.html storage resource information>
-- collected by and
-- <https://docs.aws.amazon.com/datasync/latest/userguide/discovery-understand-recommendations.html recommendations>
-- provided by DataSync Discovery.
--
-- 'httpStatus', 'describeStorageSystemResourcesResponse_httpStatus' - The response's http status code.
newDescribeStorageSystemResourcesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeStorageSystemResourcesResponse
newDescribeStorageSystemResourcesResponse
  pHttpStatus_ =
    DescribeStorageSystemResourcesResponse'
      { nextToken =
          Prelude.Nothing,
        resourceDetails = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The opaque string that indicates the position to begin the next list of
-- results in the response.
describeStorageSystemResourcesResponse_nextToken :: Lens.Lens' DescribeStorageSystemResourcesResponse (Prelude.Maybe Prelude.Text)
describeStorageSystemResourcesResponse_nextToken = Lens.lens (\DescribeStorageSystemResourcesResponse' {nextToken} -> nextToken) (\s@DescribeStorageSystemResourcesResponse' {} a -> s {nextToken = a} :: DescribeStorageSystemResourcesResponse)

-- | The information collected about your storage system\'s resources. A
-- response can also include Amazon Web Services storage service
-- recommendations.
--
-- For more information, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/discovery-understand-findings.html storage resource information>
-- collected by and
-- <https://docs.aws.amazon.com/datasync/latest/userguide/discovery-understand-recommendations.html recommendations>
-- provided by DataSync Discovery.
describeStorageSystemResourcesResponse_resourceDetails :: Lens.Lens' DescribeStorageSystemResourcesResponse (Prelude.Maybe ResourceDetails)
describeStorageSystemResourcesResponse_resourceDetails = Lens.lens (\DescribeStorageSystemResourcesResponse' {resourceDetails} -> resourceDetails) (\s@DescribeStorageSystemResourcesResponse' {} a -> s {resourceDetails = a} :: DescribeStorageSystemResourcesResponse)

-- | The response's http status code.
describeStorageSystemResourcesResponse_httpStatus :: Lens.Lens' DescribeStorageSystemResourcesResponse Prelude.Int
describeStorageSystemResourcesResponse_httpStatus = Lens.lens (\DescribeStorageSystemResourcesResponse' {httpStatus} -> httpStatus) (\s@DescribeStorageSystemResourcesResponse' {} a -> s {httpStatus = a} :: DescribeStorageSystemResourcesResponse)

instance
  Prelude.NFData
    DescribeStorageSystemResourcesResponse
  where
  rnf DescribeStorageSystemResourcesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf resourceDetails
      `Prelude.seq` Prelude.rnf httpStatus
