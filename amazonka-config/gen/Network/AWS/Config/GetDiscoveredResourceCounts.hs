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
-- Module      : Network.AWS.Config.GetDiscoveredResourceCounts
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the resource types, the number of each resource type, and the
-- total number of resources that AWS Config is recording in this region
-- for your AWS account.
--
-- __Example__
--
-- 1.  AWS Config is recording three resource types in the US East (Ohio)
--     Region for your account: 25 EC2 instances, 20 IAM users, and 15 S3
--     buckets.
--
-- 2.  You make a call to the @GetDiscoveredResourceCounts@ action and
--     specify that you want all resource types.
--
-- 3.  AWS Config returns the following:
--
--     -   The resource types (EC2 instances, IAM users, and S3 buckets).
--
--     -   The number of each resource type (25, 20, and 15).
--
--     -   The total number of all resources (60).
--
-- The response is paginated. By default, AWS Config lists 100
-- ResourceCount objects on each page. You can customize this number with
-- the @limit@ parameter. The response includes a @nextToken@ string. To
-- get the next page of results, run the request again and specify the
-- string for the @nextToken@ parameter.
--
-- If you make a call to the GetDiscoveredResourceCounts action, you might
-- not immediately receive resource counts in the following situations:
--
-- -   You are a new AWS Config customer.
--
-- -   You just enabled resource recording.
--
-- It might take a few minutes for AWS Config to record and count your
-- resources. Wait a few minutes and then retry the
-- GetDiscoveredResourceCounts action.
module Network.AWS.Config.GetDiscoveredResourceCounts
  ( -- * Creating a Request
    GetDiscoveredResourceCounts (..),
    newGetDiscoveredResourceCounts,

    -- * Request Lenses
    getDiscoveredResourceCounts_nextToken,
    getDiscoveredResourceCounts_resourceTypes,
    getDiscoveredResourceCounts_limit,

    -- * Destructuring the Response
    GetDiscoveredResourceCountsResponse (..),
    newGetDiscoveredResourceCountsResponse,

    -- * Response Lenses
    getDiscoveredResourceCountsResponse_nextToken,
    getDiscoveredResourceCountsResponse_totalDiscoveredResources,
    getDiscoveredResourceCountsResponse_resourceCounts,
    getDiscoveredResourceCountsResponse_httpStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetDiscoveredResourceCounts' smart constructor.
data GetDiscoveredResourceCounts = GetDiscoveredResourceCounts'
  { -- | The @nextToken@ string returned on a previous page that you use to get
    -- the next page of results in a paginated response.
    nextToken :: Core.Maybe Core.Text,
    -- | The comma-separated list that specifies the resource types that you want
    -- AWS Config to return (for example, @\"AWS::EC2::Instance\"@,
    -- @\"AWS::IAM::User\"@).
    --
    -- If a value for @resourceTypes@ is not specified, AWS Config returns all
    -- resource types that AWS Config is recording in the region for your
    -- account.
    --
    -- If the configuration recorder is turned off, AWS Config returns an empty
    -- list of ResourceCount objects. If the configuration recorder is not
    -- recording a specific resource type (for example, S3 buckets), that
    -- resource type is not returned in the list of ResourceCount objects.
    resourceTypes :: Core.Maybe [Core.Text],
    -- | The maximum number of ResourceCount objects returned on each page. The
    -- default is 100. You cannot specify a number greater than 100. If you
    -- specify 0, AWS Config uses the default.
    limit :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetDiscoveredResourceCounts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getDiscoveredResourceCounts_nextToken' - The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
--
-- 'resourceTypes', 'getDiscoveredResourceCounts_resourceTypes' - The comma-separated list that specifies the resource types that you want
-- AWS Config to return (for example, @\"AWS::EC2::Instance\"@,
-- @\"AWS::IAM::User\"@).
--
-- If a value for @resourceTypes@ is not specified, AWS Config returns all
-- resource types that AWS Config is recording in the region for your
-- account.
--
-- If the configuration recorder is turned off, AWS Config returns an empty
-- list of ResourceCount objects. If the configuration recorder is not
-- recording a specific resource type (for example, S3 buckets), that
-- resource type is not returned in the list of ResourceCount objects.
--
-- 'limit', 'getDiscoveredResourceCounts_limit' - The maximum number of ResourceCount objects returned on each page. The
-- default is 100. You cannot specify a number greater than 100. If you
-- specify 0, AWS Config uses the default.
newGetDiscoveredResourceCounts ::
  GetDiscoveredResourceCounts
newGetDiscoveredResourceCounts =
  GetDiscoveredResourceCounts'
    { nextToken =
        Core.Nothing,
      resourceTypes = Core.Nothing,
      limit = Core.Nothing
    }

-- | The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
getDiscoveredResourceCounts_nextToken :: Lens.Lens' GetDiscoveredResourceCounts (Core.Maybe Core.Text)
getDiscoveredResourceCounts_nextToken = Lens.lens (\GetDiscoveredResourceCounts' {nextToken} -> nextToken) (\s@GetDiscoveredResourceCounts' {} a -> s {nextToken = a} :: GetDiscoveredResourceCounts)

-- | The comma-separated list that specifies the resource types that you want
-- AWS Config to return (for example, @\"AWS::EC2::Instance\"@,
-- @\"AWS::IAM::User\"@).
--
-- If a value for @resourceTypes@ is not specified, AWS Config returns all
-- resource types that AWS Config is recording in the region for your
-- account.
--
-- If the configuration recorder is turned off, AWS Config returns an empty
-- list of ResourceCount objects. If the configuration recorder is not
-- recording a specific resource type (for example, S3 buckets), that
-- resource type is not returned in the list of ResourceCount objects.
getDiscoveredResourceCounts_resourceTypes :: Lens.Lens' GetDiscoveredResourceCounts (Core.Maybe [Core.Text])
getDiscoveredResourceCounts_resourceTypes = Lens.lens (\GetDiscoveredResourceCounts' {resourceTypes} -> resourceTypes) (\s@GetDiscoveredResourceCounts' {} a -> s {resourceTypes = a} :: GetDiscoveredResourceCounts) Core.. Lens.mapping Lens._Coerce

-- | The maximum number of ResourceCount objects returned on each page. The
-- default is 100. You cannot specify a number greater than 100. If you
-- specify 0, AWS Config uses the default.
getDiscoveredResourceCounts_limit :: Lens.Lens' GetDiscoveredResourceCounts (Core.Maybe Core.Natural)
getDiscoveredResourceCounts_limit = Lens.lens (\GetDiscoveredResourceCounts' {limit} -> limit) (\s@GetDiscoveredResourceCounts' {} a -> s {limit = a} :: GetDiscoveredResourceCounts)

instance Core.AWSRequest GetDiscoveredResourceCounts where
  type
    AWSResponse GetDiscoveredResourceCounts =
      GetDiscoveredResourceCountsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDiscoveredResourceCountsResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "totalDiscoveredResources")
            Core.<*> (x Core..?> "resourceCounts" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetDiscoveredResourceCounts

instance Core.NFData GetDiscoveredResourceCounts

instance Core.ToHeaders GetDiscoveredResourceCounts where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StarlingDoveService.GetDiscoveredResourceCounts" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetDiscoveredResourceCounts where
  toJSON GetDiscoveredResourceCounts' {..} =
    Core.object
      ( Core.catMaybes
          [ ("nextToken" Core..=) Core.<$> nextToken,
            ("resourceTypes" Core..=) Core.<$> resourceTypes,
            ("limit" Core..=) Core.<$> limit
          ]
      )

instance Core.ToPath GetDiscoveredResourceCounts where
  toPath = Core.const "/"

instance Core.ToQuery GetDiscoveredResourceCounts where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetDiscoveredResourceCountsResponse' smart constructor.
data GetDiscoveredResourceCountsResponse = GetDiscoveredResourceCountsResponse'
  { -- | The string that you use in a subsequent request to get the next page of
    -- results in a paginated response.
    nextToken :: Core.Maybe Core.Text,
    -- | The total number of resources that AWS Config is recording in the region
    -- for your account. If you specify resource types in the request, AWS
    -- Config returns only the total number of resources for those resource
    -- types.
    --
    -- __Example__
    --
    -- 1.  AWS Config is recording three resource types in the US East (Ohio)
    --     Region for your account: 25 EC2 instances, 20 IAM users, and 15 S3
    --     buckets, for a total of 60 resources.
    --
    -- 2.  You make a call to the @GetDiscoveredResourceCounts@ action and
    --     specify the resource type, @\"AWS::EC2::Instances\"@, in the
    --     request.
    --
    -- 3.  AWS Config returns 25 for @totalDiscoveredResources@.
    totalDiscoveredResources :: Core.Maybe Core.Integer,
    -- | The list of @ResourceCount@ objects. Each object is listed in descending
    -- order by the number of resources.
    resourceCounts :: Core.Maybe [ResourceCount],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetDiscoveredResourceCountsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getDiscoveredResourceCountsResponse_nextToken' - The string that you use in a subsequent request to get the next page of
-- results in a paginated response.
--
-- 'totalDiscoveredResources', 'getDiscoveredResourceCountsResponse_totalDiscoveredResources' - The total number of resources that AWS Config is recording in the region
-- for your account. If you specify resource types in the request, AWS
-- Config returns only the total number of resources for those resource
-- types.
--
-- __Example__
--
-- 1.  AWS Config is recording three resource types in the US East (Ohio)
--     Region for your account: 25 EC2 instances, 20 IAM users, and 15 S3
--     buckets, for a total of 60 resources.
--
-- 2.  You make a call to the @GetDiscoveredResourceCounts@ action and
--     specify the resource type, @\"AWS::EC2::Instances\"@, in the
--     request.
--
-- 3.  AWS Config returns 25 for @totalDiscoveredResources@.
--
-- 'resourceCounts', 'getDiscoveredResourceCountsResponse_resourceCounts' - The list of @ResourceCount@ objects. Each object is listed in descending
-- order by the number of resources.
--
-- 'httpStatus', 'getDiscoveredResourceCountsResponse_httpStatus' - The response's http status code.
newGetDiscoveredResourceCountsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetDiscoveredResourceCountsResponse
newGetDiscoveredResourceCountsResponse pHttpStatus_ =
  GetDiscoveredResourceCountsResponse'
    { nextToken =
        Core.Nothing,
      totalDiscoveredResources =
        Core.Nothing,
      resourceCounts = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The string that you use in a subsequent request to get the next page of
-- results in a paginated response.
getDiscoveredResourceCountsResponse_nextToken :: Lens.Lens' GetDiscoveredResourceCountsResponse (Core.Maybe Core.Text)
getDiscoveredResourceCountsResponse_nextToken = Lens.lens (\GetDiscoveredResourceCountsResponse' {nextToken} -> nextToken) (\s@GetDiscoveredResourceCountsResponse' {} a -> s {nextToken = a} :: GetDiscoveredResourceCountsResponse)

-- | The total number of resources that AWS Config is recording in the region
-- for your account. If you specify resource types in the request, AWS
-- Config returns only the total number of resources for those resource
-- types.
--
-- __Example__
--
-- 1.  AWS Config is recording three resource types in the US East (Ohio)
--     Region for your account: 25 EC2 instances, 20 IAM users, and 15 S3
--     buckets, for a total of 60 resources.
--
-- 2.  You make a call to the @GetDiscoveredResourceCounts@ action and
--     specify the resource type, @\"AWS::EC2::Instances\"@, in the
--     request.
--
-- 3.  AWS Config returns 25 for @totalDiscoveredResources@.
getDiscoveredResourceCountsResponse_totalDiscoveredResources :: Lens.Lens' GetDiscoveredResourceCountsResponse (Core.Maybe Core.Integer)
getDiscoveredResourceCountsResponse_totalDiscoveredResources = Lens.lens (\GetDiscoveredResourceCountsResponse' {totalDiscoveredResources} -> totalDiscoveredResources) (\s@GetDiscoveredResourceCountsResponse' {} a -> s {totalDiscoveredResources = a} :: GetDiscoveredResourceCountsResponse)

-- | The list of @ResourceCount@ objects. Each object is listed in descending
-- order by the number of resources.
getDiscoveredResourceCountsResponse_resourceCounts :: Lens.Lens' GetDiscoveredResourceCountsResponse (Core.Maybe [ResourceCount])
getDiscoveredResourceCountsResponse_resourceCounts = Lens.lens (\GetDiscoveredResourceCountsResponse' {resourceCounts} -> resourceCounts) (\s@GetDiscoveredResourceCountsResponse' {} a -> s {resourceCounts = a} :: GetDiscoveredResourceCountsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getDiscoveredResourceCountsResponse_httpStatus :: Lens.Lens' GetDiscoveredResourceCountsResponse Core.Int
getDiscoveredResourceCountsResponse_httpStatus = Lens.lens (\GetDiscoveredResourceCountsResponse' {httpStatus} -> httpStatus) (\s@GetDiscoveredResourceCountsResponse' {} a -> s {httpStatus = a} :: GetDiscoveredResourceCountsResponse)

instance
  Core.NFData
    GetDiscoveredResourceCountsResponse
