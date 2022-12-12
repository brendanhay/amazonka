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
-- Module      : Amazonka.Config.GetDiscoveredResourceCounts
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the resource types, the number of each resource type, and the
-- total number of resources that Config is recording in this region for
-- your Amazon Web Services account.
--
-- __Example__
--
-- 1.  Config is recording three resource types in the US East (Ohio)
--     Region for your account: 25 EC2 instances, 20 IAM users, and 15 S3
--     buckets.
--
-- 2.  You make a call to the @GetDiscoveredResourceCounts@ action and
--     specify that you want all resource types.
--
-- 3.  Config returns the following:
--
--     -   The resource types (EC2 instances, IAM users, and S3 buckets).
--
--     -   The number of each resource type (25, 20, and 15).
--
--     -   The total number of all resources (60).
--
-- The response is paginated. By default, Config lists 100 ResourceCount
-- objects on each page. You can customize this number with the @limit@
-- parameter. The response includes a @nextToken@ string. To get the next
-- page of results, run the request again and specify the string for the
-- @nextToken@ parameter.
--
-- If you make a call to the GetDiscoveredResourceCounts action, you might
-- not immediately receive resource counts in the following situations:
--
-- -   You are a new Config customer.
--
-- -   You just enabled resource recording.
--
-- It might take a few minutes for Config to record and count your
-- resources. Wait a few minutes and then retry the
-- GetDiscoveredResourceCounts action.
module Amazonka.Config.GetDiscoveredResourceCounts
  ( -- * Creating a Request
    GetDiscoveredResourceCounts (..),
    newGetDiscoveredResourceCounts,

    -- * Request Lenses
    getDiscoveredResourceCounts_limit,
    getDiscoveredResourceCounts_nextToken,
    getDiscoveredResourceCounts_resourceTypes,

    -- * Destructuring the Response
    GetDiscoveredResourceCountsResponse (..),
    newGetDiscoveredResourceCountsResponse,

    -- * Response Lenses
    getDiscoveredResourceCountsResponse_nextToken,
    getDiscoveredResourceCountsResponse_resourceCounts,
    getDiscoveredResourceCountsResponse_totalDiscoveredResources,
    getDiscoveredResourceCountsResponse_httpStatus,
  )
where

import Amazonka.Config.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetDiscoveredResourceCounts' smart constructor.
data GetDiscoveredResourceCounts = GetDiscoveredResourceCounts'
  { -- | The maximum number of ResourceCount objects returned on each page. The
    -- default is 100. You cannot specify a number greater than 100. If you
    -- specify 0, Config uses the default.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | The @nextToken@ string returned on a previous page that you use to get
    -- the next page of results in a paginated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The comma-separated list that specifies the resource types that you want
    -- Config to return (for example, @\"AWS::EC2::Instance\"@,
    -- @\"AWS::IAM::User\"@).
    --
    -- If a value for @resourceTypes@ is not specified, Config returns all
    -- resource types that Config is recording in the region for your account.
    --
    -- If the configuration recorder is turned off, Config returns an empty
    -- list of ResourceCount objects. If the configuration recorder is not
    -- recording a specific resource type (for example, S3 buckets), that
    -- resource type is not returned in the list of ResourceCount objects.
    resourceTypes :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDiscoveredResourceCounts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'limit', 'getDiscoveredResourceCounts_limit' - The maximum number of ResourceCount objects returned on each page. The
-- default is 100. You cannot specify a number greater than 100. If you
-- specify 0, Config uses the default.
--
-- 'nextToken', 'getDiscoveredResourceCounts_nextToken' - The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
--
-- 'resourceTypes', 'getDiscoveredResourceCounts_resourceTypes' - The comma-separated list that specifies the resource types that you want
-- Config to return (for example, @\"AWS::EC2::Instance\"@,
-- @\"AWS::IAM::User\"@).
--
-- If a value for @resourceTypes@ is not specified, Config returns all
-- resource types that Config is recording in the region for your account.
--
-- If the configuration recorder is turned off, Config returns an empty
-- list of ResourceCount objects. If the configuration recorder is not
-- recording a specific resource type (for example, S3 buckets), that
-- resource type is not returned in the list of ResourceCount objects.
newGetDiscoveredResourceCounts ::
  GetDiscoveredResourceCounts
newGetDiscoveredResourceCounts =
  GetDiscoveredResourceCounts'
    { limit =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      resourceTypes = Prelude.Nothing
    }

-- | The maximum number of ResourceCount objects returned on each page. The
-- default is 100. You cannot specify a number greater than 100. If you
-- specify 0, Config uses the default.
getDiscoveredResourceCounts_limit :: Lens.Lens' GetDiscoveredResourceCounts (Prelude.Maybe Prelude.Natural)
getDiscoveredResourceCounts_limit = Lens.lens (\GetDiscoveredResourceCounts' {limit} -> limit) (\s@GetDiscoveredResourceCounts' {} a -> s {limit = a} :: GetDiscoveredResourceCounts)

-- | The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
getDiscoveredResourceCounts_nextToken :: Lens.Lens' GetDiscoveredResourceCounts (Prelude.Maybe Prelude.Text)
getDiscoveredResourceCounts_nextToken = Lens.lens (\GetDiscoveredResourceCounts' {nextToken} -> nextToken) (\s@GetDiscoveredResourceCounts' {} a -> s {nextToken = a} :: GetDiscoveredResourceCounts)

-- | The comma-separated list that specifies the resource types that you want
-- Config to return (for example, @\"AWS::EC2::Instance\"@,
-- @\"AWS::IAM::User\"@).
--
-- If a value for @resourceTypes@ is not specified, Config returns all
-- resource types that Config is recording in the region for your account.
--
-- If the configuration recorder is turned off, Config returns an empty
-- list of ResourceCount objects. If the configuration recorder is not
-- recording a specific resource type (for example, S3 buckets), that
-- resource type is not returned in the list of ResourceCount objects.
getDiscoveredResourceCounts_resourceTypes :: Lens.Lens' GetDiscoveredResourceCounts (Prelude.Maybe [Prelude.Text])
getDiscoveredResourceCounts_resourceTypes = Lens.lens (\GetDiscoveredResourceCounts' {resourceTypes} -> resourceTypes) (\s@GetDiscoveredResourceCounts' {} a -> s {resourceTypes = a} :: GetDiscoveredResourceCounts) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSRequest GetDiscoveredResourceCounts where
  type
    AWSResponse GetDiscoveredResourceCounts =
      GetDiscoveredResourceCountsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDiscoveredResourceCountsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "resourceCounts" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "totalDiscoveredResources")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetDiscoveredResourceCounts where
  hashWithSalt _salt GetDiscoveredResourceCounts' {..} =
    _salt `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` resourceTypes

instance Prelude.NFData GetDiscoveredResourceCounts where
  rnf GetDiscoveredResourceCounts' {..} =
    Prelude.rnf limit
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf resourceTypes

instance Data.ToHeaders GetDiscoveredResourceCounts where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StarlingDoveService.GetDiscoveredResourceCounts" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetDiscoveredResourceCounts where
  toJSON GetDiscoveredResourceCounts' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("limit" Data..=) Prelude.<$> limit,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            ("resourceTypes" Data..=) Prelude.<$> resourceTypes
          ]
      )

instance Data.ToPath GetDiscoveredResourceCounts where
  toPath = Prelude.const "/"

instance Data.ToQuery GetDiscoveredResourceCounts where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetDiscoveredResourceCountsResponse' smart constructor.
data GetDiscoveredResourceCountsResponse = GetDiscoveredResourceCountsResponse'
  { -- | The string that you use in a subsequent request to get the next page of
    -- results in a paginated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of @ResourceCount@ objects. Each object is listed in descending
    -- order by the number of resources.
    resourceCounts :: Prelude.Maybe [ResourceCount],
    -- | The total number of resources that Config is recording in the region for
    -- your account. If you specify resource types in the request, Config
    -- returns only the total number of resources for those resource types.
    --
    -- __Example__
    --
    -- 1.  Config is recording three resource types in the US East (Ohio)
    --     Region for your account: 25 EC2 instances, 20 IAM users, and 15 S3
    --     buckets, for a total of 60 resources.
    --
    -- 2.  You make a call to the @GetDiscoveredResourceCounts@ action and
    --     specify the resource type, @\"AWS::EC2::Instances\"@, in the
    --     request.
    --
    -- 3.  Config returns 25 for @totalDiscoveredResources@.
    totalDiscoveredResources :: Prelude.Maybe Prelude.Integer,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'resourceCounts', 'getDiscoveredResourceCountsResponse_resourceCounts' - The list of @ResourceCount@ objects. Each object is listed in descending
-- order by the number of resources.
--
-- 'totalDiscoveredResources', 'getDiscoveredResourceCountsResponse_totalDiscoveredResources' - The total number of resources that Config is recording in the region for
-- your account. If you specify resource types in the request, Config
-- returns only the total number of resources for those resource types.
--
-- __Example__
--
-- 1.  Config is recording three resource types in the US East (Ohio)
--     Region for your account: 25 EC2 instances, 20 IAM users, and 15 S3
--     buckets, for a total of 60 resources.
--
-- 2.  You make a call to the @GetDiscoveredResourceCounts@ action and
--     specify the resource type, @\"AWS::EC2::Instances\"@, in the
--     request.
--
-- 3.  Config returns 25 for @totalDiscoveredResources@.
--
-- 'httpStatus', 'getDiscoveredResourceCountsResponse_httpStatus' - The response's http status code.
newGetDiscoveredResourceCountsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetDiscoveredResourceCountsResponse
newGetDiscoveredResourceCountsResponse pHttpStatus_ =
  GetDiscoveredResourceCountsResponse'
    { nextToken =
        Prelude.Nothing,
      resourceCounts = Prelude.Nothing,
      totalDiscoveredResources =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The string that you use in a subsequent request to get the next page of
-- results in a paginated response.
getDiscoveredResourceCountsResponse_nextToken :: Lens.Lens' GetDiscoveredResourceCountsResponse (Prelude.Maybe Prelude.Text)
getDiscoveredResourceCountsResponse_nextToken = Lens.lens (\GetDiscoveredResourceCountsResponse' {nextToken} -> nextToken) (\s@GetDiscoveredResourceCountsResponse' {} a -> s {nextToken = a} :: GetDiscoveredResourceCountsResponse)

-- | The list of @ResourceCount@ objects. Each object is listed in descending
-- order by the number of resources.
getDiscoveredResourceCountsResponse_resourceCounts :: Lens.Lens' GetDiscoveredResourceCountsResponse (Prelude.Maybe [ResourceCount])
getDiscoveredResourceCountsResponse_resourceCounts = Lens.lens (\GetDiscoveredResourceCountsResponse' {resourceCounts} -> resourceCounts) (\s@GetDiscoveredResourceCountsResponse' {} a -> s {resourceCounts = a} :: GetDiscoveredResourceCountsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The total number of resources that Config is recording in the region for
-- your account. If you specify resource types in the request, Config
-- returns only the total number of resources for those resource types.
--
-- __Example__
--
-- 1.  Config is recording three resource types in the US East (Ohio)
--     Region for your account: 25 EC2 instances, 20 IAM users, and 15 S3
--     buckets, for a total of 60 resources.
--
-- 2.  You make a call to the @GetDiscoveredResourceCounts@ action and
--     specify the resource type, @\"AWS::EC2::Instances\"@, in the
--     request.
--
-- 3.  Config returns 25 for @totalDiscoveredResources@.
getDiscoveredResourceCountsResponse_totalDiscoveredResources :: Lens.Lens' GetDiscoveredResourceCountsResponse (Prelude.Maybe Prelude.Integer)
getDiscoveredResourceCountsResponse_totalDiscoveredResources = Lens.lens (\GetDiscoveredResourceCountsResponse' {totalDiscoveredResources} -> totalDiscoveredResources) (\s@GetDiscoveredResourceCountsResponse' {} a -> s {totalDiscoveredResources = a} :: GetDiscoveredResourceCountsResponse)

-- | The response's http status code.
getDiscoveredResourceCountsResponse_httpStatus :: Lens.Lens' GetDiscoveredResourceCountsResponse Prelude.Int
getDiscoveredResourceCountsResponse_httpStatus = Lens.lens (\GetDiscoveredResourceCountsResponse' {httpStatus} -> httpStatus) (\s@GetDiscoveredResourceCountsResponse' {} a -> s {httpStatus = a} :: GetDiscoveredResourceCountsResponse)

instance
  Prelude.NFData
    GetDiscoveredResourceCountsResponse
  where
  rnf GetDiscoveredResourceCountsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf resourceCounts
      `Prelude.seq` Prelude.rnf totalDiscoveredResources
      `Prelude.seq` Prelude.rnf httpStatus
