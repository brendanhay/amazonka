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
-- Module      : Amazonka.Redshift.DescribeTags
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of tags. You can return tags from a specific resource by
-- specifying an ARN, or you can return all tags for a given type of
-- resource, such as clusters, snapshots, and so on.
--
-- The following are limitations for @DescribeTags@:
--
-- -   You cannot specify an ARN and a resource-type value together in the
--     same request.
--
-- -   You cannot use the @MaxRecords@ and @Marker@ parameters together
--     with the ARN parameter.
--
-- -   The @MaxRecords@ parameter can be a range from 10 to 50 results to
--     return in a request.
--
-- If you specify both tag keys and tag values in the same request, Amazon
-- Redshift returns all resources that match any combination of the
-- specified keys and values. For example, if you have @owner@ and
-- @environment@ for tag keys, and @admin@ and @test@ for tag values, all
-- resources that have any combination of those values are returned.
--
-- If both tag keys and values are omitted from the request, resources are
-- returned regardless of whether they have tag keys or values associated
-- with them.
--
-- This operation returns paginated results.
module Amazonka.Redshift.DescribeTags
  ( -- * Creating a Request
    DescribeTags (..),
    newDescribeTags,

    -- * Request Lenses
    describeTags_resourceType,
    describeTags_tagKeys,
    describeTags_marker,
    describeTags_resourceName,
    describeTags_tagValues,
    describeTags_maxRecords,

    -- * Destructuring the Response
    DescribeTagsResponse (..),
    newDescribeTagsResponse,

    -- * Response Lenses
    describeTagsResponse_marker,
    describeTagsResponse_taggedResources,
    describeTagsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newDescribeTags' smart constructor.
data DescribeTags = DescribeTags'
  { -- | The type of resource with which you want to view tags. Valid resource
    -- types are:
    --
    -- -   Cluster
    --
    -- -   CIDR\/IP
    --
    -- -   EC2 security group
    --
    -- -   Snapshot
    --
    -- -   Cluster security group
    --
    -- -   Subnet group
    --
    -- -   HSM connection
    --
    -- -   HSM certificate
    --
    -- -   Parameter group
    --
    -- -   Snapshot copy grant
    --
    -- For more information about Amazon Redshift resource types and
    -- constructing ARNs, go to
    -- <https://docs.aws.amazon.com/redshift/latest/mgmt/redshift-iam-access-control-overview.html#redshift-iam-access-control-specify-actions Specifying Policy Elements: Actions, Effects, Resources, and Principals>
    -- in the Amazon Redshift Cluster Management Guide.
    resourceType :: Prelude.Maybe Prelude.Text,
    -- | A tag key or keys for which you want to return all matching resources
    -- that are associated with the specified key or keys. For example, suppose
    -- that you have resources tagged with keys called @owner@ and
    -- @environment@. If you specify both of these tag keys in the request,
    -- Amazon Redshift returns a response with all resources that have either
    -- or both of these tag keys associated with them.
    tagKeys :: Prelude.Maybe [Prelude.Text],
    -- | A value that indicates the starting point for the next set of response
    -- records in a subsequent request. If a value is returned in a response,
    -- you can retrieve the next set of records by providing this returned
    -- marker value in the @marker@ parameter and retrying the command. If the
    -- @marker@ field is empty, all response records have been retrieved for
    -- the request.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) for which you want to describe the tag or
    -- tags. For example, @arn:aws:redshift:us-east-2:123456789:cluster:t1@.
    resourceName :: Prelude.Maybe Prelude.Text,
    -- | A tag value or values for which you want to return all matching
    -- resources that are associated with the specified value or values. For
    -- example, suppose that you have resources tagged with values called
    -- @admin@ and @test@. If you specify both of these tag values in the
    -- request, Amazon Redshift returns a response with all resources that have
    -- either or both of these tag values associated with them.
    tagValues :: Prelude.Maybe [Prelude.Text],
    -- | The maximum number or response records to return in each call. If the
    -- number of remaining response records exceeds the specified @MaxRecords@
    -- value, a value is returned in a @marker@ field of the response. You can
    -- retrieve the next set of records by retrying the command with the
    -- returned @marker@ value.
    maxRecords :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTags' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceType', 'describeTags_resourceType' - The type of resource with which you want to view tags. Valid resource
-- types are:
--
-- -   Cluster
--
-- -   CIDR\/IP
--
-- -   EC2 security group
--
-- -   Snapshot
--
-- -   Cluster security group
--
-- -   Subnet group
--
-- -   HSM connection
--
-- -   HSM certificate
--
-- -   Parameter group
--
-- -   Snapshot copy grant
--
-- For more information about Amazon Redshift resource types and
-- constructing ARNs, go to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/redshift-iam-access-control-overview.html#redshift-iam-access-control-specify-actions Specifying Policy Elements: Actions, Effects, Resources, and Principals>
-- in the Amazon Redshift Cluster Management Guide.
--
-- 'tagKeys', 'describeTags_tagKeys' - A tag key or keys for which you want to return all matching resources
-- that are associated with the specified key or keys. For example, suppose
-- that you have resources tagged with keys called @owner@ and
-- @environment@. If you specify both of these tag keys in the request,
-- Amazon Redshift returns a response with all resources that have either
-- or both of these tag keys associated with them.
--
-- 'marker', 'describeTags_marker' - A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the @marker@ parameter and retrying the command. If the
-- @marker@ field is empty, all response records have been retrieved for
-- the request.
--
-- 'resourceName', 'describeTags_resourceName' - The Amazon Resource Name (ARN) for which you want to describe the tag or
-- tags. For example, @arn:aws:redshift:us-east-2:123456789:cluster:t1@.
--
-- 'tagValues', 'describeTags_tagValues' - A tag value or values for which you want to return all matching
-- resources that are associated with the specified value or values. For
-- example, suppose that you have resources tagged with values called
-- @admin@ and @test@. If you specify both of these tag values in the
-- request, Amazon Redshift returns a response with all resources that have
-- either or both of these tag values associated with them.
--
-- 'maxRecords', 'describeTags_maxRecords' - The maximum number or response records to return in each call. If the
-- number of remaining response records exceeds the specified @MaxRecords@
-- value, a value is returned in a @marker@ field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned @marker@ value.
newDescribeTags ::
  DescribeTags
newDescribeTags =
  DescribeTags'
    { resourceType = Prelude.Nothing,
      tagKeys = Prelude.Nothing,
      marker = Prelude.Nothing,
      resourceName = Prelude.Nothing,
      tagValues = Prelude.Nothing,
      maxRecords = Prelude.Nothing
    }

-- | The type of resource with which you want to view tags. Valid resource
-- types are:
--
-- -   Cluster
--
-- -   CIDR\/IP
--
-- -   EC2 security group
--
-- -   Snapshot
--
-- -   Cluster security group
--
-- -   Subnet group
--
-- -   HSM connection
--
-- -   HSM certificate
--
-- -   Parameter group
--
-- -   Snapshot copy grant
--
-- For more information about Amazon Redshift resource types and
-- constructing ARNs, go to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/redshift-iam-access-control-overview.html#redshift-iam-access-control-specify-actions Specifying Policy Elements: Actions, Effects, Resources, and Principals>
-- in the Amazon Redshift Cluster Management Guide.
describeTags_resourceType :: Lens.Lens' DescribeTags (Prelude.Maybe Prelude.Text)
describeTags_resourceType = Lens.lens (\DescribeTags' {resourceType} -> resourceType) (\s@DescribeTags' {} a -> s {resourceType = a} :: DescribeTags)

-- | A tag key or keys for which you want to return all matching resources
-- that are associated with the specified key or keys. For example, suppose
-- that you have resources tagged with keys called @owner@ and
-- @environment@. If you specify both of these tag keys in the request,
-- Amazon Redshift returns a response with all resources that have either
-- or both of these tag keys associated with them.
describeTags_tagKeys :: Lens.Lens' DescribeTags (Prelude.Maybe [Prelude.Text])
describeTags_tagKeys = Lens.lens (\DescribeTags' {tagKeys} -> tagKeys) (\s@DescribeTags' {} a -> s {tagKeys = a} :: DescribeTags) Prelude.. Lens.mapping Lens.coerced

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the @marker@ parameter and retrying the command. If the
-- @marker@ field is empty, all response records have been retrieved for
-- the request.
describeTags_marker :: Lens.Lens' DescribeTags (Prelude.Maybe Prelude.Text)
describeTags_marker = Lens.lens (\DescribeTags' {marker} -> marker) (\s@DescribeTags' {} a -> s {marker = a} :: DescribeTags)

-- | The Amazon Resource Name (ARN) for which you want to describe the tag or
-- tags. For example, @arn:aws:redshift:us-east-2:123456789:cluster:t1@.
describeTags_resourceName :: Lens.Lens' DescribeTags (Prelude.Maybe Prelude.Text)
describeTags_resourceName = Lens.lens (\DescribeTags' {resourceName} -> resourceName) (\s@DescribeTags' {} a -> s {resourceName = a} :: DescribeTags)

-- | A tag value or values for which you want to return all matching
-- resources that are associated with the specified value or values. For
-- example, suppose that you have resources tagged with values called
-- @admin@ and @test@. If you specify both of these tag values in the
-- request, Amazon Redshift returns a response with all resources that have
-- either or both of these tag values associated with them.
describeTags_tagValues :: Lens.Lens' DescribeTags (Prelude.Maybe [Prelude.Text])
describeTags_tagValues = Lens.lens (\DescribeTags' {tagValues} -> tagValues) (\s@DescribeTags' {} a -> s {tagValues = a} :: DescribeTags) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number or response records to return in each call. If the
-- number of remaining response records exceeds the specified @MaxRecords@
-- value, a value is returned in a @marker@ field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned @marker@ value.
describeTags_maxRecords :: Lens.Lens' DescribeTags (Prelude.Maybe Prelude.Int)
describeTags_maxRecords = Lens.lens (\DescribeTags' {maxRecords} -> maxRecords) (\s@DescribeTags' {} a -> s {maxRecords = a} :: DescribeTags)

instance Core.AWSPager DescribeTags where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeTagsResponse_marker Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeTagsResponse_taggedResources
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeTags_marker
          Lens..~ rs
          Lens.^? describeTagsResponse_marker Prelude.. Lens._Just

instance Core.AWSRequest DescribeTags where
  type AWSResponse DescribeTags = DescribeTagsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeTagsResult"
      ( \s h x ->
          DescribeTagsResponse'
            Prelude.<$> (x Core..@? "Marker")
            Prelude.<*> ( x Core..@? "TaggedResources" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "TaggedResource")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeTags where
  hashWithSalt _salt DescribeTags' {..} =
    _salt `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` tagKeys
      `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` resourceName
      `Prelude.hashWithSalt` tagValues
      `Prelude.hashWithSalt` maxRecords

instance Prelude.NFData DescribeTags where
  rnf DescribeTags' {..} =
    Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf tagKeys
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf resourceName
      `Prelude.seq` Prelude.rnf tagValues
      `Prelude.seq` Prelude.rnf maxRecords

instance Core.ToHeaders DescribeTags where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeTags where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeTags where
  toQuery DescribeTags' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DescribeTags" :: Prelude.ByteString),
        "Version"
          Core.=: ("2012-12-01" :: Prelude.ByteString),
        "ResourceType" Core.=: resourceType,
        "TagKeys"
          Core.=: Core.toQuery
            (Core.toQueryList "TagKey" Prelude.<$> tagKeys),
        "Marker" Core.=: marker,
        "ResourceName" Core.=: resourceName,
        "TagValues"
          Core.=: Core.toQuery
            (Core.toQueryList "TagValue" Prelude.<$> tagValues),
        "MaxRecords" Core.=: maxRecords
      ]

-- |
--
-- /See:/ 'newDescribeTagsResponse' smart constructor.
data DescribeTagsResponse = DescribeTagsResponse'
  { -- | A value that indicates the starting point for the next set of response
    -- records in a subsequent request. If a value is returned in a response,
    -- you can retrieve the next set of records by providing this returned
    -- marker value in the @Marker@ parameter and retrying the command. If the
    -- @Marker@ field is empty, all response records have been retrieved for
    -- the request.
    marker :: Prelude.Maybe Prelude.Text,
    -- | A list of tags with their associated resources.
    taggedResources :: Prelude.Maybe [TaggedResource],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTagsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'describeTagsResponse_marker' - A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the @Marker@ parameter and retrying the command. If the
-- @Marker@ field is empty, all response records have been retrieved for
-- the request.
--
-- 'taggedResources', 'describeTagsResponse_taggedResources' - A list of tags with their associated resources.
--
-- 'httpStatus', 'describeTagsResponse_httpStatus' - The response's http status code.
newDescribeTagsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeTagsResponse
newDescribeTagsResponse pHttpStatus_ =
  DescribeTagsResponse'
    { marker = Prelude.Nothing,
      taggedResources = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A value that indicates the starting point for the next set of response
-- records in a subsequent request. If a value is returned in a response,
-- you can retrieve the next set of records by providing this returned
-- marker value in the @Marker@ parameter and retrying the command. If the
-- @Marker@ field is empty, all response records have been retrieved for
-- the request.
describeTagsResponse_marker :: Lens.Lens' DescribeTagsResponse (Prelude.Maybe Prelude.Text)
describeTagsResponse_marker = Lens.lens (\DescribeTagsResponse' {marker} -> marker) (\s@DescribeTagsResponse' {} a -> s {marker = a} :: DescribeTagsResponse)

-- | A list of tags with their associated resources.
describeTagsResponse_taggedResources :: Lens.Lens' DescribeTagsResponse (Prelude.Maybe [TaggedResource])
describeTagsResponse_taggedResources = Lens.lens (\DescribeTagsResponse' {taggedResources} -> taggedResources) (\s@DescribeTagsResponse' {} a -> s {taggedResources = a} :: DescribeTagsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeTagsResponse_httpStatus :: Lens.Lens' DescribeTagsResponse Prelude.Int
describeTagsResponse_httpStatus = Lens.lens (\DescribeTagsResponse' {httpStatus} -> httpStatus) (\s@DescribeTagsResponse' {} a -> s {httpStatus = a} :: DescribeTagsResponse)

instance Prelude.NFData DescribeTagsResponse where
  rnf DescribeTagsResponse' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf taggedResources
      `Prelude.seq` Prelude.rnf httpStatus
