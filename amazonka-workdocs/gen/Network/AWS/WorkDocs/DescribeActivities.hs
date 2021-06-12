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
-- Module      : Network.AWS.WorkDocs.DescribeActivities
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the user activities in a specified time period.
--
-- This operation returns paginated results.
module Network.AWS.WorkDocs.DescribeActivities
  ( -- * Creating a Request
    DescribeActivities (..),
    newDescribeActivities,

    -- * Request Lenses
    describeActivities_resourceId,
    describeActivities_organizationId,
    describeActivities_startTime,
    describeActivities_includeIndirectActivities,
    describeActivities_endTime,
    describeActivities_userId,
    describeActivities_activityTypes,
    describeActivities_authenticationToken,
    describeActivities_limit,
    describeActivities_marker,

    -- * Destructuring the Response
    DescribeActivitiesResponse (..),
    newDescribeActivitiesResponse,

    -- * Response Lenses
    describeActivitiesResponse_userActivities,
    describeActivitiesResponse_marker,
    describeActivitiesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkDocs.Types

-- | /See:/ 'newDescribeActivities' smart constructor.
data DescribeActivities = DescribeActivities'
  { -- | The document or folder ID for which to describe activity types.
    resourceId :: Core.Maybe Core.Text,
    -- | The ID of the organization. This is a mandatory parameter when using
    -- administrative API (SigV4) requests.
    organizationId :: Core.Maybe Core.Text,
    -- | The timestamp that determines the starting time of the activities. The
    -- response includes the activities performed after the specified
    -- timestamp.
    startTime :: Core.Maybe Core.POSIX,
    -- | Includes indirect activities. An indirect activity results from a direct
    -- activity performed on a parent resource. For example, sharing a parent
    -- folder (the direct activity) shares all of the subfolders and documents
    -- within the parent folder (the indirect activity).
    includeIndirectActivities :: Core.Maybe Core.Bool,
    -- | The timestamp that determines the end time of the activities. The
    -- response includes the activities performed before the specified
    -- timestamp.
    endTime :: Core.Maybe Core.POSIX,
    -- | The ID of the user who performed the action. The response includes
    -- activities pertaining to this user. This is an optional parameter and is
    -- only applicable for administrative API (SigV4) requests.
    userId :: Core.Maybe Core.Text,
    -- | Specifies which activity types to include in the response. If this field
    -- is left empty, all activity types are returned.
    activityTypes :: Core.Maybe Core.Text,
    -- | Amazon WorkDocs authentication token. Not required when using AWS
    -- administrator credentials to access the API.
    authenticationToken :: Core.Maybe (Core.Sensitive Core.Text),
    -- | The maximum number of items to return.
    limit :: Core.Maybe Core.Natural,
    -- | The marker for the next set of results.
    marker :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeActivities' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceId', 'describeActivities_resourceId' - The document or folder ID for which to describe activity types.
--
-- 'organizationId', 'describeActivities_organizationId' - The ID of the organization. This is a mandatory parameter when using
-- administrative API (SigV4) requests.
--
-- 'startTime', 'describeActivities_startTime' - The timestamp that determines the starting time of the activities. The
-- response includes the activities performed after the specified
-- timestamp.
--
-- 'includeIndirectActivities', 'describeActivities_includeIndirectActivities' - Includes indirect activities. An indirect activity results from a direct
-- activity performed on a parent resource. For example, sharing a parent
-- folder (the direct activity) shares all of the subfolders and documents
-- within the parent folder (the indirect activity).
--
-- 'endTime', 'describeActivities_endTime' - The timestamp that determines the end time of the activities. The
-- response includes the activities performed before the specified
-- timestamp.
--
-- 'userId', 'describeActivities_userId' - The ID of the user who performed the action. The response includes
-- activities pertaining to this user. This is an optional parameter and is
-- only applicable for administrative API (SigV4) requests.
--
-- 'activityTypes', 'describeActivities_activityTypes' - Specifies which activity types to include in the response. If this field
-- is left empty, all activity types are returned.
--
-- 'authenticationToken', 'describeActivities_authenticationToken' - Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
--
-- 'limit', 'describeActivities_limit' - The maximum number of items to return.
--
-- 'marker', 'describeActivities_marker' - The marker for the next set of results.
newDescribeActivities ::
  DescribeActivities
newDescribeActivities =
  DescribeActivities'
    { resourceId = Core.Nothing,
      organizationId = Core.Nothing,
      startTime = Core.Nothing,
      includeIndirectActivities = Core.Nothing,
      endTime = Core.Nothing,
      userId = Core.Nothing,
      activityTypes = Core.Nothing,
      authenticationToken = Core.Nothing,
      limit = Core.Nothing,
      marker = Core.Nothing
    }

-- | The document or folder ID for which to describe activity types.
describeActivities_resourceId :: Lens.Lens' DescribeActivities (Core.Maybe Core.Text)
describeActivities_resourceId = Lens.lens (\DescribeActivities' {resourceId} -> resourceId) (\s@DescribeActivities' {} a -> s {resourceId = a} :: DescribeActivities)

-- | The ID of the organization. This is a mandatory parameter when using
-- administrative API (SigV4) requests.
describeActivities_organizationId :: Lens.Lens' DescribeActivities (Core.Maybe Core.Text)
describeActivities_organizationId = Lens.lens (\DescribeActivities' {organizationId} -> organizationId) (\s@DescribeActivities' {} a -> s {organizationId = a} :: DescribeActivities)

-- | The timestamp that determines the starting time of the activities. The
-- response includes the activities performed after the specified
-- timestamp.
describeActivities_startTime :: Lens.Lens' DescribeActivities (Core.Maybe Core.UTCTime)
describeActivities_startTime = Lens.lens (\DescribeActivities' {startTime} -> startTime) (\s@DescribeActivities' {} a -> s {startTime = a} :: DescribeActivities) Core.. Lens.mapping Core._Time

-- | Includes indirect activities. An indirect activity results from a direct
-- activity performed on a parent resource. For example, sharing a parent
-- folder (the direct activity) shares all of the subfolders and documents
-- within the parent folder (the indirect activity).
describeActivities_includeIndirectActivities :: Lens.Lens' DescribeActivities (Core.Maybe Core.Bool)
describeActivities_includeIndirectActivities = Lens.lens (\DescribeActivities' {includeIndirectActivities} -> includeIndirectActivities) (\s@DescribeActivities' {} a -> s {includeIndirectActivities = a} :: DescribeActivities)

-- | The timestamp that determines the end time of the activities. The
-- response includes the activities performed before the specified
-- timestamp.
describeActivities_endTime :: Lens.Lens' DescribeActivities (Core.Maybe Core.UTCTime)
describeActivities_endTime = Lens.lens (\DescribeActivities' {endTime} -> endTime) (\s@DescribeActivities' {} a -> s {endTime = a} :: DescribeActivities) Core.. Lens.mapping Core._Time

-- | The ID of the user who performed the action. The response includes
-- activities pertaining to this user. This is an optional parameter and is
-- only applicable for administrative API (SigV4) requests.
describeActivities_userId :: Lens.Lens' DescribeActivities (Core.Maybe Core.Text)
describeActivities_userId = Lens.lens (\DescribeActivities' {userId} -> userId) (\s@DescribeActivities' {} a -> s {userId = a} :: DescribeActivities)

-- | Specifies which activity types to include in the response. If this field
-- is left empty, all activity types are returned.
describeActivities_activityTypes :: Lens.Lens' DescribeActivities (Core.Maybe Core.Text)
describeActivities_activityTypes = Lens.lens (\DescribeActivities' {activityTypes} -> activityTypes) (\s@DescribeActivities' {} a -> s {activityTypes = a} :: DescribeActivities)

-- | Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
describeActivities_authenticationToken :: Lens.Lens' DescribeActivities (Core.Maybe Core.Text)
describeActivities_authenticationToken = Lens.lens (\DescribeActivities' {authenticationToken} -> authenticationToken) (\s@DescribeActivities' {} a -> s {authenticationToken = a} :: DescribeActivities) Core.. Lens.mapping Core._Sensitive

-- | The maximum number of items to return.
describeActivities_limit :: Lens.Lens' DescribeActivities (Core.Maybe Core.Natural)
describeActivities_limit = Lens.lens (\DescribeActivities' {limit} -> limit) (\s@DescribeActivities' {} a -> s {limit = a} :: DescribeActivities)

-- | The marker for the next set of results.
describeActivities_marker :: Lens.Lens' DescribeActivities (Core.Maybe Core.Text)
describeActivities_marker = Lens.lens (\DescribeActivities' {marker} -> marker) (\s@DescribeActivities' {} a -> s {marker = a} :: DescribeActivities)

instance Core.AWSPager DescribeActivities where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeActivitiesResponse_marker Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeActivitiesResponse_userActivities
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeActivities_marker
          Lens..~ rs
          Lens.^? describeActivitiesResponse_marker Core.. Lens._Just

instance Core.AWSRequest DescribeActivities where
  type
    AWSResponse DescribeActivities =
      DescribeActivitiesResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeActivitiesResponse'
            Core.<$> (x Core..?> "UserActivities" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeActivities

instance Core.NFData DescribeActivities

instance Core.ToHeaders DescribeActivities where
  toHeaders DescribeActivities' {..} =
    Core.mconcat
      [ "Authentication" Core.=# authenticationToken,
        "Content-Type"
          Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
      ]

instance Core.ToPath DescribeActivities where
  toPath = Core.const "/api/v1/activities"

instance Core.ToQuery DescribeActivities where
  toQuery DescribeActivities' {..} =
    Core.mconcat
      [ "resourceId" Core.=: resourceId,
        "organizationId" Core.=: organizationId,
        "startTime" Core.=: startTime,
        "includeIndirectActivities"
          Core.=: includeIndirectActivities,
        "endTime" Core.=: endTime,
        "userId" Core.=: userId,
        "activityTypes" Core.=: activityTypes,
        "limit" Core.=: limit,
        "marker" Core.=: marker
      ]

-- | /See:/ 'newDescribeActivitiesResponse' smart constructor.
data DescribeActivitiesResponse = DescribeActivitiesResponse'
  { -- | The list of activities for the specified user and time period.
    userActivities :: Core.Maybe [Activity],
    -- | The marker for the next set of results.
    marker :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeActivitiesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userActivities', 'describeActivitiesResponse_userActivities' - The list of activities for the specified user and time period.
--
-- 'marker', 'describeActivitiesResponse_marker' - The marker for the next set of results.
--
-- 'httpStatus', 'describeActivitiesResponse_httpStatus' - The response's http status code.
newDescribeActivitiesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeActivitiesResponse
newDescribeActivitiesResponse pHttpStatus_ =
  DescribeActivitiesResponse'
    { userActivities =
        Core.Nothing,
      marker = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of activities for the specified user and time period.
describeActivitiesResponse_userActivities :: Lens.Lens' DescribeActivitiesResponse (Core.Maybe [Activity])
describeActivitiesResponse_userActivities = Lens.lens (\DescribeActivitiesResponse' {userActivities} -> userActivities) (\s@DescribeActivitiesResponse' {} a -> s {userActivities = a} :: DescribeActivitiesResponse) Core.. Lens.mapping Lens._Coerce

-- | The marker for the next set of results.
describeActivitiesResponse_marker :: Lens.Lens' DescribeActivitiesResponse (Core.Maybe Core.Text)
describeActivitiesResponse_marker = Lens.lens (\DescribeActivitiesResponse' {marker} -> marker) (\s@DescribeActivitiesResponse' {} a -> s {marker = a} :: DescribeActivitiesResponse)

-- | The response's http status code.
describeActivitiesResponse_httpStatus :: Lens.Lens' DescribeActivitiesResponse Core.Int
describeActivitiesResponse_httpStatus = Lens.lens (\DescribeActivitiesResponse' {httpStatus} -> httpStatus) (\s@DescribeActivitiesResponse' {} a -> s {httpStatus = a} :: DescribeActivitiesResponse)

instance Core.NFData DescribeActivitiesResponse
