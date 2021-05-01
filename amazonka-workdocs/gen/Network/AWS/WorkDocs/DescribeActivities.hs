{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkDocs.Types

-- | /See:/ 'newDescribeActivities' smart constructor.
data DescribeActivities = DescribeActivities'
  { -- | The document or folder ID for which to describe activity types.
    resourceId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the organization. This is a mandatory parameter when using
    -- administrative API (SigV4) requests.
    organizationId :: Prelude.Maybe Prelude.Text,
    -- | The timestamp that determines the starting time of the activities. The
    -- response includes the activities performed after the specified
    -- timestamp.
    startTime :: Prelude.Maybe Prelude.POSIX,
    -- | Includes indirect activities. An indirect activity results from a direct
    -- activity performed on a parent resource. For example, sharing a parent
    -- folder (the direct activity) shares all of the subfolders and documents
    -- within the parent folder (the indirect activity).
    includeIndirectActivities :: Prelude.Maybe Prelude.Bool,
    -- | The timestamp that determines the end time of the activities. The
    -- response includes the activities performed before the specified
    -- timestamp.
    endTime :: Prelude.Maybe Prelude.POSIX,
    -- | The ID of the user who performed the action. The response includes
    -- activities pertaining to this user. This is an optional parameter and is
    -- only applicable for administrative API (SigV4) requests.
    userId :: Prelude.Maybe Prelude.Text,
    -- | Specifies which activity types to include in the response. If this field
    -- is left empty, all activity types are returned.
    activityTypes :: Prelude.Maybe Prelude.Text,
    -- | Amazon WorkDocs authentication token. Not required when using AWS
    -- administrator credentials to access the API.
    authenticationToken :: Prelude.Maybe (Prelude.Sensitive Prelude.Text),
    -- | The maximum number of items to return.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | The marker for the next set of results.
    marker :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { resourceId = Prelude.Nothing,
      organizationId = Prelude.Nothing,
      startTime = Prelude.Nothing,
      includeIndirectActivities = Prelude.Nothing,
      endTime = Prelude.Nothing,
      userId = Prelude.Nothing,
      activityTypes = Prelude.Nothing,
      authenticationToken = Prelude.Nothing,
      limit = Prelude.Nothing,
      marker = Prelude.Nothing
    }

-- | The document or folder ID for which to describe activity types.
describeActivities_resourceId :: Lens.Lens' DescribeActivities (Prelude.Maybe Prelude.Text)
describeActivities_resourceId = Lens.lens (\DescribeActivities' {resourceId} -> resourceId) (\s@DescribeActivities' {} a -> s {resourceId = a} :: DescribeActivities)

-- | The ID of the organization. This is a mandatory parameter when using
-- administrative API (SigV4) requests.
describeActivities_organizationId :: Lens.Lens' DescribeActivities (Prelude.Maybe Prelude.Text)
describeActivities_organizationId = Lens.lens (\DescribeActivities' {organizationId} -> organizationId) (\s@DescribeActivities' {} a -> s {organizationId = a} :: DescribeActivities)

-- | The timestamp that determines the starting time of the activities. The
-- response includes the activities performed after the specified
-- timestamp.
describeActivities_startTime :: Lens.Lens' DescribeActivities (Prelude.Maybe Prelude.UTCTime)
describeActivities_startTime = Lens.lens (\DescribeActivities' {startTime} -> startTime) (\s@DescribeActivities' {} a -> s {startTime = a} :: DescribeActivities) Prelude.. Lens.mapping Prelude._Time

-- | Includes indirect activities. An indirect activity results from a direct
-- activity performed on a parent resource. For example, sharing a parent
-- folder (the direct activity) shares all of the subfolders and documents
-- within the parent folder (the indirect activity).
describeActivities_includeIndirectActivities :: Lens.Lens' DescribeActivities (Prelude.Maybe Prelude.Bool)
describeActivities_includeIndirectActivities = Lens.lens (\DescribeActivities' {includeIndirectActivities} -> includeIndirectActivities) (\s@DescribeActivities' {} a -> s {includeIndirectActivities = a} :: DescribeActivities)

-- | The timestamp that determines the end time of the activities. The
-- response includes the activities performed before the specified
-- timestamp.
describeActivities_endTime :: Lens.Lens' DescribeActivities (Prelude.Maybe Prelude.UTCTime)
describeActivities_endTime = Lens.lens (\DescribeActivities' {endTime} -> endTime) (\s@DescribeActivities' {} a -> s {endTime = a} :: DescribeActivities) Prelude.. Lens.mapping Prelude._Time

-- | The ID of the user who performed the action. The response includes
-- activities pertaining to this user. This is an optional parameter and is
-- only applicable for administrative API (SigV4) requests.
describeActivities_userId :: Lens.Lens' DescribeActivities (Prelude.Maybe Prelude.Text)
describeActivities_userId = Lens.lens (\DescribeActivities' {userId} -> userId) (\s@DescribeActivities' {} a -> s {userId = a} :: DescribeActivities)

-- | Specifies which activity types to include in the response. If this field
-- is left empty, all activity types are returned.
describeActivities_activityTypes :: Lens.Lens' DescribeActivities (Prelude.Maybe Prelude.Text)
describeActivities_activityTypes = Lens.lens (\DescribeActivities' {activityTypes} -> activityTypes) (\s@DescribeActivities' {} a -> s {activityTypes = a} :: DescribeActivities)

-- | Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
describeActivities_authenticationToken :: Lens.Lens' DescribeActivities (Prelude.Maybe Prelude.Text)
describeActivities_authenticationToken = Lens.lens (\DescribeActivities' {authenticationToken} -> authenticationToken) (\s@DescribeActivities' {} a -> s {authenticationToken = a} :: DescribeActivities) Prelude.. Lens.mapping Prelude._Sensitive

-- | The maximum number of items to return.
describeActivities_limit :: Lens.Lens' DescribeActivities (Prelude.Maybe Prelude.Natural)
describeActivities_limit = Lens.lens (\DescribeActivities' {limit} -> limit) (\s@DescribeActivities' {} a -> s {limit = a} :: DescribeActivities)

-- | The marker for the next set of results.
describeActivities_marker :: Lens.Lens' DescribeActivities (Prelude.Maybe Prelude.Text)
describeActivities_marker = Lens.lens (\DescribeActivities' {marker} -> marker) (\s@DescribeActivities' {} a -> s {marker = a} :: DescribeActivities)

instance Pager.AWSPager DescribeActivities where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? describeActivitiesResponse_marker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? describeActivitiesResponse_userActivities
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& describeActivities_marker
          Lens..~ rs
          Lens.^? describeActivitiesResponse_marker
            Prelude.. Lens._Just

instance Prelude.AWSRequest DescribeActivities where
  type
    Rs DescribeActivities =
      DescribeActivitiesResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeActivitiesResponse'
            Prelude.<$> ( x Prelude..?> "UserActivities"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (x Prelude..?> "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeActivities

instance Prelude.NFData DescribeActivities

instance Prelude.ToHeaders DescribeActivities where
  toHeaders DescribeActivities' {..} =
    Prelude.mconcat
      [ "Authentication" Prelude.=# authenticationToken,
        "Content-Type"
          Prelude.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Prelude.ToPath DescribeActivities where
  toPath = Prelude.const "/api/v1/activities"

instance Prelude.ToQuery DescribeActivities where
  toQuery DescribeActivities' {..} =
    Prelude.mconcat
      [ "resourceId" Prelude.=: resourceId,
        "organizationId" Prelude.=: organizationId,
        "startTime" Prelude.=: startTime,
        "includeIndirectActivities"
          Prelude.=: includeIndirectActivities,
        "endTime" Prelude.=: endTime,
        "userId" Prelude.=: userId,
        "activityTypes" Prelude.=: activityTypes,
        "limit" Prelude.=: limit,
        "marker" Prelude.=: marker
      ]

-- | /See:/ 'newDescribeActivitiesResponse' smart constructor.
data DescribeActivitiesResponse = DescribeActivitiesResponse'
  { -- | The list of activities for the specified user and time period.
    userActivities :: Prelude.Maybe [Activity],
    -- | The marker for the next set of results.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DescribeActivitiesResponse
newDescribeActivitiesResponse pHttpStatus_ =
  DescribeActivitiesResponse'
    { userActivities =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of activities for the specified user and time period.
describeActivitiesResponse_userActivities :: Lens.Lens' DescribeActivitiesResponse (Prelude.Maybe [Activity])
describeActivitiesResponse_userActivities = Lens.lens (\DescribeActivitiesResponse' {userActivities} -> userActivities) (\s@DescribeActivitiesResponse' {} a -> s {userActivities = a} :: DescribeActivitiesResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The marker for the next set of results.
describeActivitiesResponse_marker :: Lens.Lens' DescribeActivitiesResponse (Prelude.Maybe Prelude.Text)
describeActivitiesResponse_marker = Lens.lens (\DescribeActivitiesResponse' {marker} -> marker) (\s@DescribeActivitiesResponse' {} a -> s {marker = a} :: DescribeActivitiesResponse)

-- | The response's http status code.
describeActivitiesResponse_httpStatus :: Lens.Lens' DescribeActivitiesResponse Prelude.Int
describeActivitiesResponse_httpStatus = Lens.lens (\DescribeActivitiesResponse' {httpStatus} -> httpStatus) (\s@DescribeActivitiesResponse' {} a -> s {httpStatus = a} :: DescribeActivitiesResponse)

instance Prelude.NFData DescribeActivitiesResponse
