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
-- Module      : Network.AWS.Config.GetResourceConfigHistory
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of configuration items for the specified resource. The
-- list contains details about each state of the resource during the
-- specified time interval. If you specified a retention period to retain
-- your @ConfigurationItems@ between a minimum of 30 days and a maximum of
-- 7 years (2557 days), AWS Config returns the @ConfigurationItems@ for the
-- specified retention period.
--
-- The response is paginated. By default, AWS Config returns a limit of 10
-- configuration items per page. You can customize this number with the
-- @limit@ parameter. The response includes a @nextToken@ string. To get
-- the next page of results, run the request again and specify the string
-- for the @nextToken@ parameter.
--
-- Each call to the API is limited to span a duration of seven days. It is
-- likely that the number of records returned is smaller than the specified
-- @limit@. In such cases, you can make another call, using the
-- @nextToken@.
--
-- This operation returns paginated results.
module Network.AWS.Config.GetResourceConfigHistory
  ( -- * Creating a Request
    GetResourceConfigHistory (..),
    newGetResourceConfigHistory,

    -- * Request Lenses
    getResourceConfigHistory_nextToken,
    getResourceConfigHistory_earlierTime,
    getResourceConfigHistory_laterTime,
    getResourceConfigHistory_chronologicalOrder,
    getResourceConfigHistory_limit,
    getResourceConfigHistory_resourceType,
    getResourceConfigHistory_resourceId,

    -- * Destructuring the Response
    GetResourceConfigHistoryResponse (..),
    newGetResourceConfigHistoryResponse,

    -- * Response Lenses
    getResourceConfigHistoryResponse_nextToken,
    getResourceConfigHistoryResponse_configurationItems,
    getResourceConfigHistoryResponse_httpStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the GetResourceConfigHistory action.
--
-- /See:/ 'newGetResourceConfigHistory' smart constructor.
data GetResourceConfigHistory = GetResourceConfigHistory'
  { -- | The @nextToken@ string returned on a previous page that you use to get
    -- the next page of results in a paginated response.
    nextToken :: Core.Maybe Core.Text,
    -- | The time stamp that indicates an earlier time. If not specified, the
    -- action returns paginated results that contain configuration items that
    -- start when the first configuration item was recorded.
    earlierTime :: Core.Maybe Core.POSIX,
    -- | The time stamp that indicates a later time. If not specified, current
    -- time is taken.
    laterTime :: Core.Maybe Core.POSIX,
    -- | The chronological order for configuration items listed. By default, the
    -- results are listed in reverse chronological order.
    chronologicalOrder :: Core.Maybe ChronologicalOrder,
    -- | The maximum number of configuration items returned on each page. The
    -- default is 10. You cannot specify a number greater than 100. If you
    -- specify 0, AWS Config uses the default.
    limit :: Core.Maybe Core.Natural,
    -- | The resource type.
    resourceType :: ResourceType,
    -- | The ID of the resource (for example., @sg-xxxxxx@).
    resourceId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetResourceConfigHistory' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getResourceConfigHistory_nextToken' - The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
--
-- 'earlierTime', 'getResourceConfigHistory_earlierTime' - The time stamp that indicates an earlier time. If not specified, the
-- action returns paginated results that contain configuration items that
-- start when the first configuration item was recorded.
--
-- 'laterTime', 'getResourceConfigHistory_laterTime' - The time stamp that indicates a later time. If not specified, current
-- time is taken.
--
-- 'chronologicalOrder', 'getResourceConfigHistory_chronologicalOrder' - The chronological order for configuration items listed. By default, the
-- results are listed in reverse chronological order.
--
-- 'limit', 'getResourceConfigHistory_limit' - The maximum number of configuration items returned on each page. The
-- default is 10. You cannot specify a number greater than 100. If you
-- specify 0, AWS Config uses the default.
--
-- 'resourceType', 'getResourceConfigHistory_resourceType' - The resource type.
--
-- 'resourceId', 'getResourceConfigHistory_resourceId' - The ID of the resource (for example., @sg-xxxxxx@).
newGetResourceConfigHistory ::
  -- | 'resourceType'
  ResourceType ->
  -- | 'resourceId'
  Core.Text ->
  GetResourceConfigHistory
newGetResourceConfigHistory
  pResourceType_
  pResourceId_ =
    GetResourceConfigHistory'
      { nextToken = Core.Nothing,
        earlierTime = Core.Nothing,
        laterTime = Core.Nothing,
        chronologicalOrder = Core.Nothing,
        limit = Core.Nothing,
        resourceType = pResourceType_,
        resourceId = pResourceId_
      }

-- | The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
getResourceConfigHistory_nextToken :: Lens.Lens' GetResourceConfigHistory (Core.Maybe Core.Text)
getResourceConfigHistory_nextToken = Lens.lens (\GetResourceConfigHistory' {nextToken} -> nextToken) (\s@GetResourceConfigHistory' {} a -> s {nextToken = a} :: GetResourceConfigHistory)

-- | The time stamp that indicates an earlier time. If not specified, the
-- action returns paginated results that contain configuration items that
-- start when the first configuration item was recorded.
getResourceConfigHistory_earlierTime :: Lens.Lens' GetResourceConfigHistory (Core.Maybe Core.UTCTime)
getResourceConfigHistory_earlierTime = Lens.lens (\GetResourceConfigHistory' {earlierTime} -> earlierTime) (\s@GetResourceConfigHistory' {} a -> s {earlierTime = a} :: GetResourceConfigHistory) Core.. Lens.mapping Core._Time

-- | The time stamp that indicates a later time. If not specified, current
-- time is taken.
getResourceConfigHistory_laterTime :: Lens.Lens' GetResourceConfigHistory (Core.Maybe Core.UTCTime)
getResourceConfigHistory_laterTime = Lens.lens (\GetResourceConfigHistory' {laterTime} -> laterTime) (\s@GetResourceConfigHistory' {} a -> s {laterTime = a} :: GetResourceConfigHistory) Core.. Lens.mapping Core._Time

-- | The chronological order for configuration items listed. By default, the
-- results are listed in reverse chronological order.
getResourceConfigHistory_chronologicalOrder :: Lens.Lens' GetResourceConfigHistory (Core.Maybe ChronologicalOrder)
getResourceConfigHistory_chronologicalOrder = Lens.lens (\GetResourceConfigHistory' {chronologicalOrder} -> chronologicalOrder) (\s@GetResourceConfigHistory' {} a -> s {chronologicalOrder = a} :: GetResourceConfigHistory)

-- | The maximum number of configuration items returned on each page. The
-- default is 10. You cannot specify a number greater than 100. If you
-- specify 0, AWS Config uses the default.
getResourceConfigHistory_limit :: Lens.Lens' GetResourceConfigHistory (Core.Maybe Core.Natural)
getResourceConfigHistory_limit = Lens.lens (\GetResourceConfigHistory' {limit} -> limit) (\s@GetResourceConfigHistory' {} a -> s {limit = a} :: GetResourceConfigHistory)

-- | The resource type.
getResourceConfigHistory_resourceType :: Lens.Lens' GetResourceConfigHistory ResourceType
getResourceConfigHistory_resourceType = Lens.lens (\GetResourceConfigHistory' {resourceType} -> resourceType) (\s@GetResourceConfigHistory' {} a -> s {resourceType = a} :: GetResourceConfigHistory)

-- | The ID of the resource (for example., @sg-xxxxxx@).
getResourceConfigHistory_resourceId :: Lens.Lens' GetResourceConfigHistory Core.Text
getResourceConfigHistory_resourceId = Lens.lens (\GetResourceConfigHistory' {resourceId} -> resourceId) (\s@GetResourceConfigHistory' {} a -> s {resourceId = a} :: GetResourceConfigHistory)

instance Core.AWSPager GetResourceConfigHistory where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getResourceConfigHistoryResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? getResourceConfigHistoryResponse_configurationItems
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& getResourceConfigHistory_nextToken
          Lens..~ rs
          Lens.^? getResourceConfigHistoryResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest GetResourceConfigHistory where
  type
    AWSResponse GetResourceConfigHistory =
      GetResourceConfigHistoryResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetResourceConfigHistoryResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> ( x Core..?> "configurationItems"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetResourceConfigHistory

instance Core.NFData GetResourceConfigHistory

instance Core.ToHeaders GetResourceConfigHistory where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StarlingDoveService.GetResourceConfigHistory" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetResourceConfigHistory where
  toJSON GetResourceConfigHistory' {..} =
    Core.object
      ( Core.catMaybes
          [ ("nextToken" Core..=) Core.<$> nextToken,
            ("earlierTime" Core..=) Core.<$> earlierTime,
            ("laterTime" Core..=) Core.<$> laterTime,
            ("chronologicalOrder" Core..=)
              Core.<$> chronologicalOrder,
            ("limit" Core..=) Core.<$> limit,
            Core.Just ("resourceType" Core..= resourceType),
            Core.Just ("resourceId" Core..= resourceId)
          ]
      )

instance Core.ToPath GetResourceConfigHistory where
  toPath = Core.const "/"

instance Core.ToQuery GetResourceConfigHistory where
  toQuery = Core.const Core.mempty

-- | The output for the GetResourceConfigHistory action.
--
-- /See:/ 'newGetResourceConfigHistoryResponse' smart constructor.
data GetResourceConfigHistoryResponse = GetResourceConfigHistoryResponse'
  { -- | The string that you use in a subsequent request to get the next page of
    -- results in a paginated response.
    nextToken :: Core.Maybe Core.Text,
    -- | A list that contains the configuration history of one or more resources.
    configurationItems :: Core.Maybe [ConfigurationItem],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetResourceConfigHistoryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getResourceConfigHistoryResponse_nextToken' - The string that you use in a subsequent request to get the next page of
-- results in a paginated response.
--
-- 'configurationItems', 'getResourceConfigHistoryResponse_configurationItems' - A list that contains the configuration history of one or more resources.
--
-- 'httpStatus', 'getResourceConfigHistoryResponse_httpStatus' - The response's http status code.
newGetResourceConfigHistoryResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetResourceConfigHistoryResponse
newGetResourceConfigHistoryResponse pHttpStatus_ =
  GetResourceConfigHistoryResponse'
    { nextToken =
        Core.Nothing,
      configurationItems = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The string that you use in a subsequent request to get the next page of
-- results in a paginated response.
getResourceConfigHistoryResponse_nextToken :: Lens.Lens' GetResourceConfigHistoryResponse (Core.Maybe Core.Text)
getResourceConfigHistoryResponse_nextToken = Lens.lens (\GetResourceConfigHistoryResponse' {nextToken} -> nextToken) (\s@GetResourceConfigHistoryResponse' {} a -> s {nextToken = a} :: GetResourceConfigHistoryResponse)

-- | A list that contains the configuration history of one or more resources.
getResourceConfigHistoryResponse_configurationItems :: Lens.Lens' GetResourceConfigHistoryResponse (Core.Maybe [ConfigurationItem])
getResourceConfigHistoryResponse_configurationItems = Lens.lens (\GetResourceConfigHistoryResponse' {configurationItems} -> configurationItems) (\s@GetResourceConfigHistoryResponse' {} a -> s {configurationItems = a} :: GetResourceConfigHistoryResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getResourceConfigHistoryResponse_httpStatus :: Lens.Lens' GetResourceConfigHistoryResponse Core.Int
getResourceConfigHistoryResponse_httpStatus = Lens.lens (\GetResourceConfigHistoryResponse' {httpStatus} -> httpStatus) (\s@GetResourceConfigHistoryResponse' {} a -> s {httpStatus = a} :: GetResourceConfigHistoryResponse)

instance Core.NFData GetResourceConfigHistoryResponse
