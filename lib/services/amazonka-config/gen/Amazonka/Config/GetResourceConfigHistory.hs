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
-- Module      : Amazonka.Config.GetResourceConfigHistory
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of @ConfigurationItems@ for the specified resource. The
-- list contains details about each state of the resource during the
-- specified time interval. If you specified a retention period to retain
-- your @ConfigurationItems@ between a minimum of 30 days and a maximum of
-- 7 years (2557 days), Config returns the @ConfigurationItems@ for the
-- specified retention period.
--
-- The response is paginated. By default, Config returns a limit of 10
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
module Amazonka.Config.GetResourceConfigHistory
  ( -- * Creating a Request
    GetResourceConfigHistory (..),
    newGetResourceConfigHistory,

    -- * Request Lenses
    getResourceConfigHistory_nextToken,
    getResourceConfigHistory_earlierTime,
    getResourceConfigHistory_limit,
    getResourceConfigHistory_laterTime,
    getResourceConfigHistory_chronologicalOrder,
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

import Amazonka.Config.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The input for the GetResourceConfigHistory action.
--
-- /See:/ 'newGetResourceConfigHistory' smart constructor.
data GetResourceConfigHistory = GetResourceConfigHistory'
  { -- | The @nextToken@ string returned on a previous page that you use to get
    -- the next page of results in a paginated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The time stamp that indicates an earlier time. If not specified, the
    -- action returns paginated results that contain configuration items that
    -- start when the first configuration item was recorded.
    earlierTime :: Prelude.Maybe Data.POSIX,
    -- | The maximum number of configuration items returned on each page. The
    -- default is 10. You cannot specify a number greater than 100. If you
    -- specify 0, Config uses the default.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | The time stamp that indicates a later time. If not specified, current
    -- time is taken.
    laterTime :: Prelude.Maybe Data.POSIX,
    -- | The chronological order for configuration items listed. By default, the
    -- results are listed in reverse chronological order.
    chronologicalOrder :: Prelude.Maybe ChronologicalOrder,
    -- | The resource type.
    resourceType :: ResourceType,
    -- | The ID of the resource (for example., @sg-xxxxxx@).
    resourceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'limit', 'getResourceConfigHistory_limit' - The maximum number of configuration items returned on each page. The
-- default is 10. You cannot specify a number greater than 100. If you
-- specify 0, Config uses the default.
--
-- 'laterTime', 'getResourceConfigHistory_laterTime' - The time stamp that indicates a later time. If not specified, current
-- time is taken.
--
-- 'chronologicalOrder', 'getResourceConfigHistory_chronologicalOrder' - The chronological order for configuration items listed. By default, the
-- results are listed in reverse chronological order.
--
-- 'resourceType', 'getResourceConfigHistory_resourceType' - The resource type.
--
-- 'resourceId', 'getResourceConfigHistory_resourceId' - The ID of the resource (for example., @sg-xxxxxx@).
newGetResourceConfigHistory ::
  -- | 'resourceType'
  ResourceType ->
  -- | 'resourceId'
  Prelude.Text ->
  GetResourceConfigHistory
newGetResourceConfigHistory
  pResourceType_
  pResourceId_ =
    GetResourceConfigHistory'
      { nextToken =
          Prelude.Nothing,
        earlierTime = Prelude.Nothing,
        limit = Prelude.Nothing,
        laterTime = Prelude.Nothing,
        chronologicalOrder = Prelude.Nothing,
        resourceType = pResourceType_,
        resourceId = pResourceId_
      }

-- | The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
getResourceConfigHistory_nextToken :: Lens.Lens' GetResourceConfigHistory (Prelude.Maybe Prelude.Text)
getResourceConfigHistory_nextToken = Lens.lens (\GetResourceConfigHistory' {nextToken} -> nextToken) (\s@GetResourceConfigHistory' {} a -> s {nextToken = a} :: GetResourceConfigHistory)

-- | The time stamp that indicates an earlier time. If not specified, the
-- action returns paginated results that contain configuration items that
-- start when the first configuration item was recorded.
getResourceConfigHistory_earlierTime :: Lens.Lens' GetResourceConfigHistory (Prelude.Maybe Prelude.UTCTime)
getResourceConfigHistory_earlierTime = Lens.lens (\GetResourceConfigHistory' {earlierTime} -> earlierTime) (\s@GetResourceConfigHistory' {} a -> s {earlierTime = a} :: GetResourceConfigHistory) Prelude.. Lens.mapping Data._Time

-- | The maximum number of configuration items returned on each page. The
-- default is 10. You cannot specify a number greater than 100. If you
-- specify 0, Config uses the default.
getResourceConfigHistory_limit :: Lens.Lens' GetResourceConfigHistory (Prelude.Maybe Prelude.Natural)
getResourceConfigHistory_limit = Lens.lens (\GetResourceConfigHistory' {limit} -> limit) (\s@GetResourceConfigHistory' {} a -> s {limit = a} :: GetResourceConfigHistory)

-- | The time stamp that indicates a later time. If not specified, current
-- time is taken.
getResourceConfigHistory_laterTime :: Lens.Lens' GetResourceConfigHistory (Prelude.Maybe Prelude.UTCTime)
getResourceConfigHistory_laterTime = Lens.lens (\GetResourceConfigHistory' {laterTime} -> laterTime) (\s@GetResourceConfigHistory' {} a -> s {laterTime = a} :: GetResourceConfigHistory) Prelude.. Lens.mapping Data._Time

-- | The chronological order for configuration items listed. By default, the
-- results are listed in reverse chronological order.
getResourceConfigHistory_chronologicalOrder :: Lens.Lens' GetResourceConfigHistory (Prelude.Maybe ChronologicalOrder)
getResourceConfigHistory_chronologicalOrder = Lens.lens (\GetResourceConfigHistory' {chronologicalOrder} -> chronologicalOrder) (\s@GetResourceConfigHistory' {} a -> s {chronologicalOrder = a} :: GetResourceConfigHistory)

-- | The resource type.
getResourceConfigHistory_resourceType :: Lens.Lens' GetResourceConfigHistory ResourceType
getResourceConfigHistory_resourceType = Lens.lens (\GetResourceConfigHistory' {resourceType} -> resourceType) (\s@GetResourceConfigHistory' {} a -> s {resourceType = a} :: GetResourceConfigHistory)

-- | The ID of the resource (for example., @sg-xxxxxx@).
getResourceConfigHistory_resourceId :: Lens.Lens' GetResourceConfigHistory Prelude.Text
getResourceConfigHistory_resourceId = Lens.lens (\GetResourceConfigHistory' {resourceId} -> resourceId) (\s@GetResourceConfigHistory' {} a -> s {resourceId = a} :: GetResourceConfigHistory)

instance Core.AWSPager GetResourceConfigHistory where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getResourceConfigHistoryResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getResourceConfigHistoryResponse_configurationItems
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getResourceConfigHistory_nextToken
          Lens..~ rs
          Lens.^? getResourceConfigHistoryResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest GetResourceConfigHistory where
  type
    AWSResponse GetResourceConfigHistory =
      GetResourceConfigHistoryResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetResourceConfigHistoryResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> ( x Data..?> "configurationItems"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetResourceConfigHistory where
  hashWithSalt _salt GetResourceConfigHistory' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` earlierTime
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` laterTime
      `Prelude.hashWithSalt` chronologicalOrder
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` resourceId

instance Prelude.NFData GetResourceConfigHistory where
  rnf GetResourceConfigHistory' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf earlierTime
      `Prelude.seq` Prelude.rnf limit
      `Prelude.seq` Prelude.rnf laterTime
      `Prelude.seq` Prelude.rnf chronologicalOrder
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf resourceId

instance Data.ToHeaders GetResourceConfigHistory where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StarlingDoveService.GetResourceConfigHistory" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetResourceConfigHistory where
  toJSON GetResourceConfigHistory' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("nextToken" Data..=) Prelude.<$> nextToken,
            ("earlierTime" Data..=) Prelude.<$> earlierTime,
            ("limit" Data..=) Prelude.<$> limit,
            ("laterTime" Data..=) Prelude.<$> laterTime,
            ("chronologicalOrder" Data..=)
              Prelude.<$> chronologicalOrder,
            Prelude.Just ("resourceType" Data..= resourceType),
            Prelude.Just ("resourceId" Data..= resourceId)
          ]
      )

instance Data.ToPath GetResourceConfigHistory where
  toPath = Prelude.const "/"

instance Data.ToQuery GetResourceConfigHistory where
  toQuery = Prelude.const Prelude.mempty

-- | The output for the GetResourceConfigHistory action.
--
-- /See:/ 'newGetResourceConfigHistoryResponse' smart constructor.
data GetResourceConfigHistoryResponse = GetResourceConfigHistoryResponse'
  { -- | The string that you use in a subsequent request to get the next page of
    -- results in a paginated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list that contains the configuration history of one or more resources.
    configurationItems :: Prelude.Maybe [ConfigurationItem],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  GetResourceConfigHistoryResponse
newGetResourceConfigHistoryResponse pHttpStatus_ =
  GetResourceConfigHistoryResponse'
    { nextToken =
        Prelude.Nothing,
      configurationItems = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The string that you use in a subsequent request to get the next page of
-- results in a paginated response.
getResourceConfigHistoryResponse_nextToken :: Lens.Lens' GetResourceConfigHistoryResponse (Prelude.Maybe Prelude.Text)
getResourceConfigHistoryResponse_nextToken = Lens.lens (\GetResourceConfigHistoryResponse' {nextToken} -> nextToken) (\s@GetResourceConfigHistoryResponse' {} a -> s {nextToken = a} :: GetResourceConfigHistoryResponse)

-- | A list that contains the configuration history of one or more resources.
getResourceConfigHistoryResponse_configurationItems :: Lens.Lens' GetResourceConfigHistoryResponse (Prelude.Maybe [ConfigurationItem])
getResourceConfigHistoryResponse_configurationItems = Lens.lens (\GetResourceConfigHistoryResponse' {configurationItems} -> configurationItems) (\s@GetResourceConfigHistoryResponse' {} a -> s {configurationItems = a} :: GetResourceConfigHistoryResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getResourceConfigHistoryResponse_httpStatus :: Lens.Lens' GetResourceConfigHistoryResponse Prelude.Int
getResourceConfigHistoryResponse_httpStatus = Lens.lens (\GetResourceConfigHistoryResponse' {httpStatus} -> httpStatus) (\s@GetResourceConfigHistoryResponse' {} a -> s {httpStatus = a} :: GetResourceConfigHistoryResponse)

instance
  Prelude.NFData
    GetResourceConfigHistoryResponse
  where
  rnf GetResourceConfigHistoryResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf configurationItems
      `Prelude.seq` Prelude.rnf httpStatus
