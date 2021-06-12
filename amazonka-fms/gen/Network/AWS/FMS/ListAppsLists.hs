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
-- Module      : Network.AWS.FMS.ListAppsLists
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of @AppsListDataSummary@ objects.
module Network.AWS.FMS.ListAppsLists
  ( -- * Creating a Request
    ListAppsLists (..),
    newListAppsLists,

    -- * Request Lenses
    listAppsLists_nextToken,
    listAppsLists_defaultLists,
    listAppsLists_maxResults,

    -- * Destructuring the Response
    ListAppsListsResponse (..),
    newListAppsListsResponse,

    -- * Response Lenses
    listAppsListsResponse_nextToken,
    listAppsListsResponse_appsLists,
    listAppsListsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.FMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListAppsLists' smart constructor.
data ListAppsLists = ListAppsLists'
  { -- | If you specify a value for @MaxResults@ in your list request, and you
    -- have more objects than the maximum, AWS Firewall Manager returns this
    -- token in the response. For all but the first request, you provide the
    -- token returned by the prior request in the request parameters, to
    -- retrieve the next batch of objects.
    nextToken :: Core.Maybe Core.Text,
    -- | Specifies whether the lists to retrieve are default lists owned by AWS
    -- Firewall Manager.
    defaultLists :: Core.Maybe Core.Bool,
    -- | The maximum number of objects that you want AWS Firewall Manager to
    -- return for this request. If more objects are available, in the response,
    -- AWS Firewall Manager provides a @NextToken@ value that you can use in a
    -- subsequent call to get the next batch of objects.
    --
    -- If you don\'t specify this, AWS Firewall Manager returns all available
    -- objects.
    maxResults :: Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListAppsLists' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAppsLists_nextToken' - If you specify a value for @MaxResults@ in your list request, and you
-- have more objects than the maximum, AWS Firewall Manager returns this
-- token in the response. For all but the first request, you provide the
-- token returned by the prior request in the request parameters, to
-- retrieve the next batch of objects.
--
-- 'defaultLists', 'listAppsLists_defaultLists' - Specifies whether the lists to retrieve are default lists owned by AWS
-- Firewall Manager.
--
-- 'maxResults', 'listAppsLists_maxResults' - The maximum number of objects that you want AWS Firewall Manager to
-- return for this request. If more objects are available, in the response,
-- AWS Firewall Manager provides a @NextToken@ value that you can use in a
-- subsequent call to get the next batch of objects.
--
-- If you don\'t specify this, AWS Firewall Manager returns all available
-- objects.
newListAppsLists ::
  -- | 'maxResults'
  Core.Natural ->
  ListAppsLists
newListAppsLists pMaxResults_ =
  ListAppsLists'
    { nextToken = Core.Nothing,
      defaultLists = Core.Nothing,
      maxResults = pMaxResults_
    }

-- | If you specify a value for @MaxResults@ in your list request, and you
-- have more objects than the maximum, AWS Firewall Manager returns this
-- token in the response. For all but the first request, you provide the
-- token returned by the prior request in the request parameters, to
-- retrieve the next batch of objects.
listAppsLists_nextToken :: Lens.Lens' ListAppsLists (Core.Maybe Core.Text)
listAppsLists_nextToken = Lens.lens (\ListAppsLists' {nextToken} -> nextToken) (\s@ListAppsLists' {} a -> s {nextToken = a} :: ListAppsLists)

-- | Specifies whether the lists to retrieve are default lists owned by AWS
-- Firewall Manager.
listAppsLists_defaultLists :: Lens.Lens' ListAppsLists (Core.Maybe Core.Bool)
listAppsLists_defaultLists = Lens.lens (\ListAppsLists' {defaultLists} -> defaultLists) (\s@ListAppsLists' {} a -> s {defaultLists = a} :: ListAppsLists)

-- | The maximum number of objects that you want AWS Firewall Manager to
-- return for this request. If more objects are available, in the response,
-- AWS Firewall Manager provides a @NextToken@ value that you can use in a
-- subsequent call to get the next batch of objects.
--
-- If you don\'t specify this, AWS Firewall Manager returns all available
-- objects.
listAppsLists_maxResults :: Lens.Lens' ListAppsLists Core.Natural
listAppsLists_maxResults = Lens.lens (\ListAppsLists' {maxResults} -> maxResults) (\s@ListAppsLists' {} a -> s {maxResults = a} :: ListAppsLists)

instance Core.AWSRequest ListAppsLists where
  type
    AWSResponse ListAppsLists =
      ListAppsListsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAppsListsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "AppsLists" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListAppsLists

instance Core.NFData ListAppsLists

instance Core.ToHeaders ListAppsLists where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSFMS_20180101.ListAppsLists" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListAppsLists where
  toJSON ListAppsLists' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("DefaultLists" Core..=) Core.<$> defaultLists,
            Core.Just ("MaxResults" Core..= maxResults)
          ]
      )

instance Core.ToPath ListAppsLists where
  toPath = Core.const "/"

instance Core.ToQuery ListAppsLists where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListAppsListsResponse' smart constructor.
data ListAppsListsResponse = ListAppsListsResponse'
  { -- | If you specify a value for @MaxResults@ in your list request, and you
    -- have more objects than the maximum, AWS Firewall Manager returns this
    -- token in the response. You can use this token in subsequent requests to
    -- retrieve the next batch of objects.
    nextToken :: Core.Maybe Core.Text,
    -- | An array of @AppsListDataSummary@ objects.
    appsLists :: Core.Maybe [AppsListDataSummary],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListAppsListsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAppsListsResponse_nextToken' - If you specify a value for @MaxResults@ in your list request, and you
-- have more objects than the maximum, AWS Firewall Manager returns this
-- token in the response. You can use this token in subsequent requests to
-- retrieve the next batch of objects.
--
-- 'appsLists', 'listAppsListsResponse_appsLists' - An array of @AppsListDataSummary@ objects.
--
-- 'httpStatus', 'listAppsListsResponse_httpStatus' - The response's http status code.
newListAppsListsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListAppsListsResponse
newListAppsListsResponse pHttpStatus_ =
  ListAppsListsResponse'
    { nextToken = Core.Nothing,
      appsLists = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If you specify a value for @MaxResults@ in your list request, and you
-- have more objects than the maximum, AWS Firewall Manager returns this
-- token in the response. You can use this token in subsequent requests to
-- retrieve the next batch of objects.
listAppsListsResponse_nextToken :: Lens.Lens' ListAppsListsResponse (Core.Maybe Core.Text)
listAppsListsResponse_nextToken = Lens.lens (\ListAppsListsResponse' {nextToken} -> nextToken) (\s@ListAppsListsResponse' {} a -> s {nextToken = a} :: ListAppsListsResponse)

-- | An array of @AppsListDataSummary@ objects.
listAppsListsResponse_appsLists :: Lens.Lens' ListAppsListsResponse (Core.Maybe [AppsListDataSummary])
listAppsListsResponse_appsLists = Lens.lens (\ListAppsListsResponse' {appsLists} -> appsLists) (\s@ListAppsListsResponse' {} a -> s {appsLists = a} :: ListAppsListsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listAppsListsResponse_httpStatus :: Lens.Lens' ListAppsListsResponse Core.Int
listAppsListsResponse_httpStatus = Lens.lens (\ListAppsListsResponse' {httpStatus} -> httpStatus) (\s@ListAppsListsResponse' {} a -> s {httpStatus = a} :: ListAppsListsResponse)

instance Core.NFData ListAppsListsResponse
