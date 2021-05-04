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

import Network.AWS.FMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListAppsLists' smart constructor.
data ListAppsLists = ListAppsLists'
  { -- | If you specify a value for @MaxResults@ in your list request, and you
    -- have more objects than the maximum, AWS Firewall Manager returns this
    -- token in the response. For all but the first request, you provide the
    -- token returned by the prior request in the request parameters, to
    -- retrieve the next batch of objects.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the lists to retrieve are default lists owned by AWS
    -- Firewall Manager.
    defaultLists :: Prelude.Maybe Prelude.Bool,
    -- | The maximum number of objects that you want AWS Firewall Manager to
    -- return for this request. If more objects are available, in the response,
    -- AWS Firewall Manager provides a @NextToken@ value that you can use in a
    -- subsequent call to get the next batch of objects.
    --
    -- If you don\'t specify this, AWS Firewall Manager returns all available
    -- objects.
    maxResults :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Natural ->
  ListAppsLists
newListAppsLists pMaxResults_ =
  ListAppsLists'
    { nextToken = Prelude.Nothing,
      defaultLists = Prelude.Nothing,
      maxResults = pMaxResults_
    }

-- | If you specify a value for @MaxResults@ in your list request, and you
-- have more objects than the maximum, AWS Firewall Manager returns this
-- token in the response. For all but the first request, you provide the
-- token returned by the prior request in the request parameters, to
-- retrieve the next batch of objects.
listAppsLists_nextToken :: Lens.Lens' ListAppsLists (Prelude.Maybe Prelude.Text)
listAppsLists_nextToken = Lens.lens (\ListAppsLists' {nextToken} -> nextToken) (\s@ListAppsLists' {} a -> s {nextToken = a} :: ListAppsLists)

-- | Specifies whether the lists to retrieve are default lists owned by AWS
-- Firewall Manager.
listAppsLists_defaultLists :: Lens.Lens' ListAppsLists (Prelude.Maybe Prelude.Bool)
listAppsLists_defaultLists = Lens.lens (\ListAppsLists' {defaultLists} -> defaultLists) (\s@ListAppsLists' {} a -> s {defaultLists = a} :: ListAppsLists)

-- | The maximum number of objects that you want AWS Firewall Manager to
-- return for this request. If more objects are available, in the response,
-- AWS Firewall Manager provides a @NextToken@ value that you can use in a
-- subsequent call to get the next batch of objects.
--
-- If you don\'t specify this, AWS Firewall Manager returns all available
-- objects.
listAppsLists_maxResults :: Lens.Lens' ListAppsLists Prelude.Natural
listAppsLists_maxResults = Lens.lens (\ListAppsLists' {maxResults} -> maxResults) (\s@ListAppsLists' {} a -> s {maxResults = a} :: ListAppsLists)

instance Prelude.AWSRequest ListAppsLists where
  type Rs ListAppsLists = ListAppsListsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAppsListsResponse'
            Prelude.<$> (x Prelude..?> "NextToken")
            Prelude.<*> ( x Prelude..?> "AppsLists"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListAppsLists

instance Prelude.NFData ListAppsLists

instance Prelude.ToHeaders ListAppsLists where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSFMS_20180101.ListAppsLists" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ListAppsLists where
  toJSON ListAppsLists' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("NextToken" Prelude..=) Prelude.<$> nextToken,
            ("DefaultLists" Prelude..=) Prelude.<$> defaultLists,
            Prelude.Just ("MaxResults" Prelude..= maxResults)
          ]
      )

instance Prelude.ToPath ListAppsLists where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ListAppsLists where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListAppsListsResponse' smart constructor.
data ListAppsListsResponse = ListAppsListsResponse'
  { -- | If you specify a value for @MaxResults@ in your list request, and you
    -- have more objects than the maximum, AWS Firewall Manager returns this
    -- token in the response. You can use this token in subsequent requests to
    -- retrieve the next batch of objects.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of @AppsListDataSummary@ objects.
    appsLists :: Prelude.Maybe [AppsListDataSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  ListAppsListsResponse
newListAppsListsResponse pHttpStatus_ =
  ListAppsListsResponse'
    { nextToken = Prelude.Nothing,
      appsLists = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If you specify a value for @MaxResults@ in your list request, and you
-- have more objects than the maximum, AWS Firewall Manager returns this
-- token in the response. You can use this token in subsequent requests to
-- retrieve the next batch of objects.
listAppsListsResponse_nextToken :: Lens.Lens' ListAppsListsResponse (Prelude.Maybe Prelude.Text)
listAppsListsResponse_nextToken = Lens.lens (\ListAppsListsResponse' {nextToken} -> nextToken) (\s@ListAppsListsResponse' {} a -> s {nextToken = a} :: ListAppsListsResponse)

-- | An array of @AppsListDataSummary@ objects.
listAppsListsResponse_appsLists :: Lens.Lens' ListAppsListsResponse (Prelude.Maybe [AppsListDataSummary])
listAppsListsResponse_appsLists = Lens.lens (\ListAppsListsResponse' {appsLists} -> appsLists) (\s@ListAppsListsResponse' {} a -> s {appsLists = a} :: ListAppsListsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
listAppsListsResponse_httpStatus :: Lens.Lens' ListAppsListsResponse Prelude.Int
listAppsListsResponse_httpStatus = Lens.lens (\ListAppsListsResponse' {httpStatus} -> httpStatus) (\s@ListAppsListsResponse' {} a -> s {httpStatus = a} :: ListAppsListsResponse)

instance Prelude.NFData ListAppsListsResponse
