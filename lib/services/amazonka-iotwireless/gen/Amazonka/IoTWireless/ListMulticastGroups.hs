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
-- Module      : Amazonka.IoTWireless.ListMulticastGroups
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the multicast groups registered to your AWS account.
module Amazonka.IoTWireless.ListMulticastGroups
  ( -- * Creating a Request
    ListMulticastGroups (..),
    newListMulticastGroups,

    -- * Request Lenses
    listMulticastGroups_nextToken,
    listMulticastGroups_maxResults,

    -- * Destructuring the Response
    ListMulticastGroupsResponse (..),
    newListMulticastGroupsResponse,

    -- * Response Lenses
    listMulticastGroupsResponse_multicastGroupList,
    listMulticastGroupsResponse_nextToken,
    listMulticastGroupsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListMulticastGroups' smart constructor.
data ListMulticastGroups = ListMulticastGroups'
  { -- | To retrieve the next set of results, the @nextToken@ value from a
    -- previous response; otherwise __null__ to receive the first set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListMulticastGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listMulticastGroups_nextToken' - To retrieve the next set of results, the @nextToken@ value from a
-- previous response; otherwise __null__ to receive the first set of
-- results.
--
-- 'maxResults', 'listMulticastGroups_maxResults' - Undocumented member.
newListMulticastGroups ::
  ListMulticastGroups
newListMulticastGroups =
  ListMulticastGroups'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | To retrieve the next set of results, the @nextToken@ value from a
-- previous response; otherwise __null__ to receive the first set of
-- results.
listMulticastGroups_nextToken :: Lens.Lens' ListMulticastGroups (Prelude.Maybe Prelude.Text)
listMulticastGroups_nextToken = Lens.lens (\ListMulticastGroups' {nextToken} -> nextToken) (\s@ListMulticastGroups' {} a -> s {nextToken = a} :: ListMulticastGroups)

-- | Undocumented member.
listMulticastGroups_maxResults :: Lens.Lens' ListMulticastGroups (Prelude.Maybe Prelude.Natural)
listMulticastGroups_maxResults = Lens.lens (\ListMulticastGroups' {maxResults} -> maxResults) (\s@ListMulticastGroups' {} a -> s {maxResults = a} :: ListMulticastGroups)

instance Core.AWSRequest ListMulticastGroups where
  type
    AWSResponse ListMulticastGroups =
      ListMulticastGroupsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListMulticastGroupsResponse'
            Prelude.<$> ( x Data..?> "MulticastGroupList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListMulticastGroups where
  hashWithSalt _salt ListMulticastGroups' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListMulticastGroups where
  rnf ListMulticastGroups' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Data.ToHeaders ListMulticastGroups where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListMulticastGroups where
  toPath = Prelude.const "/multicast-groups"

instance Data.ToQuery ListMulticastGroups where
  toQuery ListMulticastGroups' {..} =
    Prelude.mconcat
      [ "nextToken" Data.=: nextToken,
        "maxResults" Data.=: maxResults
      ]

-- | /See:/ 'newListMulticastGroupsResponse' smart constructor.
data ListMulticastGroupsResponse = ListMulticastGroupsResponse'
  { multicastGroupList :: Prelude.Maybe [MulticastGroup],
    -- | To retrieve the next set of results, the @nextToken@ value from a
    -- previous response; otherwise __null__ to receive the first set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListMulticastGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'multicastGroupList', 'listMulticastGroupsResponse_multicastGroupList' - Undocumented member.
--
-- 'nextToken', 'listMulticastGroupsResponse_nextToken' - To retrieve the next set of results, the @nextToken@ value from a
-- previous response; otherwise __null__ to receive the first set of
-- results.
--
-- 'httpStatus', 'listMulticastGroupsResponse_httpStatus' - The response's http status code.
newListMulticastGroupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListMulticastGroupsResponse
newListMulticastGroupsResponse pHttpStatus_ =
  ListMulticastGroupsResponse'
    { multicastGroupList =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
listMulticastGroupsResponse_multicastGroupList :: Lens.Lens' ListMulticastGroupsResponse (Prelude.Maybe [MulticastGroup])
listMulticastGroupsResponse_multicastGroupList = Lens.lens (\ListMulticastGroupsResponse' {multicastGroupList} -> multicastGroupList) (\s@ListMulticastGroupsResponse' {} a -> s {multicastGroupList = a} :: ListMulticastGroupsResponse) Prelude.. Lens.mapping Lens.coerced

-- | To retrieve the next set of results, the @nextToken@ value from a
-- previous response; otherwise __null__ to receive the first set of
-- results.
listMulticastGroupsResponse_nextToken :: Lens.Lens' ListMulticastGroupsResponse (Prelude.Maybe Prelude.Text)
listMulticastGroupsResponse_nextToken = Lens.lens (\ListMulticastGroupsResponse' {nextToken} -> nextToken) (\s@ListMulticastGroupsResponse' {} a -> s {nextToken = a} :: ListMulticastGroupsResponse)

-- | The response's http status code.
listMulticastGroupsResponse_httpStatus :: Lens.Lens' ListMulticastGroupsResponse Prelude.Int
listMulticastGroupsResponse_httpStatus = Lens.lens (\ListMulticastGroupsResponse' {httpStatus} -> httpStatus) (\s@ListMulticastGroupsResponse' {} a -> s {httpStatus = a} :: ListMulticastGroupsResponse)

instance Prelude.NFData ListMulticastGroupsResponse where
  rnf ListMulticastGroupsResponse' {..} =
    Prelude.rnf multicastGroupList
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
