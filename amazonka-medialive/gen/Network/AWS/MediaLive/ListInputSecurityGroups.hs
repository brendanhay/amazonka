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
-- Module      : Network.AWS.MediaLive.ListInputSecurityGroups
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Produces a list of Input Security Groups for an account
--
-- This operation returns paginated results.
module Network.AWS.MediaLive.ListInputSecurityGroups
  ( -- * Creating a Request
    ListInputSecurityGroups (..),
    newListInputSecurityGroups,

    -- * Request Lenses
    listInputSecurityGroups_nextToken,
    listInputSecurityGroups_maxResults,

    -- * Destructuring the Response
    ListInputSecurityGroupsResponse (..),
    newListInputSecurityGroupsResponse,

    -- * Response Lenses
    listInputSecurityGroupsResponse_nextToken,
    listInputSecurityGroupsResponse_inputSecurityGroups,
    listInputSecurityGroupsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Placeholder documentation for ListInputSecurityGroupsRequest
--
-- /See:/ 'newListInputSecurityGroups' smart constructor.
data ListInputSecurityGroups = ListInputSecurityGroups'
  { nextToken :: Core.Maybe Core.Text,
    maxResults :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListInputSecurityGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listInputSecurityGroups_nextToken' - Undocumented member.
--
-- 'maxResults', 'listInputSecurityGroups_maxResults' - Undocumented member.
newListInputSecurityGroups ::
  ListInputSecurityGroups
newListInputSecurityGroups =
  ListInputSecurityGroups'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing
    }

-- | Undocumented member.
listInputSecurityGroups_nextToken :: Lens.Lens' ListInputSecurityGroups (Core.Maybe Core.Text)
listInputSecurityGroups_nextToken = Lens.lens (\ListInputSecurityGroups' {nextToken} -> nextToken) (\s@ListInputSecurityGroups' {} a -> s {nextToken = a} :: ListInputSecurityGroups)

-- | Undocumented member.
listInputSecurityGroups_maxResults :: Lens.Lens' ListInputSecurityGroups (Core.Maybe Core.Natural)
listInputSecurityGroups_maxResults = Lens.lens (\ListInputSecurityGroups' {maxResults} -> maxResults) (\s@ListInputSecurityGroups' {} a -> s {maxResults = a} :: ListInputSecurityGroups)

instance Core.AWSPager ListInputSecurityGroups where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listInputSecurityGroupsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listInputSecurityGroupsResponse_inputSecurityGroups
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listInputSecurityGroups_nextToken
          Lens..~ rs
          Lens.^? listInputSecurityGroupsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListInputSecurityGroups where
  type
    AWSResponse ListInputSecurityGroups =
      ListInputSecurityGroupsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListInputSecurityGroupsResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> ( x Core..?> "inputSecurityGroups"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListInputSecurityGroups

instance Core.NFData ListInputSecurityGroups

instance Core.ToHeaders ListInputSecurityGroups where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath ListInputSecurityGroups where
  toPath = Core.const "/prod/inputSecurityGroups"

instance Core.ToQuery ListInputSecurityGroups where
  toQuery ListInputSecurityGroups' {..} =
    Core.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | Placeholder documentation for ListInputSecurityGroupsResponse
--
-- /See:/ 'newListInputSecurityGroupsResponse' smart constructor.
data ListInputSecurityGroupsResponse = ListInputSecurityGroupsResponse'
  { nextToken :: Core.Maybe Core.Text,
    -- | List of input security groups
    inputSecurityGroups :: Core.Maybe [InputSecurityGroup],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListInputSecurityGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listInputSecurityGroupsResponse_nextToken' - Undocumented member.
--
-- 'inputSecurityGroups', 'listInputSecurityGroupsResponse_inputSecurityGroups' - List of input security groups
--
-- 'httpStatus', 'listInputSecurityGroupsResponse_httpStatus' - The response's http status code.
newListInputSecurityGroupsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListInputSecurityGroupsResponse
newListInputSecurityGroupsResponse pHttpStatus_ =
  ListInputSecurityGroupsResponse'
    { nextToken =
        Core.Nothing,
      inputSecurityGroups = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
listInputSecurityGroupsResponse_nextToken :: Lens.Lens' ListInputSecurityGroupsResponse (Core.Maybe Core.Text)
listInputSecurityGroupsResponse_nextToken = Lens.lens (\ListInputSecurityGroupsResponse' {nextToken} -> nextToken) (\s@ListInputSecurityGroupsResponse' {} a -> s {nextToken = a} :: ListInputSecurityGroupsResponse)

-- | List of input security groups
listInputSecurityGroupsResponse_inputSecurityGroups :: Lens.Lens' ListInputSecurityGroupsResponse (Core.Maybe [InputSecurityGroup])
listInputSecurityGroupsResponse_inputSecurityGroups = Lens.lens (\ListInputSecurityGroupsResponse' {inputSecurityGroups} -> inputSecurityGroups) (\s@ListInputSecurityGroupsResponse' {} a -> s {inputSecurityGroups = a} :: ListInputSecurityGroupsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listInputSecurityGroupsResponse_httpStatus :: Lens.Lens' ListInputSecurityGroupsResponse Core.Int
listInputSecurityGroupsResponse_httpStatus = Lens.lens (\ListInputSecurityGroupsResponse' {httpStatus} -> httpStatus) (\s@ListInputSecurityGroupsResponse' {} a -> s {httpStatus = a} :: ListInputSecurityGroupsResponse)

instance Core.NFData ListInputSecurityGroupsResponse
