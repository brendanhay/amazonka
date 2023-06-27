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
-- Module      : Amazonka.MediaLive.ListInputSecurityGroups
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Produces a list of Input Security Groups for an account
--
-- This operation returns paginated results.
module Amazonka.MediaLive.ListInputSecurityGroups
  ( -- * Creating a Request
    ListInputSecurityGroups (..),
    newListInputSecurityGroups,

    -- * Request Lenses
    listInputSecurityGroups_maxResults,
    listInputSecurityGroups_nextToken,

    -- * Destructuring the Response
    ListInputSecurityGroupsResponse (..),
    newListInputSecurityGroupsResponse,

    -- * Response Lenses
    listInputSecurityGroupsResponse_inputSecurityGroups,
    listInputSecurityGroupsResponse_nextToken,
    listInputSecurityGroupsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Placeholder documentation for ListInputSecurityGroupsRequest
--
-- /See:/ 'newListInputSecurityGroups' smart constructor.
data ListInputSecurityGroups = ListInputSecurityGroups'
  { maxResults :: Prelude.Maybe Prelude.Natural,
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListInputSecurityGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listInputSecurityGroups_maxResults' - Undocumented member.
--
-- 'nextToken', 'listInputSecurityGroups_nextToken' - Undocumented member.
newListInputSecurityGroups ::
  ListInputSecurityGroups
newListInputSecurityGroups =
  ListInputSecurityGroups'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Undocumented member.
listInputSecurityGroups_maxResults :: Lens.Lens' ListInputSecurityGroups (Prelude.Maybe Prelude.Natural)
listInputSecurityGroups_maxResults = Lens.lens (\ListInputSecurityGroups' {maxResults} -> maxResults) (\s@ListInputSecurityGroups' {} a -> s {maxResults = a} :: ListInputSecurityGroups)

-- | Undocumented member.
listInputSecurityGroups_nextToken :: Lens.Lens' ListInputSecurityGroups (Prelude.Maybe Prelude.Text)
listInputSecurityGroups_nextToken = Lens.lens (\ListInputSecurityGroups' {nextToken} -> nextToken) (\s@ListInputSecurityGroups' {} a -> s {nextToken = a} :: ListInputSecurityGroups)

instance Core.AWSPager ListInputSecurityGroups where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listInputSecurityGroupsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listInputSecurityGroupsResponse_inputSecurityGroups
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listInputSecurityGroups_nextToken
          Lens..~ rs
          Lens.^? listInputSecurityGroupsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListInputSecurityGroups where
  type
    AWSResponse ListInputSecurityGroups =
      ListInputSecurityGroupsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListInputSecurityGroupsResponse'
            Prelude.<$> ( x
                            Data..?> "inputSecurityGroups"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListInputSecurityGroups where
  hashWithSalt _salt ListInputSecurityGroups' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListInputSecurityGroups where
  rnf ListInputSecurityGroups' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListInputSecurityGroups where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListInputSecurityGroups where
  toPath = Prelude.const "/prod/inputSecurityGroups"

instance Data.ToQuery ListInputSecurityGroups where
  toQuery ListInputSecurityGroups' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | Placeholder documentation for ListInputSecurityGroupsResponse
--
-- /See:/ 'newListInputSecurityGroupsResponse' smart constructor.
data ListInputSecurityGroupsResponse = ListInputSecurityGroupsResponse'
  { -- | List of input security groups
    inputSecurityGroups :: Prelude.Maybe [InputSecurityGroup],
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListInputSecurityGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inputSecurityGroups', 'listInputSecurityGroupsResponse_inputSecurityGroups' - List of input security groups
--
-- 'nextToken', 'listInputSecurityGroupsResponse_nextToken' - Undocumented member.
--
-- 'httpStatus', 'listInputSecurityGroupsResponse_httpStatus' - The response's http status code.
newListInputSecurityGroupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListInputSecurityGroupsResponse
newListInputSecurityGroupsResponse pHttpStatus_ =
  ListInputSecurityGroupsResponse'
    { inputSecurityGroups =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | List of input security groups
listInputSecurityGroupsResponse_inputSecurityGroups :: Lens.Lens' ListInputSecurityGroupsResponse (Prelude.Maybe [InputSecurityGroup])
listInputSecurityGroupsResponse_inputSecurityGroups = Lens.lens (\ListInputSecurityGroupsResponse' {inputSecurityGroups} -> inputSecurityGroups) (\s@ListInputSecurityGroupsResponse' {} a -> s {inputSecurityGroups = a} :: ListInputSecurityGroupsResponse) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
listInputSecurityGroupsResponse_nextToken :: Lens.Lens' ListInputSecurityGroupsResponse (Prelude.Maybe Prelude.Text)
listInputSecurityGroupsResponse_nextToken = Lens.lens (\ListInputSecurityGroupsResponse' {nextToken} -> nextToken) (\s@ListInputSecurityGroupsResponse' {} a -> s {nextToken = a} :: ListInputSecurityGroupsResponse)

-- | The response's http status code.
listInputSecurityGroupsResponse_httpStatus :: Lens.Lens' ListInputSecurityGroupsResponse Prelude.Int
listInputSecurityGroupsResponse_httpStatus = Lens.lens (\ListInputSecurityGroupsResponse' {httpStatus} -> httpStatus) (\s@ListInputSecurityGroupsResponse' {} a -> s {httpStatus = a} :: ListInputSecurityGroupsResponse)

instance
  Prelude.NFData
    ListInputSecurityGroupsResponse
  where
  rnf ListInputSecurityGroupsResponse' {..} =
    Prelude.rnf inputSecurityGroups
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
