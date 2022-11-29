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
-- Module      : Amazonka.DeviceFarm.ListInstanceProfiles
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about all the instance profiles in an AWS account.
--
-- This operation returns paginated results.
module Amazonka.DeviceFarm.ListInstanceProfiles
  ( -- * Creating a Request
    ListInstanceProfiles (..),
    newListInstanceProfiles,

    -- * Request Lenses
    listInstanceProfiles_nextToken,
    listInstanceProfiles_maxResults,

    -- * Destructuring the Response
    ListInstanceProfilesResponse (..),
    newListInstanceProfilesResponse,

    -- * Response Lenses
    listInstanceProfilesResponse_nextToken,
    listInstanceProfilesResponse_instanceProfiles,
    listInstanceProfilesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DeviceFarm.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListInstanceProfiles' smart constructor.
data ListInstanceProfiles = ListInstanceProfiles'
  { -- | An identifier that was returned from the previous call to this
    -- operation, which can be used to return the next set of items in the
    -- list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An integer that specifies the maximum number of items you want to return
    -- in the API response.
    maxResults :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListInstanceProfiles' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listInstanceProfiles_nextToken' - An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
--
-- 'maxResults', 'listInstanceProfiles_maxResults' - An integer that specifies the maximum number of items you want to return
-- in the API response.
newListInstanceProfiles ::
  ListInstanceProfiles
newListInstanceProfiles =
  ListInstanceProfiles'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
listInstanceProfiles_nextToken :: Lens.Lens' ListInstanceProfiles (Prelude.Maybe Prelude.Text)
listInstanceProfiles_nextToken = Lens.lens (\ListInstanceProfiles' {nextToken} -> nextToken) (\s@ListInstanceProfiles' {} a -> s {nextToken = a} :: ListInstanceProfiles)

-- | An integer that specifies the maximum number of items you want to return
-- in the API response.
listInstanceProfiles_maxResults :: Lens.Lens' ListInstanceProfiles (Prelude.Maybe Prelude.Int)
listInstanceProfiles_maxResults = Lens.lens (\ListInstanceProfiles' {maxResults} -> maxResults) (\s@ListInstanceProfiles' {} a -> s {maxResults = a} :: ListInstanceProfiles)

instance Core.AWSPager ListInstanceProfiles where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listInstanceProfilesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listInstanceProfilesResponse_instanceProfiles
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listInstanceProfiles_nextToken
          Lens..~ rs
          Lens.^? listInstanceProfilesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListInstanceProfiles where
  type
    AWSResponse ListInstanceProfiles =
      ListInstanceProfilesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListInstanceProfilesResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> ( x Core..?> "instanceProfiles"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListInstanceProfiles where
  hashWithSalt _salt ListInstanceProfiles' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListInstanceProfiles where
  rnf ListInstanceProfiles' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders ListInstanceProfiles where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DeviceFarm_20150623.ListInstanceProfiles" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListInstanceProfiles where
  toJSON ListInstanceProfiles' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("nextToken" Core..=) Prelude.<$> nextToken,
            ("maxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListInstanceProfiles where
  toPath = Prelude.const "/"

instance Core.ToQuery ListInstanceProfiles where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListInstanceProfilesResponse' smart constructor.
data ListInstanceProfilesResponse = ListInstanceProfilesResponse'
  { -- | An identifier that can be used in the next call to this operation to
    -- return the next set of items in the list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An object that contains information about your instance profiles.
    instanceProfiles :: Prelude.Maybe [InstanceProfile],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListInstanceProfilesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listInstanceProfilesResponse_nextToken' - An identifier that can be used in the next call to this operation to
-- return the next set of items in the list.
--
-- 'instanceProfiles', 'listInstanceProfilesResponse_instanceProfiles' - An object that contains information about your instance profiles.
--
-- 'httpStatus', 'listInstanceProfilesResponse_httpStatus' - The response's http status code.
newListInstanceProfilesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListInstanceProfilesResponse
newListInstanceProfilesResponse pHttpStatus_ =
  ListInstanceProfilesResponse'
    { nextToken =
        Prelude.Nothing,
      instanceProfiles = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An identifier that can be used in the next call to this operation to
-- return the next set of items in the list.
listInstanceProfilesResponse_nextToken :: Lens.Lens' ListInstanceProfilesResponse (Prelude.Maybe Prelude.Text)
listInstanceProfilesResponse_nextToken = Lens.lens (\ListInstanceProfilesResponse' {nextToken} -> nextToken) (\s@ListInstanceProfilesResponse' {} a -> s {nextToken = a} :: ListInstanceProfilesResponse)

-- | An object that contains information about your instance profiles.
listInstanceProfilesResponse_instanceProfiles :: Lens.Lens' ListInstanceProfilesResponse (Prelude.Maybe [InstanceProfile])
listInstanceProfilesResponse_instanceProfiles = Lens.lens (\ListInstanceProfilesResponse' {instanceProfiles} -> instanceProfiles) (\s@ListInstanceProfilesResponse' {} a -> s {instanceProfiles = a} :: ListInstanceProfilesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listInstanceProfilesResponse_httpStatus :: Lens.Lens' ListInstanceProfilesResponse Prelude.Int
listInstanceProfilesResponse_httpStatus = Lens.lens (\ListInstanceProfilesResponse' {httpStatus} -> httpStatus) (\s@ListInstanceProfilesResponse' {} a -> s {httpStatus = a} :: ListInstanceProfilesResponse)

instance Prelude.NFData ListInstanceProfilesResponse where
  rnf ListInstanceProfilesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf instanceProfiles
      `Prelude.seq` Prelude.rnf httpStatus
