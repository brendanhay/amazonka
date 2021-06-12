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
-- Module      : Network.AWS.DeviceFarm.ListInstanceProfiles
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about all the instance profiles in an AWS account.
--
-- This operation returns paginated results.
module Network.AWS.DeviceFarm.ListInstanceProfiles
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

import qualified Network.AWS.Core as Core
import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListInstanceProfiles' smart constructor.
data ListInstanceProfiles = ListInstanceProfiles'
  { -- | An identifier that was returned from the previous call to this
    -- operation, which can be used to return the next set of items in the
    -- list.
    nextToken :: Core.Maybe Core.Text,
    -- | An integer that specifies the maximum number of items you want to return
    -- in the API response.
    maxResults :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing
    }

-- | An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
listInstanceProfiles_nextToken :: Lens.Lens' ListInstanceProfiles (Core.Maybe Core.Text)
listInstanceProfiles_nextToken = Lens.lens (\ListInstanceProfiles' {nextToken} -> nextToken) (\s@ListInstanceProfiles' {} a -> s {nextToken = a} :: ListInstanceProfiles)

-- | An integer that specifies the maximum number of items you want to return
-- in the API response.
listInstanceProfiles_maxResults :: Lens.Lens' ListInstanceProfiles (Core.Maybe Core.Int)
listInstanceProfiles_maxResults = Lens.lens (\ListInstanceProfiles' {maxResults} -> maxResults) (\s@ListInstanceProfiles' {} a -> s {maxResults = a} :: ListInstanceProfiles)

instance Core.AWSPager ListInstanceProfiles where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listInstanceProfilesResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listInstanceProfilesResponse_instanceProfiles
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listInstanceProfiles_nextToken
          Lens..~ rs
          Lens.^? listInstanceProfilesResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListInstanceProfiles where
  type
    AWSResponse ListInstanceProfiles =
      ListInstanceProfilesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListInstanceProfilesResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "instanceProfiles" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListInstanceProfiles

instance Core.NFData ListInstanceProfiles

instance Core.ToHeaders ListInstanceProfiles where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DeviceFarm_20150623.ListInstanceProfiles" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListInstanceProfiles where
  toJSON ListInstanceProfiles' {..} =
    Core.object
      ( Core.catMaybes
          [ ("nextToken" Core..=) Core.<$> nextToken,
            ("maxResults" Core..=) Core.<$> maxResults
          ]
      )

instance Core.ToPath ListInstanceProfiles where
  toPath = Core.const "/"

instance Core.ToQuery ListInstanceProfiles where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListInstanceProfilesResponse' smart constructor.
data ListInstanceProfilesResponse = ListInstanceProfilesResponse'
  { -- | An identifier that can be used in the next call to this operation to
    -- return the next set of items in the list.
    nextToken :: Core.Maybe Core.Text,
    -- | An object that contains information about your instance profiles.
    instanceProfiles :: Core.Maybe [InstanceProfile],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  ListInstanceProfilesResponse
newListInstanceProfilesResponse pHttpStatus_ =
  ListInstanceProfilesResponse'
    { nextToken =
        Core.Nothing,
      instanceProfiles = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An identifier that can be used in the next call to this operation to
-- return the next set of items in the list.
listInstanceProfilesResponse_nextToken :: Lens.Lens' ListInstanceProfilesResponse (Core.Maybe Core.Text)
listInstanceProfilesResponse_nextToken = Lens.lens (\ListInstanceProfilesResponse' {nextToken} -> nextToken) (\s@ListInstanceProfilesResponse' {} a -> s {nextToken = a} :: ListInstanceProfilesResponse)

-- | An object that contains information about your instance profiles.
listInstanceProfilesResponse_instanceProfiles :: Lens.Lens' ListInstanceProfilesResponse (Core.Maybe [InstanceProfile])
listInstanceProfilesResponse_instanceProfiles = Lens.lens (\ListInstanceProfilesResponse' {instanceProfiles} -> instanceProfiles) (\s@ListInstanceProfilesResponse' {} a -> s {instanceProfiles = a} :: ListInstanceProfilesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listInstanceProfilesResponse_httpStatus :: Lens.Lens' ListInstanceProfilesResponse Core.Int
listInstanceProfilesResponse_httpStatus = Lens.lens (\ListInstanceProfilesResponse' {httpStatus} -> httpStatus) (\s@ListInstanceProfilesResponse' {} a -> s {httpStatus = a} :: ListInstanceProfilesResponse)

instance Core.NFData ListInstanceProfilesResponse
