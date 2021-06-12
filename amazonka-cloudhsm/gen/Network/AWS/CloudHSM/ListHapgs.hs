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
-- Module      : Network.AWS.CloudHSM.ListHapgs
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This is documentation for __AWS CloudHSM Classic__. For more
-- information, see
-- <http://aws.amazon.com/cloudhsm/faqs-classic/ AWS CloudHSM Classic FAQs>,
-- the
-- <http://docs.aws.amazon.com/cloudhsm/classic/userguide/ AWS CloudHSM Classic User Guide>,
-- and the
-- <http://docs.aws.amazon.com/cloudhsm/classic/APIReference/ AWS CloudHSM Classic API Reference>.
--
-- __For information about the current version of AWS CloudHSM__, see
-- <http://aws.amazon.com/cloudhsm/ AWS CloudHSM>, the
-- <http://docs.aws.amazon.com/cloudhsm/latest/userguide/ AWS CloudHSM User Guide>,
-- and the
-- <http://docs.aws.amazon.com/cloudhsm/latest/APIReference/ AWS CloudHSM API Reference>.
--
-- Lists the high-availability partition groups for the account.
--
-- This operation supports pagination with the use of the @NextToken@
-- member. If more results are available, the @NextToken@ member of the
-- response contains a token that you pass in the next call to @ListHapgs@
-- to retrieve the next set of items.
--
-- This operation returns paginated results.
module Network.AWS.CloudHSM.ListHapgs
  ( -- * Creating a Request
    ListHapgs (..),
    newListHapgs,

    -- * Request Lenses
    listHapgs_nextToken,

    -- * Destructuring the Response
    ListHapgsResponse (..),
    newListHapgsResponse,

    -- * Response Lenses
    listHapgsResponse_nextToken,
    listHapgsResponse_httpStatus,
    listHapgsResponse_hapgList,
  )
where

import Network.AWS.CloudHSM.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListHapgs' smart constructor.
data ListHapgs = ListHapgs'
  { -- | The @NextToken@ value from a previous call to @ListHapgs@. Pass null if
    -- this is the first call.
    nextToken :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListHapgs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listHapgs_nextToken' - The @NextToken@ value from a previous call to @ListHapgs@. Pass null if
-- this is the first call.
newListHapgs ::
  ListHapgs
newListHapgs = ListHapgs' {nextToken = Core.Nothing}

-- | The @NextToken@ value from a previous call to @ListHapgs@. Pass null if
-- this is the first call.
listHapgs_nextToken :: Lens.Lens' ListHapgs (Core.Maybe Core.Text)
listHapgs_nextToken = Lens.lens (\ListHapgs' {nextToken} -> nextToken) (\s@ListHapgs' {} a -> s {nextToken = a} :: ListHapgs)

instance Core.AWSPager ListHapgs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listHapgsResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop (rs Lens.^. listHapgsResponse_hapgList) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listHapgs_nextToken
          Lens..~ rs
          Lens.^? listHapgsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListHapgs where
  type AWSResponse ListHapgs = ListHapgsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListHapgsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..?> "HapgList" Core..!@ Core.mempty)
      )

instance Core.Hashable ListHapgs

instance Core.NFData ListHapgs

instance Core.ToHeaders ListHapgs where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CloudHsmFrontendService.ListHapgs" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListHapgs where
  toJSON ListHapgs' {..} =
    Core.object
      ( Core.catMaybes
          [("NextToken" Core..=) Core.<$> nextToken]
      )

instance Core.ToPath ListHapgs where
  toPath = Core.const "/"

instance Core.ToQuery ListHapgs where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListHapgsResponse' smart constructor.
data ListHapgsResponse = ListHapgsResponse'
  { -- | If not null, more results are available. Pass this value to @ListHapgs@
    -- to retrieve the next set of items.
    nextToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The list of high-availability partition groups.
    hapgList :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListHapgsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listHapgsResponse_nextToken' - If not null, more results are available. Pass this value to @ListHapgs@
-- to retrieve the next set of items.
--
-- 'httpStatus', 'listHapgsResponse_httpStatus' - The response's http status code.
--
-- 'hapgList', 'listHapgsResponse_hapgList' - The list of high-availability partition groups.
newListHapgsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListHapgsResponse
newListHapgsResponse pHttpStatus_ =
  ListHapgsResponse'
    { nextToken = Core.Nothing,
      httpStatus = pHttpStatus_,
      hapgList = Core.mempty
    }

-- | If not null, more results are available. Pass this value to @ListHapgs@
-- to retrieve the next set of items.
listHapgsResponse_nextToken :: Lens.Lens' ListHapgsResponse (Core.Maybe Core.Text)
listHapgsResponse_nextToken = Lens.lens (\ListHapgsResponse' {nextToken} -> nextToken) (\s@ListHapgsResponse' {} a -> s {nextToken = a} :: ListHapgsResponse)

-- | The response's http status code.
listHapgsResponse_httpStatus :: Lens.Lens' ListHapgsResponse Core.Int
listHapgsResponse_httpStatus = Lens.lens (\ListHapgsResponse' {httpStatus} -> httpStatus) (\s@ListHapgsResponse' {} a -> s {httpStatus = a} :: ListHapgsResponse)

-- | The list of high-availability partition groups.
listHapgsResponse_hapgList :: Lens.Lens' ListHapgsResponse [Core.Text]
listHapgsResponse_hapgList = Lens.lens (\ListHapgsResponse' {hapgList} -> hapgList) (\s@ListHapgsResponse' {} a -> s {hapgList = a} :: ListHapgsResponse) Core.. Lens._Coerce

instance Core.NFData ListHapgsResponse
