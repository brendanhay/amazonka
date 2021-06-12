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
-- Module      : Network.AWS.CloudHSM.ListHsms
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
-- Retrieves the identifiers of all of the HSMs provisioned for the current
-- customer.
--
-- This operation supports pagination with the use of the @NextToken@
-- member. If more results are available, the @NextToken@ member of the
-- response contains a token that you pass in the next call to @ListHsms@
-- to retrieve the next set of items.
--
-- This operation returns paginated results.
module Network.AWS.CloudHSM.ListHsms
  ( -- * Creating a Request
    ListHsms (..),
    newListHsms,

    -- * Request Lenses
    listHsms_nextToken,

    -- * Destructuring the Response
    ListHsmsResponse (..),
    newListHsmsResponse,

    -- * Response Lenses
    listHsmsResponse_hsmList,
    listHsmsResponse_nextToken,
    listHsmsResponse_httpStatus,
  )
where

import Network.AWS.CloudHSM.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListHsms' smart constructor.
data ListHsms = ListHsms'
  { -- | The @NextToken@ value from a previous call to @ListHsms@. Pass null if
    -- this is the first call.
    nextToken :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListHsms' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listHsms_nextToken' - The @NextToken@ value from a previous call to @ListHsms@. Pass null if
-- this is the first call.
newListHsms ::
  ListHsms
newListHsms = ListHsms' {nextToken = Core.Nothing}

-- | The @NextToken@ value from a previous call to @ListHsms@. Pass null if
-- this is the first call.
listHsms_nextToken :: Lens.Lens' ListHsms (Core.Maybe Core.Text)
listHsms_nextToken = Lens.lens (\ListHsms' {nextToken} -> nextToken) (\s@ListHsms' {} a -> s {nextToken = a} :: ListHsms)

instance Core.AWSPager ListHsms where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listHsmsResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listHsmsResponse_hsmList Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listHsms_nextToken
          Lens..~ rs
          Lens.^? listHsmsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListHsms where
  type AWSResponse ListHsms = ListHsmsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListHsmsResponse'
            Core.<$> (x Core..?> "HsmList" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListHsms

instance Core.NFData ListHsms

instance Core.ToHeaders ListHsms where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CloudHsmFrontendService.ListHsms" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListHsms where
  toJSON ListHsms' {..} =
    Core.object
      ( Core.catMaybes
          [("NextToken" Core..=) Core.<$> nextToken]
      )

instance Core.ToPath ListHsms where
  toPath = Core.const "/"

instance Core.ToQuery ListHsms where
  toQuery = Core.const Core.mempty

-- | Contains the output of the @ListHsms@ operation.
--
-- /See:/ 'newListHsmsResponse' smart constructor.
data ListHsmsResponse = ListHsmsResponse'
  { -- | The list of ARNs that identify the HSMs.
    hsmList :: Core.Maybe [Core.Text],
    -- | If not null, more results are available. Pass this value to @ListHsms@
    -- to retrieve the next set of items.
    nextToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListHsmsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hsmList', 'listHsmsResponse_hsmList' - The list of ARNs that identify the HSMs.
--
-- 'nextToken', 'listHsmsResponse_nextToken' - If not null, more results are available. Pass this value to @ListHsms@
-- to retrieve the next set of items.
--
-- 'httpStatus', 'listHsmsResponse_httpStatus' - The response's http status code.
newListHsmsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListHsmsResponse
newListHsmsResponse pHttpStatus_ =
  ListHsmsResponse'
    { hsmList = Core.Nothing,
      nextToken = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of ARNs that identify the HSMs.
listHsmsResponse_hsmList :: Lens.Lens' ListHsmsResponse (Core.Maybe [Core.Text])
listHsmsResponse_hsmList = Lens.lens (\ListHsmsResponse' {hsmList} -> hsmList) (\s@ListHsmsResponse' {} a -> s {hsmList = a} :: ListHsmsResponse) Core.. Lens.mapping Lens._Coerce

-- | If not null, more results are available. Pass this value to @ListHsms@
-- to retrieve the next set of items.
listHsmsResponse_nextToken :: Lens.Lens' ListHsmsResponse (Core.Maybe Core.Text)
listHsmsResponse_nextToken = Lens.lens (\ListHsmsResponse' {nextToken} -> nextToken) (\s@ListHsmsResponse' {} a -> s {nextToken = a} :: ListHsmsResponse)

-- | The response's http status code.
listHsmsResponse_httpStatus :: Lens.Lens' ListHsmsResponse Core.Int
listHsmsResponse_httpStatus = Lens.lens (\ListHsmsResponse' {httpStatus} -> httpStatus) (\s@ListHsmsResponse' {} a -> s {httpStatus = a} :: ListHsmsResponse)

instance Core.NFData ListHsmsResponse
