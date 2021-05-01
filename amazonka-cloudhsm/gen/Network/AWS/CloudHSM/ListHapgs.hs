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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListHapgs' smart constructor.
data ListHapgs = ListHapgs'
  { -- | The @NextToken@ value from a previous call to @ListHapgs@. Pass null if
    -- this is the first call.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
newListHapgs =
  ListHapgs' {nextToken = Prelude.Nothing}

-- | The @NextToken@ value from a previous call to @ListHapgs@. Pass null if
-- this is the first call.
listHapgs_nextToken :: Lens.Lens' ListHapgs (Prelude.Maybe Prelude.Text)
listHapgs_nextToken = Lens.lens (\ListHapgs' {nextToken} -> nextToken) (\s@ListHapgs' {} a -> s {nextToken = a} :: ListHapgs)

instance Pager.AWSPager ListHapgs where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? listHapgsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop (rs Lens.^. listHapgsResponse_hapgList) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& listHapgs_nextToken
          Lens..~ rs
          Lens.^? listHapgsResponse_nextToken Prelude.. Lens._Just

instance Prelude.AWSRequest ListHapgs where
  type Rs ListHapgs = ListHapgsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListHapgsResponse'
            Prelude.<$> (x Prelude..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Prelude..?> "HapgList"
                            Prelude..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListHapgs

instance Prelude.NFData ListHapgs

instance Prelude.ToHeaders ListHapgs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "CloudHsmFrontendService.ListHapgs" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ListHapgs where
  toJSON ListHapgs' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("NextToken" Prelude..=) Prelude.<$> nextToken]
      )

instance Prelude.ToPath ListHapgs where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ListHapgs where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListHapgsResponse' smart constructor.
data ListHapgsResponse = ListHapgsResponse'
  { -- | If not null, more results are available. Pass this value to @ListHapgs@
    -- to retrieve the next set of items.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The list of high-availability partition groups.
    hapgList :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  ListHapgsResponse
newListHapgsResponse pHttpStatus_ =
  ListHapgsResponse'
    { nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      hapgList = Prelude.mempty
    }

-- | If not null, more results are available. Pass this value to @ListHapgs@
-- to retrieve the next set of items.
listHapgsResponse_nextToken :: Lens.Lens' ListHapgsResponse (Prelude.Maybe Prelude.Text)
listHapgsResponse_nextToken = Lens.lens (\ListHapgsResponse' {nextToken} -> nextToken) (\s@ListHapgsResponse' {} a -> s {nextToken = a} :: ListHapgsResponse)

-- | The response's http status code.
listHapgsResponse_httpStatus :: Lens.Lens' ListHapgsResponse Prelude.Int
listHapgsResponse_httpStatus = Lens.lens (\ListHapgsResponse' {httpStatus} -> httpStatus) (\s@ListHapgsResponse' {} a -> s {httpStatus = a} :: ListHapgsResponse)

-- | The list of high-availability partition groups.
listHapgsResponse_hapgList :: Lens.Lens' ListHapgsResponse [Prelude.Text]
listHapgsResponse_hapgList = Lens.lens (\ListHapgsResponse' {hapgList} -> hapgList) (\s@ListHapgsResponse' {} a -> s {hapgList = a} :: ListHapgsResponse) Prelude.. Prelude._Coerce

instance Prelude.NFData ListHapgsResponse
