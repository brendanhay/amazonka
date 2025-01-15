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
-- Module      : Amazonka.CloudHSM.ListHapgs
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This is documentation for __AWS CloudHSM Classic__. For more
-- information, see
-- <http://aws.amazon.com/cloudhsm/faqs-classic/ AWS CloudHSM Classic FAQs>,
-- the
-- <https://docs.aws.amazon.com/cloudhsm/classic/userguide/ AWS CloudHSM Classic User Guide>,
-- and the
-- <https://docs.aws.amazon.com/cloudhsm/classic/APIReference/ AWS CloudHSM Classic API Reference>.
--
-- __For information about the current version of AWS CloudHSM__, see
-- <http://aws.amazon.com/cloudhsm/ AWS CloudHSM>, the
-- <https://docs.aws.amazon.com/cloudhsm/latest/userguide/ AWS CloudHSM User Guide>,
-- and the
-- <https://docs.aws.amazon.com/cloudhsm/latest/APIReference/ AWS CloudHSM API Reference>.
--
-- Lists the high-availability partition groups for the account.
--
-- This operation supports pagination with the use of the @NextToken@
-- member. If more results are available, the @NextToken@ member of the
-- response contains a token that you pass in the next call to @ListHapgs@
-- to retrieve the next set of items.
--
-- This operation returns paginated results.
module Amazonka.CloudHSM.ListHapgs
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

import Amazonka.CloudHSM.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListHapgs' smart constructor.
data ListHapgs = ListHapgs'
  { -- | The @NextToken@ value from a previous call to @ListHapgs@. Pass null if
    -- this is the first call.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSPager ListHapgs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listHapgsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop (rs Lens.^. listHapgsResponse_hapgList) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listHapgs_nextToken
              Lens..~ rs
              Lens.^? listHapgsResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest ListHapgs where
  type AWSResponse ListHapgs = ListHapgsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListHapgsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "HapgList" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListHapgs where
  hashWithSalt _salt ListHapgs' {..} =
    _salt `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListHapgs where
  rnf ListHapgs' {..} = Prelude.rnf nextToken

instance Data.ToHeaders ListHapgs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CloudHsmFrontendService.ListHapgs" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListHapgs where
  toJSON ListHapgs' {..} =
    Data.object
      ( Prelude.catMaybes
          [("NextToken" Data..=) Prelude.<$> nextToken]
      )

instance Data.ToPath ListHapgs where
  toPath = Prelude.const "/"

instance Data.ToQuery ListHapgs where
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
listHapgsResponse_hapgList = Lens.lens (\ListHapgsResponse' {hapgList} -> hapgList) (\s@ListHapgsResponse' {} a -> s {hapgList = a} :: ListHapgsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListHapgsResponse where
  rnf ListHapgsResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf httpStatus `Prelude.seq`
        Prelude.rnf hapgList
