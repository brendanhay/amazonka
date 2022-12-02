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
-- Module      : Amazonka.CloudHSM.ListHsms
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
-- Retrieves the identifiers of all of the HSMs provisioned for the current
-- customer.
--
-- This operation supports pagination with the use of the @NextToken@
-- member. If more results are available, the @NextToken@ member of the
-- response contains a token that you pass in the next call to @ListHsms@
-- to retrieve the next set of items.
--
-- This operation returns paginated results.
module Amazonka.CloudHSM.ListHsms
  ( -- * Creating a Request
    ListHsms (..),
    newListHsms,

    -- * Request Lenses
    listHsms_nextToken,

    -- * Destructuring the Response
    ListHsmsResponse (..),
    newListHsmsResponse,

    -- * Response Lenses
    listHsmsResponse_nextToken,
    listHsmsResponse_hsmList,
    listHsmsResponse_httpStatus,
  )
where

import Amazonka.CloudHSM.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListHsms' smart constructor.
data ListHsms = ListHsms'
  { -- | The @NextToken@ value from a previous call to @ListHsms@. Pass null if
    -- this is the first call.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
newListHsms = ListHsms' {nextToken = Prelude.Nothing}

-- | The @NextToken@ value from a previous call to @ListHsms@. Pass null if
-- this is the first call.
listHsms_nextToken :: Lens.Lens' ListHsms (Prelude.Maybe Prelude.Text)
listHsms_nextToken = Lens.lens (\ListHsms' {nextToken} -> nextToken) (\s@ListHsms' {} a -> s {nextToken = a} :: ListHsms)

instance Core.AWSPager ListHsms where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listHsmsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listHsmsResponse_hsmList Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listHsms_nextToken
          Lens..~ rs
          Lens.^? listHsmsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListHsms where
  type AWSResponse ListHsms = ListHsmsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListHsmsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "HsmList" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListHsms where
  hashWithSalt _salt ListHsms' {..} =
    _salt `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListHsms where
  rnf ListHsms' {..} = Prelude.rnf nextToken

instance Data.ToHeaders ListHsms where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CloudHsmFrontendService.ListHsms" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListHsms where
  toJSON ListHsms' {..} =
    Data.object
      ( Prelude.catMaybes
          [("NextToken" Data..=) Prelude.<$> nextToken]
      )

instance Data.ToPath ListHsms where
  toPath = Prelude.const "/"

instance Data.ToQuery ListHsms where
  toQuery = Prelude.const Prelude.mempty

-- | Contains the output of the @ListHsms@ operation.
--
-- /See:/ 'newListHsmsResponse' smart constructor.
data ListHsmsResponse = ListHsmsResponse'
  { -- | If not null, more results are available. Pass this value to @ListHsms@
    -- to retrieve the next set of items.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of ARNs that identify the HSMs.
    hsmList :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListHsmsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listHsmsResponse_nextToken' - If not null, more results are available. Pass this value to @ListHsms@
-- to retrieve the next set of items.
--
-- 'hsmList', 'listHsmsResponse_hsmList' - The list of ARNs that identify the HSMs.
--
-- 'httpStatus', 'listHsmsResponse_httpStatus' - The response's http status code.
newListHsmsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListHsmsResponse
newListHsmsResponse pHttpStatus_ =
  ListHsmsResponse'
    { nextToken = Prelude.Nothing,
      hsmList = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If not null, more results are available. Pass this value to @ListHsms@
-- to retrieve the next set of items.
listHsmsResponse_nextToken :: Lens.Lens' ListHsmsResponse (Prelude.Maybe Prelude.Text)
listHsmsResponse_nextToken = Lens.lens (\ListHsmsResponse' {nextToken} -> nextToken) (\s@ListHsmsResponse' {} a -> s {nextToken = a} :: ListHsmsResponse)

-- | The list of ARNs that identify the HSMs.
listHsmsResponse_hsmList :: Lens.Lens' ListHsmsResponse (Prelude.Maybe [Prelude.Text])
listHsmsResponse_hsmList = Lens.lens (\ListHsmsResponse' {hsmList} -> hsmList) (\s@ListHsmsResponse' {} a -> s {hsmList = a} :: ListHsmsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listHsmsResponse_httpStatus :: Lens.Lens' ListHsmsResponse Prelude.Int
listHsmsResponse_httpStatus = Lens.lens (\ListHsmsResponse' {httpStatus} -> httpStatus) (\s@ListHsmsResponse' {} a -> s {httpStatus = a} :: ListHsmsResponse)

instance Prelude.NFData ListHsmsResponse where
  rnf ListHsmsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf hsmList
      `Prelude.seq` Prelude.rnf httpStatus
