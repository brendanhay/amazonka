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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListHsms' smart constructor.
data ListHsms = ListHsms'
  { -- | The @NextToken@ value from a previous call to @ListHsms@. Pass null if
    -- this is the first call.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Pager.AWSPager ListHsms where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? listHsmsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? listHsmsResponse_hsmList Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& listHsms_nextToken
          Lens..~ rs
          Lens.^? listHsmsResponse_nextToken Prelude.. Lens._Just

instance Prelude.AWSRequest ListHsms where
  type Rs ListHsms = ListHsmsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListHsmsResponse'
            Prelude.<$> (x Prelude..?> "HsmList" Prelude..!@ Prelude.mempty)
            Prelude.<*> (x Prelude..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListHsms

instance Prelude.NFData ListHsms

instance Prelude.ToHeaders ListHsms where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "CloudHsmFrontendService.ListHsms" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ListHsms where
  toJSON ListHsms' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("NextToken" Prelude..=) Prelude.<$> nextToken]
      )

instance Prelude.ToPath ListHsms where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ListHsms where
  toQuery = Prelude.const Prelude.mempty

-- | Contains the output of the @ListHsms@ operation.
--
-- /See:/ 'newListHsmsResponse' smart constructor.
data ListHsmsResponse = ListHsmsResponse'
  { -- | The list of ARNs that identify the HSMs.
    hsmList :: Prelude.Maybe [Prelude.Text],
    -- | If not null, more results are available. Pass this value to @ListHsms@
    -- to retrieve the next set of items.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  ListHsmsResponse
newListHsmsResponse pHttpStatus_ =
  ListHsmsResponse'
    { hsmList = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of ARNs that identify the HSMs.
listHsmsResponse_hsmList :: Lens.Lens' ListHsmsResponse (Prelude.Maybe [Prelude.Text])
listHsmsResponse_hsmList = Lens.lens (\ListHsmsResponse' {hsmList} -> hsmList) (\s@ListHsmsResponse' {} a -> s {hsmList = a} :: ListHsmsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | If not null, more results are available. Pass this value to @ListHsms@
-- to retrieve the next set of items.
listHsmsResponse_nextToken :: Lens.Lens' ListHsmsResponse (Prelude.Maybe Prelude.Text)
listHsmsResponse_nextToken = Lens.lens (\ListHsmsResponse' {nextToken} -> nextToken) (\s@ListHsmsResponse' {} a -> s {nextToken = a} :: ListHsmsResponse)

-- | The response's http status code.
listHsmsResponse_httpStatus :: Lens.Lens' ListHsmsResponse Prelude.Int
listHsmsResponse_httpStatus = Lens.lens (\ListHsmsResponse' {httpStatus} -> httpStatus) (\s@ListHsmsResponse' {} a -> s {httpStatus = a} :: ListHsmsResponse)

instance Prelude.NFData ListHsmsResponse
