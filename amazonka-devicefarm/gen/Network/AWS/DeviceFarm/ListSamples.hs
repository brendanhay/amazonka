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
-- Module      : Network.AWS.DeviceFarm.ListSamples
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about samples, given an AWS Device Farm job ARN.
--
-- This operation returns paginated results.
module Network.AWS.DeviceFarm.ListSamples
  ( -- * Creating a Request
    ListSamples (..),
    newListSamples,

    -- * Request Lenses
    listSamples_nextToken,
    listSamples_arn,

    -- * Destructuring the Response
    ListSamplesResponse (..),
    newListSamplesResponse,

    -- * Response Lenses
    listSamplesResponse_nextToken,
    listSamplesResponse_samples,
    listSamplesResponse_httpStatus,
  )
where

import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents a request to the list samples operation.
--
-- /See:/ 'newListSamples' smart constructor.
data ListSamples = ListSamples'
  { -- | An identifier that was returned from the previous call to this
    -- operation, which can be used to return the next set of items in the
    -- list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the job used to list samples.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListSamples' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSamples_nextToken' - An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
--
-- 'arn', 'listSamples_arn' - The Amazon Resource Name (ARN) of the job used to list samples.
newListSamples ::
  -- | 'arn'
  Prelude.Text ->
  ListSamples
newListSamples pArn_ =
  ListSamples'
    { nextToken = Prelude.Nothing,
      arn = pArn_
    }

-- | An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
listSamples_nextToken :: Lens.Lens' ListSamples (Prelude.Maybe Prelude.Text)
listSamples_nextToken = Lens.lens (\ListSamples' {nextToken} -> nextToken) (\s@ListSamples' {} a -> s {nextToken = a} :: ListSamples)

-- | The Amazon Resource Name (ARN) of the job used to list samples.
listSamples_arn :: Lens.Lens' ListSamples Prelude.Text
listSamples_arn = Lens.lens (\ListSamples' {arn} -> arn) (\s@ListSamples' {} a -> s {arn = a} :: ListSamples)

instance Pager.AWSPager ListSamples where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? listSamplesResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? listSamplesResponse_samples Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& listSamples_nextToken
          Lens..~ rs
          Lens.^? listSamplesResponse_nextToken Prelude.. Lens._Just

instance Prelude.AWSRequest ListSamples where
  type Rs ListSamples = ListSamplesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSamplesResponse'
            Prelude.<$> (x Prelude..?> "nextToken")
            Prelude.<*> (x Prelude..?> "samples" Prelude..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListSamples

instance Prelude.NFData ListSamples

instance Prelude.ToHeaders ListSamples where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "DeviceFarm_20150623.ListSamples" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ListSamples where
  toJSON ListSamples' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("nextToken" Prelude..=) Prelude.<$> nextToken,
            Prelude.Just ("arn" Prelude..= arn)
          ]
      )

instance Prelude.ToPath ListSamples where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ListSamples where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the result of a list samples request.
--
-- /See:/ 'newListSamplesResponse' smart constructor.
data ListSamplesResponse = ListSamplesResponse'
  { -- | If the number of items that are returned is significantly large, this is
    -- an identifier that is also returned. It can be used in a subsequent call
    -- to this operation to return the next set of items in the list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the samples.
    samples :: Prelude.Maybe [Sample],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListSamplesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSamplesResponse_nextToken' - If the number of items that are returned is significantly large, this is
-- an identifier that is also returned. It can be used in a subsequent call
-- to this operation to return the next set of items in the list.
--
-- 'samples', 'listSamplesResponse_samples' - Information about the samples.
--
-- 'httpStatus', 'listSamplesResponse_httpStatus' - The response's http status code.
newListSamplesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListSamplesResponse
newListSamplesResponse pHttpStatus_ =
  ListSamplesResponse'
    { nextToken = Prelude.Nothing,
      samples = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the number of items that are returned is significantly large, this is
-- an identifier that is also returned. It can be used in a subsequent call
-- to this operation to return the next set of items in the list.
listSamplesResponse_nextToken :: Lens.Lens' ListSamplesResponse (Prelude.Maybe Prelude.Text)
listSamplesResponse_nextToken = Lens.lens (\ListSamplesResponse' {nextToken} -> nextToken) (\s@ListSamplesResponse' {} a -> s {nextToken = a} :: ListSamplesResponse)

-- | Information about the samples.
listSamplesResponse_samples :: Lens.Lens' ListSamplesResponse (Prelude.Maybe [Sample])
listSamplesResponse_samples = Lens.lens (\ListSamplesResponse' {samples} -> samples) (\s@ListSamplesResponse' {} a -> s {samples = a} :: ListSamplesResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
listSamplesResponse_httpStatus :: Lens.Lens' ListSamplesResponse Prelude.Int
listSamplesResponse_httpStatus = Lens.lens (\ListSamplesResponse' {httpStatus} -> httpStatus) (\s@ListSamplesResponse' {} a -> s {httpStatus = a} :: ListSamplesResponse)

instance Prelude.NFData ListSamplesResponse
