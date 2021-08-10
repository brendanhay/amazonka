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
-- Module      : Network.AWS.DeviceFarm.ListTests
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about tests in a given test suite.
--
-- This operation returns paginated results.
module Network.AWS.DeviceFarm.ListTests
  ( -- * Creating a Request
    ListTests (..),
    newListTests,

    -- * Request Lenses
    listTests_nextToken,
    listTests_arn,

    -- * Destructuring the Response
    ListTestsResponse (..),
    newListTestsResponse,

    -- * Response Lenses
    listTestsResponse_nextToken,
    listTestsResponse_tests,
    listTestsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents a request to the list tests operation.
--
-- /See:/ 'newListTests' smart constructor.
data ListTests = ListTests'
  { -- | An identifier that was returned from the previous call to this
    -- operation, which can be used to return the next set of items in the
    -- list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The test suite\'s Amazon Resource Name (ARN).
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTests' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTests_nextToken' - An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
--
-- 'arn', 'listTests_arn' - The test suite\'s Amazon Resource Name (ARN).
newListTests ::
  -- | 'arn'
  Prelude.Text ->
  ListTests
newListTests pArn_ =
  ListTests'
    { nextToken = Prelude.Nothing,
      arn = pArn_
    }

-- | An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
listTests_nextToken :: Lens.Lens' ListTests (Prelude.Maybe Prelude.Text)
listTests_nextToken = Lens.lens (\ListTests' {nextToken} -> nextToken) (\s@ListTests' {} a -> s {nextToken = a} :: ListTests)

-- | The test suite\'s Amazon Resource Name (ARN).
listTests_arn :: Lens.Lens' ListTests Prelude.Text
listTests_arn = Lens.lens (\ListTests' {arn} -> arn) (\s@ListTests' {} a -> s {arn = a} :: ListTests)

instance Core.AWSPager ListTests where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listTestsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listTestsResponse_tests Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listTests_nextToken
          Lens..~ rs
          Lens.^? listTestsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListTests where
  type AWSResponse ListTests = ListTestsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTestsResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "tests" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListTests

instance Prelude.NFData ListTests

instance Core.ToHeaders ListTests where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DeviceFarm_20150623.ListTests" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListTests where
  toJSON ListTests' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("nextToken" Core..=) Prelude.<$> nextToken,
            Prelude.Just ("arn" Core..= arn)
          ]
      )

instance Core.ToPath ListTests where
  toPath = Prelude.const "/"

instance Core.ToQuery ListTests where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the result of a list tests request.
--
-- /See:/ 'newListTestsResponse' smart constructor.
data ListTestsResponse = ListTestsResponse'
  { -- | If the number of items that are returned is significantly large, this is
    -- an identifier that is also returned. It can be used in a subsequent call
    -- to this operation to return the next set of items in the list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the tests.
    tests :: Prelude.Maybe [Test],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTestsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTestsResponse_nextToken' - If the number of items that are returned is significantly large, this is
-- an identifier that is also returned. It can be used in a subsequent call
-- to this operation to return the next set of items in the list.
--
-- 'tests', 'listTestsResponse_tests' - Information about the tests.
--
-- 'httpStatus', 'listTestsResponse_httpStatus' - The response's http status code.
newListTestsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListTestsResponse
newListTestsResponse pHttpStatus_ =
  ListTestsResponse'
    { nextToken = Prelude.Nothing,
      tests = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the number of items that are returned is significantly large, this is
-- an identifier that is also returned. It can be used in a subsequent call
-- to this operation to return the next set of items in the list.
listTestsResponse_nextToken :: Lens.Lens' ListTestsResponse (Prelude.Maybe Prelude.Text)
listTestsResponse_nextToken = Lens.lens (\ListTestsResponse' {nextToken} -> nextToken) (\s@ListTestsResponse' {} a -> s {nextToken = a} :: ListTestsResponse)

-- | Information about the tests.
listTestsResponse_tests :: Lens.Lens' ListTestsResponse (Prelude.Maybe [Test])
listTestsResponse_tests = Lens.lens (\ListTestsResponse' {tests} -> tests) (\s@ListTestsResponse' {} a -> s {tests = a} :: ListTestsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listTestsResponse_httpStatus :: Lens.Lens' ListTestsResponse Prelude.Int
listTestsResponse_httpStatus = Lens.lens (\ListTestsResponse' {httpStatus} -> httpStatus) (\s@ListTestsResponse' {} a -> s {httpStatus = a} :: ListTestsResponse)

instance Prelude.NFData ListTestsResponse
