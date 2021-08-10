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
-- Module      : Network.AWS.CloudFormation.ListExports
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all exported output values in the account and Region in which you
-- call this action. Use this action to see the exported output values that
-- you can import into other stacks. To import values, use the
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/intrinsic-function-reference-importvalue.html Fn::ImportValue>
-- function.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-exports.html AWS CloudFormation Export Stack Output Values>.
--
-- This operation returns paginated results.
module Network.AWS.CloudFormation.ListExports
  ( -- * Creating a Request
    ListExports (..),
    newListExports,

    -- * Request Lenses
    listExports_nextToken,

    -- * Destructuring the Response
    ListExportsResponse (..),
    newListExportsResponse,

    -- * Response Lenses
    listExportsResponse_exports,
    listExportsResponse_nextToken,
    listExportsResponse_httpStatus,
  )
where

import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListExports' smart constructor.
data ListExports = ListExports'
  { -- | A string (provided by the ListExports response output) that identifies
    -- the next page of exported output values that you asked to retrieve.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListExports' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listExports_nextToken' - A string (provided by the ListExports response output) that identifies
-- the next page of exported output values that you asked to retrieve.
newListExports ::
  ListExports
newListExports =
  ListExports' {nextToken = Prelude.Nothing}

-- | A string (provided by the ListExports response output) that identifies
-- the next page of exported output values that you asked to retrieve.
listExports_nextToken :: Lens.Lens' ListExports (Prelude.Maybe Prelude.Text)
listExports_nextToken = Lens.lens (\ListExports' {nextToken} -> nextToken) (\s@ListExports' {} a -> s {nextToken = a} :: ListExports)

instance Core.AWSPager ListExports where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listExportsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listExportsResponse_exports Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listExports_nextToken
          Lens..~ rs
          Lens.^? listExportsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListExports where
  type AWSResponse ListExports = ListExportsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ListExportsResult"
      ( \s h x ->
          ListExportsResponse'
            Prelude.<$> ( x Core..@? "Exports" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "member")
                        )
            Prelude.<*> (x Core..@? "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListExports

instance Prelude.NFData ListExports

instance Core.ToHeaders ListExports where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListExports where
  toPath = Prelude.const "/"

instance Core.ToQuery ListExports where
  toQuery ListExports' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("ListExports" :: Prelude.ByteString),
        "Version"
          Core.=: ("2010-05-15" :: Prelude.ByteString),
        "NextToken" Core.=: nextToken
      ]

-- | /See:/ 'newListExportsResponse' smart constructor.
data ListExportsResponse = ListExportsResponse'
  { -- | The output for the ListExports action.
    exports :: Prelude.Maybe [Export],
    -- | If the output exceeds 100 exported output values, a string that
    -- identifies the next page of exports. If there is no additional page,
    -- this value is null.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListExportsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'exports', 'listExportsResponse_exports' - The output for the ListExports action.
--
-- 'nextToken', 'listExportsResponse_nextToken' - If the output exceeds 100 exported output values, a string that
-- identifies the next page of exports. If there is no additional page,
-- this value is null.
--
-- 'httpStatus', 'listExportsResponse_httpStatus' - The response's http status code.
newListExportsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListExportsResponse
newListExportsResponse pHttpStatus_ =
  ListExportsResponse'
    { exports = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The output for the ListExports action.
listExportsResponse_exports :: Lens.Lens' ListExportsResponse (Prelude.Maybe [Export])
listExportsResponse_exports = Lens.lens (\ListExportsResponse' {exports} -> exports) (\s@ListExportsResponse' {} a -> s {exports = a} :: ListExportsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | If the output exceeds 100 exported output values, a string that
-- identifies the next page of exports. If there is no additional page,
-- this value is null.
listExportsResponse_nextToken :: Lens.Lens' ListExportsResponse (Prelude.Maybe Prelude.Text)
listExportsResponse_nextToken = Lens.lens (\ListExportsResponse' {nextToken} -> nextToken) (\s@ListExportsResponse' {} a -> s {nextToken = a} :: ListExportsResponse)

-- | The response's http status code.
listExportsResponse_httpStatus :: Lens.Lens' ListExportsResponse Prelude.Int
listExportsResponse_httpStatus = Lens.lens (\ListExportsResponse' {httpStatus} -> httpStatus) (\s@ListExportsResponse' {} a -> s {httpStatus = a} :: ListExportsResponse)

instance Prelude.NFData ListExportsResponse
