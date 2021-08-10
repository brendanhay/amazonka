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
-- Module      : Network.AWS.Polly.ListLexicons
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of pronunciation lexicons stored in an AWS Region. For
-- more information, see
-- <https://docs.aws.amazon.com/polly/latest/dg/managing-lexicons.html Managing Lexicons>.
--
-- This operation returns paginated results.
module Network.AWS.Polly.ListLexicons
  ( -- * Creating a Request
    ListLexicons (..),
    newListLexicons,

    -- * Request Lenses
    listLexicons_nextToken,

    -- * Destructuring the Response
    ListLexiconsResponse (..),
    newListLexiconsResponse,

    -- * Response Lenses
    listLexiconsResponse_nextToken,
    listLexiconsResponse_lexicons,
    listLexiconsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Polly.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListLexicons' smart constructor.
data ListLexicons = ListLexicons'
  { -- | An opaque pagination token returned from previous @ListLexicons@
    -- operation. If present, indicates where to continue the list of lexicons.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListLexicons' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listLexicons_nextToken' - An opaque pagination token returned from previous @ListLexicons@
-- operation. If present, indicates where to continue the list of lexicons.
newListLexicons ::
  ListLexicons
newListLexicons =
  ListLexicons' {nextToken = Prelude.Nothing}

-- | An opaque pagination token returned from previous @ListLexicons@
-- operation. If present, indicates where to continue the list of lexicons.
listLexicons_nextToken :: Lens.Lens' ListLexicons (Prelude.Maybe Prelude.Text)
listLexicons_nextToken = Lens.lens (\ListLexicons' {nextToken} -> nextToken) (\s@ListLexicons' {} a -> s {nextToken = a} :: ListLexicons)

instance Core.AWSPager ListLexicons where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listLexiconsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listLexiconsResponse_lexicons Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listLexicons_nextToken
          Lens..~ rs
          Lens.^? listLexiconsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListLexicons where
  type AWSResponse ListLexicons = ListLexiconsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListLexiconsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "Lexicons" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListLexicons

instance Prelude.NFData ListLexicons

instance Core.ToHeaders ListLexicons where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListLexicons where
  toPath = Prelude.const "/v1/lexicons"

instance Core.ToQuery ListLexicons where
  toQuery ListLexicons' {..} =
    Prelude.mconcat ["NextToken" Core.=: nextToken]

-- | /See:/ 'newListLexiconsResponse' smart constructor.
data ListLexiconsResponse = ListLexiconsResponse'
  { -- | The pagination token to use in the next request to continue the listing
    -- of lexicons. @NextToken@ is returned only if the response is truncated.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of lexicon names and attributes.
    lexicons :: Prelude.Maybe [LexiconDescription],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListLexiconsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listLexiconsResponse_nextToken' - The pagination token to use in the next request to continue the listing
-- of lexicons. @NextToken@ is returned only if the response is truncated.
--
-- 'lexicons', 'listLexiconsResponse_lexicons' - A list of lexicon names and attributes.
--
-- 'httpStatus', 'listLexiconsResponse_httpStatus' - The response's http status code.
newListLexiconsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListLexiconsResponse
newListLexiconsResponse pHttpStatus_ =
  ListLexiconsResponse'
    { nextToken = Prelude.Nothing,
      lexicons = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The pagination token to use in the next request to continue the listing
-- of lexicons. @NextToken@ is returned only if the response is truncated.
listLexiconsResponse_nextToken :: Lens.Lens' ListLexiconsResponse (Prelude.Maybe Prelude.Text)
listLexiconsResponse_nextToken = Lens.lens (\ListLexiconsResponse' {nextToken} -> nextToken) (\s@ListLexiconsResponse' {} a -> s {nextToken = a} :: ListLexiconsResponse)

-- | A list of lexicon names and attributes.
listLexiconsResponse_lexicons :: Lens.Lens' ListLexiconsResponse (Prelude.Maybe [LexiconDescription])
listLexiconsResponse_lexicons = Lens.lens (\ListLexiconsResponse' {lexicons} -> lexicons) (\s@ListLexiconsResponse' {} a -> s {lexicons = a} :: ListLexiconsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listLexiconsResponse_httpStatus :: Lens.Lens' ListLexiconsResponse Prelude.Int
listLexiconsResponse_httpStatus = Lens.lens (\ListLexiconsResponse' {httpStatus} -> httpStatus) (\s@ListLexiconsResponse' {} a -> s {httpStatus = a} :: ListLexiconsResponse)

instance Prelude.NFData ListLexiconsResponse
