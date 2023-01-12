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
-- Module      : Amazonka.Polly.ListLexicons
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of pronunciation lexicons stored in an Amazon Web
-- Services Region. For more information, see
-- <https://docs.aws.amazon.com/polly/latest/dg/managing-lexicons.html Managing Lexicons>.
--
-- This operation returns paginated results.
module Amazonka.Polly.ListLexicons
  ( -- * Creating a Request
    ListLexicons (..),
    newListLexicons,

    -- * Request Lenses
    listLexicons_nextToken,

    -- * Destructuring the Response
    ListLexiconsResponse (..),
    newListLexiconsResponse,

    -- * Response Lenses
    listLexiconsResponse_lexicons,
    listLexiconsResponse_nextToken,
    listLexiconsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Polly.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListLexiconsResponse'
            Prelude.<$> (x Data..?> "Lexicons" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListLexicons where
  hashWithSalt _salt ListLexicons' {..} =
    _salt `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListLexicons where
  rnf ListLexicons' {..} = Prelude.rnf nextToken

instance Data.ToHeaders ListLexicons where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListLexicons where
  toPath = Prelude.const "/v1/lexicons"

instance Data.ToQuery ListLexicons where
  toQuery ListLexicons' {..} =
    Prelude.mconcat ["NextToken" Data.=: nextToken]

-- | /See:/ 'newListLexiconsResponse' smart constructor.
data ListLexiconsResponse = ListLexiconsResponse'
  { -- | A list of lexicon names and attributes.
    lexicons :: Prelude.Maybe [LexiconDescription],
    -- | The pagination token to use in the next request to continue the listing
    -- of lexicons. @NextToken@ is returned only if the response is truncated.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'lexicons', 'listLexiconsResponse_lexicons' - A list of lexicon names and attributes.
--
-- 'nextToken', 'listLexiconsResponse_nextToken' - The pagination token to use in the next request to continue the listing
-- of lexicons. @NextToken@ is returned only if the response is truncated.
--
-- 'httpStatus', 'listLexiconsResponse_httpStatus' - The response's http status code.
newListLexiconsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListLexiconsResponse
newListLexiconsResponse pHttpStatus_ =
  ListLexiconsResponse'
    { lexicons = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of lexicon names and attributes.
listLexiconsResponse_lexicons :: Lens.Lens' ListLexiconsResponse (Prelude.Maybe [LexiconDescription])
listLexiconsResponse_lexicons = Lens.lens (\ListLexiconsResponse' {lexicons} -> lexicons) (\s@ListLexiconsResponse' {} a -> s {lexicons = a} :: ListLexiconsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The pagination token to use in the next request to continue the listing
-- of lexicons. @NextToken@ is returned only if the response is truncated.
listLexiconsResponse_nextToken :: Lens.Lens' ListLexiconsResponse (Prelude.Maybe Prelude.Text)
listLexiconsResponse_nextToken = Lens.lens (\ListLexiconsResponse' {nextToken} -> nextToken) (\s@ListLexiconsResponse' {} a -> s {nextToken = a} :: ListLexiconsResponse)

-- | The response's http status code.
listLexiconsResponse_httpStatus :: Lens.Lens' ListLexiconsResponse Prelude.Int
listLexiconsResponse_httpStatus = Lens.lens (\ListLexiconsResponse' {httpStatus} -> httpStatus) (\s@ListLexiconsResponse' {} a -> s {httpStatus = a} :: ListLexiconsResponse)

instance Prelude.NFData ListLexiconsResponse where
  rnf ListLexiconsResponse' {..} =
    Prelude.rnf lexicons
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
