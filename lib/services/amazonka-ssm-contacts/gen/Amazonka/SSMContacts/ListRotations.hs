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
-- Module      : Amazonka.SSMContacts.ListRotations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of on-call rotations.
--
-- This operation returns paginated results.
module Amazonka.SSMContacts.ListRotations
  ( -- * Creating a Request
    ListRotations (..),
    newListRotations,

    -- * Request Lenses
    listRotations_maxResults,
    listRotations_nextToken,
    listRotations_rotationNamePrefix,

    -- * Destructuring the Response
    ListRotationsResponse (..),
    newListRotationsResponse,

    -- * Response Lenses
    listRotationsResponse_nextToken,
    listRotationsResponse_httpStatus,
    listRotationsResponse_rotations,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSMContacts.Types

-- | /See:/ 'newListRotations' smart constructor.
data ListRotations = ListRotations'
  { -- | The maximum number of items to return for this call. The call also
    -- returns a token that you can specify in a subsequent call to get the
    -- next set of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A token to start the list. Use this token to get the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A filter to include rotations in list results based on their common
    -- prefix. For example, entering prod returns a list of all rotation names
    -- that begin with @prod@, such as @production@ and @prod-1@.
    rotationNamePrefix :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRotations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listRotations_maxResults' - The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
--
-- 'nextToken', 'listRotations_nextToken' - A token to start the list. Use this token to get the next set of
-- results.
--
-- 'rotationNamePrefix', 'listRotations_rotationNamePrefix' - A filter to include rotations in list results based on their common
-- prefix. For example, entering prod returns a list of all rotation names
-- that begin with @prod@, such as @production@ and @prod-1@.
newListRotations ::
  ListRotations
newListRotations =
  ListRotations'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      rotationNamePrefix = Prelude.Nothing
    }

-- | The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
listRotations_maxResults :: Lens.Lens' ListRotations (Prelude.Maybe Prelude.Natural)
listRotations_maxResults = Lens.lens (\ListRotations' {maxResults} -> maxResults) (\s@ListRotations' {} a -> s {maxResults = a} :: ListRotations)

-- | A token to start the list. Use this token to get the next set of
-- results.
listRotations_nextToken :: Lens.Lens' ListRotations (Prelude.Maybe Prelude.Text)
listRotations_nextToken = Lens.lens (\ListRotations' {nextToken} -> nextToken) (\s@ListRotations' {} a -> s {nextToken = a} :: ListRotations)

-- | A filter to include rotations in list results based on their common
-- prefix. For example, entering prod returns a list of all rotation names
-- that begin with @prod@, such as @production@ and @prod-1@.
listRotations_rotationNamePrefix :: Lens.Lens' ListRotations (Prelude.Maybe Prelude.Text)
listRotations_rotationNamePrefix = Lens.lens (\ListRotations' {rotationNamePrefix} -> rotationNamePrefix) (\s@ListRotations' {} a -> s {rotationNamePrefix = a} :: ListRotations)

instance Core.AWSPager ListRotations where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listRotationsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        (rs Lens.^. listRotationsResponse_rotations) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listRotations_nextToken
          Lens..~ rs
          Lens.^? listRotationsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListRotations where
  type
    AWSResponse ListRotations =
      ListRotationsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListRotationsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "Rotations" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListRotations where
  hashWithSalt _salt ListRotations' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` rotationNamePrefix

instance Prelude.NFData ListRotations where
  rnf ListRotations' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf rotationNamePrefix

instance Data.ToHeaders ListRotations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("SSMContacts.ListRotations" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListRotations where
  toJSON ListRotations' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("RotationNamePrefix" Data..=)
              Prelude.<$> rotationNamePrefix
          ]
      )

instance Data.ToPath ListRotations where
  toPath = Prelude.const "/"

instance Data.ToQuery ListRotations where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListRotationsResponse' smart constructor.
data ListRotationsResponse = ListRotationsResponse'
  { -- | The token for the next set of items to return. Use this token to get the
    -- next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Information about rotations that meet the filter criteria.
    rotations :: [Rotation]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRotationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listRotationsResponse_nextToken' - The token for the next set of items to return. Use this token to get the
-- next set of results.
--
-- 'httpStatus', 'listRotationsResponse_httpStatus' - The response's http status code.
--
-- 'rotations', 'listRotationsResponse_rotations' - Information about rotations that meet the filter criteria.
newListRotationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListRotationsResponse
newListRotationsResponse pHttpStatus_ =
  ListRotationsResponse'
    { nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      rotations = Prelude.mempty
    }

-- | The token for the next set of items to return. Use this token to get the
-- next set of results.
listRotationsResponse_nextToken :: Lens.Lens' ListRotationsResponse (Prelude.Maybe Prelude.Text)
listRotationsResponse_nextToken = Lens.lens (\ListRotationsResponse' {nextToken} -> nextToken) (\s@ListRotationsResponse' {} a -> s {nextToken = a} :: ListRotationsResponse)

-- | The response's http status code.
listRotationsResponse_httpStatus :: Lens.Lens' ListRotationsResponse Prelude.Int
listRotationsResponse_httpStatus = Lens.lens (\ListRotationsResponse' {httpStatus} -> httpStatus) (\s@ListRotationsResponse' {} a -> s {httpStatus = a} :: ListRotationsResponse)

-- | Information about rotations that meet the filter criteria.
listRotationsResponse_rotations :: Lens.Lens' ListRotationsResponse [Rotation]
listRotationsResponse_rotations = Lens.lens (\ListRotationsResponse' {rotations} -> rotations) (\s@ListRotationsResponse' {} a -> s {rotations = a} :: ListRotationsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListRotationsResponse where
  rnf ListRotationsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf rotations
