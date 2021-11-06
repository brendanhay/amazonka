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
-- Module      : Amazonka.Shield.ListProtections
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all Protection objects for the account.
--
-- This operation returns paginated results.
module Amazonka.Shield.ListProtections
  ( -- * Creating a Request
    ListProtections (..),
    newListProtections,

    -- * Request Lenses
    listProtections_nextToken,
    listProtections_maxResults,

    -- * Destructuring the Response
    ListProtectionsResponse (..),
    newListProtectionsResponse,

    -- * Response Lenses
    listProtectionsResponse_protections,
    listProtectionsResponse_nextToken,
    listProtectionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Shield.Types

-- | /See:/ 'newListProtections' smart constructor.
data ListProtections = ListProtections'
  { -- | The @ListProtectionsRequest.NextToken@ value from a previous call to
    -- @ListProtections@. Pass null if this is the first call.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of Protection objects to return. If you leave this
    -- blank, Shield Advanced returns the first 20 results.
    --
    -- This is a maximum value. Shield Advanced might return the results in
    -- smaller batches. That is, the number of objects returned could be less
    -- than @MaxResults@, even if there are still more objects yet to return.
    -- If there are more objects to return, Shield Advanced returns a value in
    -- @NextToken@ that you can use in your next request, to get the next batch
    -- of objects.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListProtections' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listProtections_nextToken' - The @ListProtectionsRequest.NextToken@ value from a previous call to
-- @ListProtections@. Pass null if this is the first call.
--
-- 'maxResults', 'listProtections_maxResults' - The maximum number of Protection objects to return. If you leave this
-- blank, Shield Advanced returns the first 20 results.
--
-- This is a maximum value. Shield Advanced might return the results in
-- smaller batches. That is, the number of objects returned could be less
-- than @MaxResults@, even if there are still more objects yet to return.
-- If there are more objects to return, Shield Advanced returns a value in
-- @NextToken@ that you can use in your next request, to get the next batch
-- of objects.
newListProtections ::
  ListProtections
newListProtections =
  ListProtections'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The @ListProtectionsRequest.NextToken@ value from a previous call to
-- @ListProtections@. Pass null if this is the first call.
listProtections_nextToken :: Lens.Lens' ListProtections (Prelude.Maybe Prelude.Text)
listProtections_nextToken = Lens.lens (\ListProtections' {nextToken} -> nextToken) (\s@ListProtections' {} a -> s {nextToken = a} :: ListProtections)

-- | The maximum number of Protection objects to return. If you leave this
-- blank, Shield Advanced returns the first 20 results.
--
-- This is a maximum value. Shield Advanced might return the results in
-- smaller batches. That is, the number of objects returned could be less
-- than @MaxResults@, even if there are still more objects yet to return.
-- If there are more objects to return, Shield Advanced returns a value in
-- @NextToken@ that you can use in your next request, to get the next batch
-- of objects.
listProtections_maxResults :: Lens.Lens' ListProtections (Prelude.Maybe Prelude.Natural)
listProtections_maxResults = Lens.lens (\ListProtections' {maxResults} -> maxResults) (\s@ListProtections' {} a -> s {maxResults = a} :: ListProtections)

instance Core.AWSPager ListProtections where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listProtectionsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listProtectionsResponse_protections
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listProtections_nextToken
          Lens..~ rs
          Lens.^? listProtectionsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListProtections where
  type
    AWSResponse ListProtections =
      ListProtectionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListProtectionsResponse'
            Prelude.<$> (x Core..?> "Protections" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListProtections

instance Prelude.NFData ListProtections

instance Core.ToHeaders ListProtections where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSShield_20160616.ListProtections" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListProtections where
  toJSON ListProtections' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListProtections where
  toPath = Prelude.const "/"

instance Core.ToQuery ListProtections where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListProtectionsResponse' smart constructor.
data ListProtectionsResponse = ListProtectionsResponse'
  { -- | The array of enabled Protection objects.
    protections :: Prelude.Maybe [Protection],
    -- | If you specify a value for @MaxResults@ and you have more Protections
    -- than the value of MaxResults, Shield Advanced returns a NextToken value
    -- in the response that allows you to list another group of Protections.
    -- For the second and subsequent ListProtections requests, specify the
    -- value of NextToken from the previous response to get information about
    -- another batch of Protections.
    --
    -- Shield Advanced might return the list of Protection objects in batches
    -- smaller than the number specified by MaxResults. If there are more
    -- Protection objects to return, Shield Advanced will always also return a
    -- @NextToken@.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListProtectionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'protections', 'listProtectionsResponse_protections' - The array of enabled Protection objects.
--
-- 'nextToken', 'listProtectionsResponse_nextToken' - If you specify a value for @MaxResults@ and you have more Protections
-- than the value of MaxResults, Shield Advanced returns a NextToken value
-- in the response that allows you to list another group of Protections.
-- For the second and subsequent ListProtections requests, specify the
-- value of NextToken from the previous response to get information about
-- another batch of Protections.
--
-- Shield Advanced might return the list of Protection objects in batches
-- smaller than the number specified by MaxResults. If there are more
-- Protection objects to return, Shield Advanced will always also return a
-- @NextToken@.
--
-- 'httpStatus', 'listProtectionsResponse_httpStatus' - The response's http status code.
newListProtectionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListProtectionsResponse
newListProtectionsResponse pHttpStatus_ =
  ListProtectionsResponse'
    { protections =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The array of enabled Protection objects.
listProtectionsResponse_protections :: Lens.Lens' ListProtectionsResponse (Prelude.Maybe [Protection])
listProtectionsResponse_protections = Lens.lens (\ListProtectionsResponse' {protections} -> protections) (\s@ListProtectionsResponse' {} a -> s {protections = a} :: ListProtectionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | If you specify a value for @MaxResults@ and you have more Protections
-- than the value of MaxResults, Shield Advanced returns a NextToken value
-- in the response that allows you to list another group of Protections.
-- For the second and subsequent ListProtections requests, specify the
-- value of NextToken from the previous response to get information about
-- another batch of Protections.
--
-- Shield Advanced might return the list of Protection objects in batches
-- smaller than the number specified by MaxResults. If there are more
-- Protection objects to return, Shield Advanced will always also return a
-- @NextToken@.
listProtectionsResponse_nextToken :: Lens.Lens' ListProtectionsResponse (Prelude.Maybe Prelude.Text)
listProtectionsResponse_nextToken = Lens.lens (\ListProtectionsResponse' {nextToken} -> nextToken) (\s@ListProtectionsResponse' {} a -> s {nextToken = a} :: ListProtectionsResponse)

-- | The response's http status code.
listProtectionsResponse_httpStatus :: Lens.Lens' ListProtectionsResponse Prelude.Int
listProtectionsResponse_httpStatus = Lens.lens (\ListProtectionsResponse' {httpStatus} -> httpStatus) (\s@ListProtectionsResponse' {} a -> s {httpStatus = a} :: ListProtectionsResponse)

instance Prelude.NFData ListProtectionsResponse
