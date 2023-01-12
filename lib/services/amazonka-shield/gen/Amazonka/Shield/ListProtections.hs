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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves Protection objects for the account. You can retrieve all
-- protections or you can provide filtering criteria and retrieve just the
-- subset of protections that match the criteria.
--
-- This operation returns paginated results.
module Amazonka.Shield.ListProtections
  ( -- * Creating a Request
    ListProtections (..),
    newListProtections,

    -- * Request Lenses
    listProtections_inclusionFilters,
    listProtections_maxResults,
    listProtections_nextToken,

    -- * Destructuring the Response
    ListProtectionsResponse (..),
    newListProtectionsResponse,

    -- * Response Lenses
    listProtectionsResponse_nextToken,
    listProtectionsResponse_protections,
    listProtectionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Shield.Types

-- | /See:/ 'newListProtections' smart constructor.
data ListProtections = ListProtections'
  { -- | Narrows the set of protections that the call retrieves. You can retrieve
    -- a single protection by providing its name or the ARN (Amazon Resource
    -- Name) of its protected resource. You can also retrieve all protections
    -- for a specific resource type. You can provide up to one criteria per
    -- filter type. Shield Advanced returns protections that exactly match all
    -- of the filter criteria that you provide.
    inclusionFilters :: Prelude.Maybe InclusionProtectionFilters,
    -- | The greatest number of objects that you want Shield Advanced to return
    -- to the list request. Shield Advanced might return fewer objects than you
    -- indicate in this setting, even if more objects are available. If there
    -- are more objects remaining, Shield Advanced will always also return a
    -- @NextToken@ value in the response.
    --
    -- The default setting is 20.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | When you request a list of objects from Shield Advanced, if the response
    -- does not include all of the remaining available objects, Shield Advanced
    -- includes a @NextToken@ value in the response. You can retrieve the next
    -- batch of objects by requesting the list again and providing the token
    -- that was returned by the prior call in your request.
    --
    -- You can indicate the maximum number of objects that you want Shield
    -- Advanced to return for a single call with the @MaxResults@ setting.
    -- Shield Advanced will not return more than @MaxResults@ objects, but may
    -- return fewer, even if more objects are still available.
    --
    -- Whenever more objects remain that Shield Advanced has not yet returned
    -- to you, the response will include a @NextToken@ value.
    --
    -- On your first call to a list operation, leave this setting empty.
    nextToken :: Prelude.Maybe Prelude.Text
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
-- 'inclusionFilters', 'listProtections_inclusionFilters' - Narrows the set of protections that the call retrieves. You can retrieve
-- a single protection by providing its name or the ARN (Amazon Resource
-- Name) of its protected resource. You can also retrieve all protections
-- for a specific resource type. You can provide up to one criteria per
-- filter type. Shield Advanced returns protections that exactly match all
-- of the filter criteria that you provide.
--
-- 'maxResults', 'listProtections_maxResults' - The greatest number of objects that you want Shield Advanced to return
-- to the list request. Shield Advanced might return fewer objects than you
-- indicate in this setting, even if more objects are available. If there
-- are more objects remaining, Shield Advanced will always also return a
-- @NextToken@ value in the response.
--
-- The default setting is 20.
--
-- 'nextToken', 'listProtections_nextToken' - When you request a list of objects from Shield Advanced, if the response
-- does not include all of the remaining available objects, Shield Advanced
-- includes a @NextToken@ value in the response. You can retrieve the next
-- batch of objects by requesting the list again and providing the token
-- that was returned by the prior call in your request.
--
-- You can indicate the maximum number of objects that you want Shield
-- Advanced to return for a single call with the @MaxResults@ setting.
-- Shield Advanced will not return more than @MaxResults@ objects, but may
-- return fewer, even if more objects are still available.
--
-- Whenever more objects remain that Shield Advanced has not yet returned
-- to you, the response will include a @NextToken@ value.
--
-- On your first call to a list operation, leave this setting empty.
newListProtections ::
  ListProtections
newListProtections =
  ListProtections'
    { inclusionFilters =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Narrows the set of protections that the call retrieves. You can retrieve
-- a single protection by providing its name or the ARN (Amazon Resource
-- Name) of its protected resource. You can also retrieve all protections
-- for a specific resource type. You can provide up to one criteria per
-- filter type. Shield Advanced returns protections that exactly match all
-- of the filter criteria that you provide.
listProtections_inclusionFilters :: Lens.Lens' ListProtections (Prelude.Maybe InclusionProtectionFilters)
listProtections_inclusionFilters = Lens.lens (\ListProtections' {inclusionFilters} -> inclusionFilters) (\s@ListProtections' {} a -> s {inclusionFilters = a} :: ListProtections)

-- | The greatest number of objects that you want Shield Advanced to return
-- to the list request. Shield Advanced might return fewer objects than you
-- indicate in this setting, even if more objects are available. If there
-- are more objects remaining, Shield Advanced will always also return a
-- @NextToken@ value in the response.
--
-- The default setting is 20.
listProtections_maxResults :: Lens.Lens' ListProtections (Prelude.Maybe Prelude.Natural)
listProtections_maxResults = Lens.lens (\ListProtections' {maxResults} -> maxResults) (\s@ListProtections' {} a -> s {maxResults = a} :: ListProtections)

-- | When you request a list of objects from Shield Advanced, if the response
-- does not include all of the remaining available objects, Shield Advanced
-- includes a @NextToken@ value in the response. You can retrieve the next
-- batch of objects by requesting the list again and providing the token
-- that was returned by the prior call in your request.
--
-- You can indicate the maximum number of objects that you want Shield
-- Advanced to return for a single call with the @MaxResults@ setting.
-- Shield Advanced will not return more than @MaxResults@ objects, but may
-- return fewer, even if more objects are still available.
--
-- Whenever more objects remain that Shield Advanced has not yet returned
-- to you, the response will include a @NextToken@ value.
--
-- On your first call to a list operation, leave this setting empty.
listProtections_nextToken :: Lens.Lens' ListProtections (Prelude.Maybe Prelude.Text)
listProtections_nextToken = Lens.lens (\ListProtections' {nextToken} -> nextToken) (\s@ListProtections' {} a -> s {nextToken = a} :: ListProtections)

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListProtectionsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Protections" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListProtections where
  hashWithSalt _salt ListProtections' {..} =
    _salt `Prelude.hashWithSalt` inclusionFilters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListProtections where
  rnf ListProtections' {..} =
    Prelude.rnf inclusionFilters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListProtections where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSShield_20160616.ListProtections" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListProtections where
  toJSON ListProtections' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("InclusionFilters" Data..=)
              Prelude.<$> inclusionFilters,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListProtections where
  toPath = Prelude.const "/"

instance Data.ToQuery ListProtections where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListProtectionsResponse' smart constructor.
data ListProtectionsResponse = ListProtectionsResponse'
  { -- | When you request a list of objects from Shield Advanced, if the response
    -- does not include all of the remaining available objects, Shield Advanced
    -- includes a @NextToken@ value in the response. You can retrieve the next
    -- batch of objects by requesting the list again and providing the token
    -- that was returned by the prior call in your request.
    --
    -- You can indicate the maximum number of objects that you want Shield
    -- Advanced to return for a single call with the @MaxResults@ setting.
    -- Shield Advanced will not return more than @MaxResults@ objects, but may
    -- return fewer, even if more objects are still available.
    --
    -- Whenever more objects remain that Shield Advanced has not yet returned
    -- to you, the response will include a @NextToken@ value.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The array of enabled Protection objects.
    protections :: Prelude.Maybe [Protection],
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
-- 'nextToken', 'listProtectionsResponse_nextToken' - When you request a list of objects from Shield Advanced, if the response
-- does not include all of the remaining available objects, Shield Advanced
-- includes a @NextToken@ value in the response. You can retrieve the next
-- batch of objects by requesting the list again and providing the token
-- that was returned by the prior call in your request.
--
-- You can indicate the maximum number of objects that you want Shield
-- Advanced to return for a single call with the @MaxResults@ setting.
-- Shield Advanced will not return more than @MaxResults@ objects, but may
-- return fewer, even if more objects are still available.
--
-- Whenever more objects remain that Shield Advanced has not yet returned
-- to you, the response will include a @NextToken@ value.
--
-- 'protections', 'listProtectionsResponse_protections' - The array of enabled Protection objects.
--
-- 'httpStatus', 'listProtectionsResponse_httpStatus' - The response's http status code.
newListProtectionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListProtectionsResponse
newListProtectionsResponse pHttpStatus_ =
  ListProtectionsResponse'
    { nextToken =
        Prelude.Nothing,
      protections = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | When you request a list of objects from Shield Advanced, if the response
-- does not include all of the remaining available objects, Shield Advanced
-- includes a @NextToken@ value in the response. You can retrieve the next
-- batch of objects by requesting the list again and providing the token
-- that was returned by the prior call in your request.
--
-- You can indicate the maximum number of objects that you want Shield
-- Advanced to return for a single call with the @MaxResults@ setting.
-- Shield Advanced will not return more than @MaxResults@ objects, but may
-- return fewer, even if more objects are still available.
--
-- Whenever more objects remain that Shield Advanced has not yet returned
-- to you, the response will include a @NextToken@ value.
listProtectionsResponse_nextToken :: Lens.Lens' ListProtectionsResponse (Prelude.Maybe Prelude.Text)
listProtectionsResponse_nextToken = Lens.lens (\ListProtectionsResponse' {nextToken} -> nextToken) (\s@ListProtectionsResponse' {} a -> s {nextToken = a} :: ListProtectionsResponse)

-- | The array of enabled Protection objects.
listProtectionsResponse_protections :: Lens.Lens' ListProtectionsResponse (Prelude.Maybe [Protection])
listProtectionsResponse_protections = Lens.lens (\ListProtectionsResponse' {protections} -> protections) (\s@ListProtectionsResponse' {} a -> s {protections = a} :: ListProtectionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listProtectionsResponse_httpStatus :: Lens.Lens' ListProtectionsResponse Prelude.Int
listProtectionsResponse_httpStatus = Lens.lens (\ListProtectionsResponse' {httpStatus} -> httpStatus) (\s@ListProtectionsResponse' {} a -> s {httpStatus = a} :: ListProtectionsResponse)

instance Prelude.NFData ListProtectionsResponse where
  rnf ListProtectionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf protections
      `Prelude.seq` Prelude.rnf httpStatus
