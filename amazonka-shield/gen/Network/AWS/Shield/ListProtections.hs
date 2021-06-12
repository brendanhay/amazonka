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
-- Module      : Network.AWS.Shield.ListProtections
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all Protection objects for the account.
--
-- This operation returns paginated results.
module Network.AWS.Shield.ListProtections
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
    listProtectionsResponse_nextToken,
    listProtectionsResponse_protections,
    listProtectionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Shield.Types

-- | /See:/ 'newListProtections' smart constructor.
data ListProtections = ListProtections'
  { -- | The @ListProtectionsRequest.NextToken@ value from a previous call to
    -- @ListProtections@. Pass null if this is the first call.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of Protection objects to return. If you leave this
    -- blank, Shield Advanced returns the first 20 results.
    --
    -- This is a maximum value. Shield Advanced might return the results in
    -- smaller batches. That is, the number of objects returned could be less
    -- than @MaxResults@, even if there are still more objects yet to return.
    -- If there are more objects to return, Shield Advanced returns a value in
    -- @NextToken@ that you can use in your next request, to get the next batch
    -- of objects.
    maxResults :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing
    }

-- | The @ListProtectionsRequest.NextToken@ value from a previous call to
-- @ListProtections@. Pass null if this is the first call.
listProtections_nextToken :: Lens.Lens' ListProtections (Core.Maybe Core.Text)
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
listProtections_maxResults :: Lens.Lens' ListProtections (Core.Maybe Core.Natural)
listProtections_maxResults = Lens.lens (\ListProtections' {maxResults} -> maxResults) (\s@ListProtections' {} a -> s {maxResults = a} :: ListProtections)

instance Core.AWSPager ListProtections where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listProtectionsResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listProtectionsResponse_protections
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listProtections_nextToken
          Lens..~ rs
          Lens.^? listProtectionsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListProtections where
  type
    AWSResponse ListProtections =
      ListProtectionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListProtectionsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Protections" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListProtections

instance Core.NFData ListProtections

instance Core.ToHeaders ListProtections where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSShield_20160616.ListProtections" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListProtections where
  toJSON ListProtections' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults
          ]
      )

instance Core.ToPath ListProtections where
  toPath = Core.const "/"

instance Core.ToQuery ListProtections where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListProtectionsResponse' smart constructor.
data ListProtectionsResponse = ListProtectionsResponse'
  { -- | If you specify a value for @MaxResults@ and you have more Protections
    -- than the value of MaxResults, AWS Shield Advanced returns a NextToken
    -- value in the response that allows you to list another group of
    -- Protections. For the second and subsequent ListProtections requests,
    -- specify the value of NextToken from the previous response to get
    -- information about another batch of Protections.
    --
    -- Shield Advanced might return the list of Protection objects in batches
    -- smaller than the number specified by MaxResults. If there are more
    -- Protection objects to return, Shield Advanced will always also return a
    -- @NextToken@.
    nextToken :: Core.Maybe Core.Text,
    -- | The array of enabled Protection objects.
    protections :: Core.Maybe [Protection],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListProtectionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listProtectionsResponse_nextToken' - If you specify a value for @MaxResults@ and you have more Protections
-- than the value of MaxResults, AWS Shield Advanced returns a NextToken
-- value in the response that allows you to list another group of
-- Protections. For the second and subsequent ListProtections requests,
-- specify the value of NextToken from the previous response to get
-- information about another batch of Protections.
--
-- Shield Advanced might return the list of Protection objects in batches
-- smaller than the number specified by MaxResults. If there are more
-- Protection objects to return, Shield Advanced will always also return a
-- @NextToken@.
--
-- 'protections', 'listProtectionsResponse_protections' - The array of enabled Protection objects.
--
-- 'httpStatus', 'listProtectionsResponse_httpStatus' - The response's http status code.
newListProtectionsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListProtectionsResponse
newListProtectionsResponse pHttpStatus_ =
  ListProtectionsResponse'
    { nextToken = Core.Nothing,
      protections = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If you specify a value for @MaxResults@ and you have more Protections
-- than the value of MaxResults, AWS Shield Advanced returns a NextToken
-- value in the response that allows you to list another group of
-- Protections. For the second and subsequent ListProtections requests,
-- specify the value of NextToken from the previous response to get
-- information about another batch of Protections.
--
-- Shield Advanced might return the list of Protection objects in batches
-- smaller than the number specified by MaxResults. If there are more
-- Protection objects to return, Shield Advanced will always also return a
-- @NextToken@.
listProtectionsResponse_nextToken :: Lens.Lens' ListProtectionsResponse (Core.Maybe Core.Text)
listProtectionsResponse_nextToken = Lens.lens (\ListProtectionsResponse' {nextToken} -> nextToken) (\s@ListProtectionsResponse' {} a -> s {nextToken = a} :: ListProtectionsResponse)

-- | The array of enabled Protection objects.
listProtectionsResponse_protections :: Lens.Lens' ListProtectionsResponse (Core.Maybe [Protection])
listProtectionsResponse_protections = Lens.lens (\ListProtectionsResponse' {protections} -> protections) (\s@ListProtectionsResponse' {} a -> s {protections = a} :: ListProtectionsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listProtectionsResponse_httpStatus :: Lens.Lens' ListProtectionsResponse Core.Int
listProtectionsResponse_httpStatus = Lens.lens (\ListProtectionsResponse' {httpStatus} -> httpStatus) (\s@ListProtectionsResponse' {} a -> s {httpStatus = a} :: ListProtectionsResponse)

instance Core.NFData ListProtectionsResponse
