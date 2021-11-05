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
-- Module      : Amazonka.IoTThingsGraph.SearchThings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Searches for things associated with the specified entity. You can search
-- by both device and device model.
--
-- For example, if two different devices, camera1 and camera2, implement
-- the camera device model, the user can associate thing1 to camera1 and
-- thing2 to camera2. @SearchThings(camera2)@ will return only thing2, but
-- @SearchThings(camera)@ will return both thing1 and thing2.
--
-- This action searches for exact matches and doesn\'t perform partial text
-- matching.
--
-- This operation returns paginated results.
module Amazonka.IoTThingsGraph.SearchThings
  ( -- * Creating a Request
    SearchThings (..),
    newSearchThings,

    -- * Request Lenses
    searchThings_namespaceVersion,
    searchThings_nextToken,
    searchThings_maxResults,
    searchThings_entityId,

    -- * Destructuring the Response
    SearchThingsResponse (..),
    newSearchThingsResponse,

    -- * Response Lenses
    searchThingsResponse_nextToken,
    searchThingsResponse_things,
    searchThingsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.IoTThingsGraph.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSearchThings' smart constructor.
data SearchThings = SearchThings'
  { -- | The version of the user\'s namespace. Defaults to the latest version of
    -- the user\'s namespace.
    namespaceVersion :: Prelude.Maybe Prelude.Integer,
    -- | The string that specifies the next page of results. Use this when
    -- you\'re paginating results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return in the response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The ID of the entity to which the things are associated.
    --
    -- The IDs should be in the following format.
    --
    -- @urn:tdm:REGION\/ACCOUNT ID\/default:device:DEVICENAME@
    entityId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchThings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'namespaceVersion', 'searchThings_namespaceVersion' - The version of the user\'s namespace. Defaults to the latest version of
-- the user\'s namespace.
--
-- 'nextToken', 'searchThings_nextToken' - The string that specifies the next page of results. Use this when
-- you\'re paginating results.
--
-- 'maxResults', 'searchThings_maxResults' - The maximum number of results to return in the response.
--
-- 'entityId', 'searchThings_entityId' - The ID of the entity to which the things are associated.
--
-- The IDs should be in the following format.
--
-- @urn:tdm:REGION\/ACCOUNT ID\/default:device:DEVICENAME@
newSearchThings ::
  -- | 'entityId'
  Prelude.Text ->
  SearchThings
newSearchThings pEntityId_ =
  SearchThings'
    { namespaceVersion = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      entityId = pEntityId_
    }

-- | The version of the user\'s namespace. Defaults to the latest version of
-- the user\'s namespace.
searchThings_namespaceVersion :: Lens.Lens' SearchThings (Prelude.Maybe Prelude.Integer)
searchThings_namespaceVersion = Lens.lens (\SearchThings' {namespaceVersion} -> namespaceVersion) (\s@SearchThings' {} a -> s {namespaceVersion = a} :: SearchThings)

-- | The string that specifies the next page of results. Use this when
-- you\'re paginating results.
searchThings_nextToken :: Lens.Lens' SearchThings (Prelude.Maybe Prelude.Text)
searchThings_nextToken = Lens.lens (\SearchThings' {nextToken} -> nextToken) (\s@SearchThings' {} a -> s {nextToken = a} :: SearchThings)

-- | The maximum number of results to return in the response.
searchThings_maxResults :: Lens.Lens' SearchThings (Prelude.Maybe Prelude.Natural)
searchThings_maxResults = Lens.lens (\SearchThings' {maxResults} -> maxResults) (\s@SearchThings' {} a -> s {maxResults = a} :: SearchThings)

-- | The ID of the entity to which the things are associated.
--
-- The IDs should be in the following format.
--
-- @urn:tdm:REGION\/ACCOUNT ID\/default:device:DEVICENAME@
searchThings_entityId :: Lens.Lens' SearchThings Prelude.Text
searchThings_entityId = Lens.lens (\SearchThings' {entityId} -> entityId) (\s@SearchThings' {} a -> s {entityId = a} :: SearchThings)

instance Core.AWSPager SearchThings where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? searchThingsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? searchThingsResponse_things Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& searchThings_nextToken
          Lens..~ rs
          Lens.^? searchThingsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest SearchThings where
  type AWSResponse SearchThings = SearchThingsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          SearchThingsResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "things" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SearchThings

instance Prelude.NFData SearchThings

instance Core.ToHeaders SearchThings where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "IotThingsGraphFrontEndService.SearchThings" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON SearchThings where
  toJSON SearchThings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("namespaceVersion" Core..=)
              Prelude.<$> namespaceVersion,
            ("nextToken" Core..=) Prelude.<$> nextToken,
            ("maxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just ("entityId" Core..= entityId)
          ]
      )

instance Core.ToPath SearchThings where
  toPath = Prelude.const "/"

instance Core.ToQuery SearchThings where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSearchThingsResponse' smart constructor.
data SearchThingsResponse = SearchThingsResponse'
  { -- | The string to specify as @nextToken@ when you request the next page of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of things in the result set.
    things :: Prelude.Maybe [Thing],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchThingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'searchThingsResponse_nextToken' - The string to specify as @nextToken@ when you request the next page of
-- results.
--
-- 'things', 'searchThingsResponse_things' - An array of things in the result set.
--
-- 'httpStatus', 'searchThingsResponse_httpStatus' - The response's http status code.
newSearchThingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SearchThingsResponse
newSearchThingsResponse pHttpStatus_ =
  SearchThingsResponse'
    { nextToken = Prelude.Nothing,
      things = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The string to specify as @nextToken@ when you request the next page of
-- results.
searchThingsResponse_nextToken :: Lens.Lens' SearchThingsResponse (Prelude.Maybe Prelude.Text)
searchThingsResponse_nextToken = Lens.lens (\SearchThingsResponse' {nextToken} -> nextToken) (\s@SearchThingsResponse' {} a -> s {nextToken = a} :: SearchThingsResponse)

-- | An array of things in the result set.
searchThingsResponse_things :: Lens.Lens' SearchThingsResponse (Prelude.Maybe [Thing])
searchThingsResponse_things = Lens.lens (\SearchThingsResponse' {things} -> things) (\s@SearchThingsResponse' {} a -> s {things = a} :: SearchThingsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
searchThingsResponse_httpStatus :: Lens.Lens' SearchThingsResponse Prelude.Int
searchThingsResponse_httpStatus = Lens.lens (\SearchThingsResponse' {httpStatus} -> httpStatus) (\s@SearchThingsResponse' {} a -> s {httpStatus = a} :: SearchThingsResponse)

instance Prelude.NFData SearchThingsResponse
