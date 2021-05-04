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
-- Module      : Network.AWS.Glue.ListRegistries
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of registries that you have created, with minimal
-- registry information. Registries in the @Deleting@ status will not be
-- included in the results. Empty results will be returned if there are no
-- registries available.
--
-- This operation returns paginated results.
module Network.AWS.Glue.ListRegistries
  ( -- * Creating a Request
    ListRegistries (..),
    newListRegistries,

    -- * Request Lenses
    listRegistries_nextToken,
    listRegistries_maxResults,

    -- * Destructuring the Response
    ListRegistriesResponse (..),
    newListRegistriesResponse,

    -- * Response Lenses
    listRegistriesResponse_nextToken,
    listRegistriesResponse_registries,
    listRegistriesResponse_httpStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListRegistries' smart constructor.
data ListRegistries = ListRegistries'
  { -- | A continuation token, if this is a continuation call.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Maximum number of results required per page. If the value is not
    -- supplied, this will be defaulted to 25 per page.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListRegistries' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listRegistries_nextToken' - A continuation token, if this is a continuation call.
--
-- 'maxResults', 'listRegistries_maxResults' - Maximum number of results required per page. If the value is not
-- supplied, this will be defaulted to 25 per page.
newListRegistries ::
  ListRegistries
newListRegistries =
  ListRegistries'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | A continuation token, if this is a continuation call.
listRegistries_nextToken :: Lens.Lens' ListRegistries (Prelude.Maybe Prelude.Text)
listRegistries_nextToken = Lens.lens (\ListRegistries' {nextToken} -> nextToken) (\s@ListRegistries' {} a -> s {nextToken = a} :: ListRegistries)

-- | Maximum number of results required per page. If the value is not
-- supplied, this will be defaulted to 25 per page.
listRegistries_maxResults :: Lens.Lens' ListRegistries (Prelude.Maybe Prelude.Natural)
listRegistries_maxResults = Lens.lens (\ListRegistries' {maxResults} -> maxResults) (\s@ListRegistries' {} a -> s {maxResults = a} :: ListRegistries)

instance Pager.AWSPager ListRegistries where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? listRegistriesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? listRegistriesResponse_registries
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& listRegistries_nextToken
          Lens..~ rs
          Lens.^? listRegistriesResponse_nextToken Prelude.. Lens._Just

instance Prelude.AWSRequest ListRegistries where
  type Rs ListRegistries = ListRegistriesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListRegistriesResponse'
            Prelude.<$> (x Prelude..?> "NextToken")
            Prelude.<*> ( x Prelude..?> "Registries"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListRegistries

instance Prelude.NFData ListRegistries

instance Prelude.ToHeaders ListRegistries where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("AWSGlue.ListRegistries" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ListRegistries where
  toJSON ListRegistries' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("NextToken" Prelude..=) Prelude.<$> nextToken,
            ("MaxResults" Prelude..=) Prelude.<$> maxResults
          ]
      )

instance Prelude.ToPath ListRegistries where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ListRegistries where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListRegistriesResponse' smart constructor.
data ListRegistriesResponse = ListRegistriesResponse'
  { -- | A continuation token for paginating the returned list of tokens,
    -- returned if the current segment of the list is not the last.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of @RegistryDetailedListItem@ objects containing minimal
    -- details of each registry.
    registries :: Prelude.Maybe [RegistryListItem],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListRegistriesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listRegistriesResponse_nextToken' - A continuation token for paginating the returned list of tokens,
-- returned if the current segment of the list is not the last.
--
-- 'registries', 'listRegistriesResponse_registries' - An array of @RegistryDetailedListItem@ objects containing minimal
-- details of each registry.
--
-- 'httpStatus', 'listRegistriesResponse_httpStatus' - The response's http status code.
newListRegistriesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListRegistriesResponse
newListRegistriesResponse pHttpStatus_ =
  ListRegistriesResponse'
    { nextToken =
        Prelude.Nothing,
      registries = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A continuation token for paginating the returned list of tokens,
-- returned if the current segment of the list is not the last.
listRegistriesResponse_nextToken :: Lens.Lens' ListRegistriesResponse (Prelude.Maybe Prelude.Text)
listRegistriesResponse_nextToken = Lens.lens (\ListRegistriesResponse' {nextToken} -> nextToken) (\s@ListRegistriesResponse' {} a -> s {nextToken = a} :: ListRegistriesResponse)

-- | An array of @RegistryDetailedListItem@ objects containing minimal
-- details of each registry.
listRegistriesResponse_registries :: Lens.Lens' ListRegistriesResponse (Prelude.Maybe [RegistryListItem])
listRegistriesResponse_registries = Lens.lens (\ListRegistriesResponse' {registries} -> registries) (\s@ListRegistriesResponse' {} a -> s {registries = a} :: ListRegistriesResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
listRegistriesResponse_httpStatus :: Lens.Lens' ListRegistriesResponse Prelude.Int
listRegistriesResponse_httpStatus = Lens.lens (\ListRegistriesResponse' {httpStatus} -> httpStatus) (\s@ListRegistriesResponse' {} a -> s {httpStatus = a} :: ListRegistriesResponse)

instance Prelude.NFData ListRegistriesResponse
