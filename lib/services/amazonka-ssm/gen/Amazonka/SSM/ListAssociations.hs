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
-- Module      : Amazonka.SSM.ListAssociations
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns all State Manager associations in the current Amazon Web
-- Services account and Amazon Web Services Region. You can limit the
-- results to a specific State Manager association document or managed node
-- by specifying a filter. State Manager is a capability of Amazon Web
-- Services Systems Manager.
--
-- This operation returns paginated results.
module Amazonka.SSM.ListAssociations
  ( -- * Creating a Request
    ListAssociations (..),
    newListAssociations,

    -- * Request Lenses
    listAssociations_associationFilterList,
    listAssociations_maxResults,
    listAssociations_nextToken,

    -- * Destructuring the Response
    ListAssociationsResponse (..),
    newListAssociationsResponse,

    -- * Response Lenses
    listAssociationsResponse_associations,
    listAssociationsResponse_nextToken,
    listAssociationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newListAssociations' smart constructor.
data ListAssociations = ListAssociations'
  { -- | One or more filters. Use a filter to return a more specific list of
    -- results.
    --
    -- Filtering associations using the @InstanceID@ attribute only returns
    -- legacy associations created using the @InstanceID@ attribute.
    -- Associations targeting the managed node that are part of the Target
    -- Attributes @ResourceGroup@ or @Tags@ aren\'t returned.
    associationFilterList :: Prelude.Maybe (Prelude.NonEmpty AssociationFilter),
    -- | The maximum number of items to return for this call. The call also
    -- returns a token that you can specify in a subsequent call to get the
    -- next set of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next set of items to return. (You received this token
    -- from a previous call.)
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAssociations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associationFilterList', 'listAssociations_associationFilterList' - One or more filters. Use a filter to return a more specific list of
-- results.
--
-- Filtering associations using the @InstanceID@ attribute only returns
-- legacy associations created using the @InstanceID@ attribute.
-- Associations targeting the managed node that are part of the Target
-- Attributes @ResourceGroup@ or @Tags@ aren\'t returned.
--
-- 'maxResults', 'listAssociations_maxResults' - The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
--
-- 'nextToken', 'listAssociations_nextToken' - The token for the next set of items to return. (You received this token
-- from a previous call.)
newListAssociations ::
  ListAssociations
newListAssociations =
  ListAssociations'
    { associationFilterList =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | One or more filters. Use a filter to return a more specific list of
-- results.
--
-- Filtering associations using the @InstanceID@ attribute only returns
-- legacy associations created using the @InstanceID@ attribute.
-- Associations targeting the managed node that are part of the Target
-- Attributes @ResourceGroup@ or @Tags@ aren\'t returned.
listAssociations_associationFilterList :: Lens.Lens' ListAssociations (Prelude.Maybe (Prelude.NonEmpty AssociationFilter))
listAssociations_associationFilterList = Lens.lens (\ListAssociations' {associationFilterList} -> associationFilterList) (\s@ListAssociations' {} a -> s {associationFilterList = a} :: ListAssociations) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
listAssociations_maxResults :: Lens.Lens' ListAssociations (Prelude.Maybe Prelude.Natural)
listAssociations_maxResults = Lens.lens (\ListAssociations' {maxResults} -> maxResults) (\s@ListAssociations' {} a -> s {maxResults = a} :: ListAssociations)

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
listAssociations_nextToken :: Lens.Lens' ListAssociations (Prelude.Maybe Prelude.Text)
listAssociations_nextToken = Lens.lens (\ListAssociations' {nextToken} -> nextToken) (\s@ListAssociations' {} a -> s {nextToken = a} :: ListAssociations)

instance Core.AWSPager ListAssociations where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAssociationsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listAssociationsResponse_associations
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listAssociations_nextToken
          Lens..~ rs
          Lens.^? listAssociationsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListAssociations where
  type
    AWSResponse ListAssociations =
      ListAssociationsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAssociationsResponse'
            Prelude.<$> (x Data..?> "Associations" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListAssociations where
  hashWithSalt _salt ListAssociations' {..} =
    _salt `Prelude.hashWithSalt` associationFilterList
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListAssociations where
  rnf ListAssociations' {..} =
    Prelude.rnf associationFilterList
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListAssociations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AmazonSSM.ListAssociations" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListAssociations where
  toJSON ListAssociations' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AssociationFilterList" Data..=)
              Prelude.<$> associationFilterList,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListAssociations where
  toPath = Prelude.const "/"

instance Data.ToQuery ListAssociations where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListAssociationsResponse' smart constructor.
data ListAssociationsResponse = ListAssociationsResponse'
  { -- | The associations.
    associations :: Prelude.Maybe [Association],
    -- | The token to use when requesting the next set of items. If there are no
    -- additional items to return, the string is empty.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAssociationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associations', 'listAssociationsResponse_associations' - The associations.
--
-- 'nextToken', 'listAssociationsResponse_nextToken' - The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
--
-- 'httpStatus', 'listAssociationsResponse_httpStatus' - The response's http status code.
newListAssociationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAssociationsResponse
newListAssociationsResponse pHttpStatus_ =
  ListAssociationsResponse'
    { associations =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The associations.
listAssociationsResponse_associations :: Lens.Lens' ListAssociationsResponse (Prelude.Maybe [Association])
listAssociationsResponse_associations = Lens.lens (\ListAssociationsResponse' {associations} -> associations) (\s@ListAssociationsResponse' {} a -> s {associations = a} :: ListAssociationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
listAssociationsResponse_nextToken :: Lens.Lens' ListAssociationsResponse (Prelude.Maybe Prelude.Text)
listAssociationsResponse_nextToken = Lens.lens (\ListAssociationsResponse' {nextToken} -> nextToken) (\s@ListAssociationsResponse' {} a -> s {nextToken = a} :: ListAssociationsResponse)

-- | The response's http status code.
listAssociationsResponse_httpStatus :: Lens.Lens' ListAssociationsResponse Prelude.Int
listAssociationsResponse_httpStatus = Lens.lens (\ListAssociationsResponse' {httpStatus} -> httpStatus) (\s@ListAssociationsResponse' {} a -> s {httpStatus = a} :: ListAssociationsResponse)

instance Prelude.NFData ListAssociationsResponse where
  rnf ListAssociationsResponse' {..} =
    Prelude.rnf associations
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
