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
-- Module      : Amazonka.CustomerProfiles.ListProfileObjects
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of objects associated with a profile of a given
-- ProfileObjectType.
module Amazonka.CustomerProfiles.ListProfileObjects
  ( -- * Creating a Request
    ListProfileObjects (..),
    newListProfileObjects,

    -- * Request Lenses
    listProfileObjects_maxResults,
    listProfileObjects_nextToken,
    listProfileObjects_objectFilter,
    listProfileObjects_domainName,
    listProfileObjects_objectTypeName,
    listProfileObjects_profileId,

    -- * Destructuring the Response
    ListProfileObjectsResponse (..),
    newListProfileObjectsResponse,

    -- * Response Lenses
    listProfileObjectsResponse_items,
    listProfileObjectsResponse_nextToken,
    listProfileObjectsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CustomerProfiles.Types
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListProfileObjects' smart constructor.
data ListProfileObjects = ListProfileObjects'
  { -- | The maximum number of objects returned per page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The pagination token from the previous call to ListProfileObjects.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Applies a filter to the response to include profile objects with the
    -- specified index values.
    objectFilter :: Prelude.Maybe ObjectFilter,
    -- | The unique name of the domain.
    domainName :: Prelude.Text,
    -- | The name of the profile object type.
    objectTypeName :: Prelude.Text,
    -- | The unique identifier of a customer profile.
    profileId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListProfileObjects' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listProfileObjects_maxResults' - The maximum number of objects returned per page.
--
-- 'nextToken', 'listProfileObjects_nextToken' - The pagination token from the previous call to ListProfileObjects.
--
-- 'objectFilter', 'listProfileObjects_objectFilter' - Applies a filter to the response to include profile objects with the
-- specified index values.
--
-- 'domainName', 'listProfileObjects_domainName' - The unique name of the domain.
--
-- 'objectTypeName', 'listProfileObjects_objectTypeName' - The name of the profile object type.
--
-- 'profileId', 'listProfileObjects_profileId' - The unique identifier of a customer profile.
newListProfileObjects ::
  -- | 'domainName'
  Prelude.Text ->
  -- | 'objectTypeName'
  Prelude.Text ->
  -- | 'profileId'
  Prelude.Text ->
  ListProfileObjects
newListProfileObjects
  pDomainName_
  pObjectTypeName_
  pProfileId_ =
    ListProfileObjects'
      { maxResults = Prelude.Nothing,
        nextToken = Prelude.Nothing,
        objectFilter = Prelude.Nothing,
        domainName = pDomainName_,
        objectTypeName = pObjectTypeName_,
        profileId = pProfileId_
      }

-- | The maximum number of objects returned per page.
listProfileObjects_maxResults :: Lens.Lens' ListProfileObjects (Prelude.Maybe Prelude.Natural)
listProfileObjects_maxResults = Lens.lens (\ListProfileObjects' {maxResults} -> maxResults) (\s@ListProfileObjects' {} a -> s {maxResults = a} :: ListProfileObjects)

-- | The pagination token from the previous call to ListProfileObjects.
listProfileObjects_nextToken :: Lens.Lens' ListProfileObjects (Prelude.Maybe Prelude.Text)
listProfileObjects_nextToken = Lens.lens (\ListProfileObjects' {nextToken} -> nextToken) (\s@ListProfileObjects' {} a -> s {nextToken = a} :: ListProfileObjects)

-- | Applies a filter to the response to include profile objects with the
-- specified index values.
listProfileObjects_objectFilter :: Lens.Lens' ListProfileObjects (Prelude.Maybe ObjectFilter)
listProfileObjects_objectFilter = Lens.lens (\ListProfileObjects' {objectFilter} -> objectFilter) (\s@ListProfileObjects' {} a -> s {objectFilter = a} :: ListProfileObjects)

-- | The unique name of the domain.
listProfileObjects_domainName :: Lens.Lens' ListProfileObjects Prelude.Text
listProfileObjects_domainName = Lens.lens (\ListProfileObjects' {domainName} -> domainName) (\s@ListProfileObjects' {} a -> s {domainName = a} :: ListProfileObjects)

-- | The name of the profile object type.
listProfileObjects_objectTypeName :: Lens.Lens' ListProfileObjects Prelude.Text
listProfileObjects_objectTypeName = Lens.lens (\ListProfileObjects' {objectTypeName} -> objectTypeName) (\s@ListProfileObjects' {} a -> s {objectTypeName = a} :: ListProfileObjects)

-- | The unique identifier of a customer profile.
listProfileObjects_profileId :: Lens.Lens' ListProfileObjects Prelude.Text
listProfileObjects_profileId = Lens.lens (\ListProfileObjects' {profileId} -> profileId) (\s@ListProfileObjects' {} a -> s {profileId = a} :: ListProfileObjects)

instance Core.AWSRequest ListProfileObjects where
  type
    AWSResponse ListProfileObjects =
      ListProfileObjectsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListProfileObjectsResponse'
            Prelude.<$> (x Data..?> "Items" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListProfileObjects where
  hashWithSalt _salt ListProfileObjects' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` objectFilter
      `Prelude.hashWithSalt` domainName
      `Prelude.hashWithSalt` objectTypeName
      `Prelude.hashWithSalt` profileId

instance Prelude.NFData ListProfileObjects where
  rnf ListProfileObjects' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf objectFilter
      `Prelude.seq` Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf objectTypeName
      `Prelude.seq` Prelude.rnf profileId

instance Data.ToHeaders ListProfileObjects where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListProfileObjects where
  toJSON ListProfileObjects' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ObjectFilter" Data..=) Prelude.<$> objectFilter,
            Prelude.Just
              ("ObjectTypeName" Data..= objectTypeName),
            Prelude.Just ("ProfileId" Data..= profileId)
          ]
      )

instance Data.ToPath ListProfileObjects where
  toPath ListProfileObjects' {..} =
    Prelude.mconcat
      [ "/domains/",
        Data.toBS domainName,
        "/profiles/objects"
      ]

instance Data.ToQuery ListProfileObjects where
  toQuery ListProfileObjects' {..} =
    Prelude.mconcat
      [ "max-results" Data.=: maxResults,
        "next-token" Data.=: nextToken
      ]

-- | /See:/ 'newListProfileObjectsResponse' smart constructor.
data ListProfileObjectsResponse = ListProfileObjectsResponse'
  { -- | The list of ListProfileObject instances.
    items :: Prelude.Maybe [ListProfileObjectsItem],
    -- | The pagination token from the previous call to ListProfileObjects.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListProfileObjectsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'listProfileObjectsResponse_items' - The list of ListProfileObject instances.
--
-- 'nextToken', 'listProfileObjectsResponse_nextToken' - The pagination token from the previous call to ListProfileObjects.
--
-- 'httpStatus', 'listProfileObjectsResponse_httpStatus' - The response's http status code.
newListProfileObjectsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListProfileObjectsResponse
newListProfileObjectsResponse pHttpStatus_ =
  ListProfileObjectsResponse'
    { items =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of ListProfileObject instances.
listProfileObjectsResponse_items :: Lens.Lens' ListProfileObjectsResponse (Prelude.Maybe [ListProfileObjectsItem])
listProfileObjectsResponse_items = Lens.lens (\ListProfileObjectsResponse' {items} -> items) (\s@ListProfileObjectsResponse' {} a -> s {items = a} :: ListProfileObjectsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The pagination token from the previous call to ListProfileObjects.
listProfileObjectsResponse_nextToken :: Lens.Lens' ListProfileObjectsResponse (Prelude.Maybe Prelude.Text)
listProfileObjectsResponse_nextToken = Lens.lens (\ListProfileObjectsResponse' {nextToken} -> nextToken) (\s@ListProfileObjectsResponse' {} a -> s {nextToken = a} :: ListProfileObjectsResponse)

-- | The response's http status code.
listProfileObjectsResponse_httpStatus :: Lens.Lens' ListProfileObjectsResponse Prelude.Int
listProfileObjectsResponse_httpStatus = Lens.lens (\ListProfileObjectsResponse' {httpStatus} -> httpStatus) (\s@ListProfileObjectsResponse' {} a -> s {httpStatus = a} :: ListProfileObjectsResponse)

instance Prelude.NFData ListProfileObjectsResponse where
  rnf ListProfileObjectsResponse' {..} =
    Prelude.rnf items
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
