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
-- Module      : Amazonka.FMS.ListResourceSetResources
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of resources that are currently associated to a
-- resource set.
module Amazonka.FMS.ListResourceSetResources
  ( -- * Creating a Request
    ListResourceSetResources (..),
    newListResourceSetResources,

    -- * Request Lenses
    listResourceSetResources_maxResults,
    listResourceSetResources_nextToken,
    listResourceSetResources_identifier,

    -- * Destructuring the Response
    ListResourceSetResourcesResponse (..),
    newListResourceSetResourcesResponse,

    -- * Response Lenses
    listResourceSetResourcesResponse_nextToken,
    listResourceSetResourcesResponse_httpStatus,
    listResourceSetResourcesResponse_items,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FMS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListResourceSetResources' smart constructor.
data ListResourceSetResources = ListResourceSetResources'
  { -- | The maximum number of objects that you want Firewall Manager to return
    -- for this request. If more objects are available, in the response,
    -- Firewall Manager provides a @NextToken@ value that you can use in a
    -- subsequent call to get the next batch of objects.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | When you request a list of objects with a @MaxResults@ setting, if the
    -- number of objects that are still available for retrieval exceeds the
    -- maximum you requested, Firewall Manager returns a @NextToken@ value in
    -- the response. To retrieve the next batch of objects, use the token
    -- returned from the prior request in your next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the resource set, used in a TODO to refer to the
    -- resource set.
    identifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListResourceSetResources' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listResourceSetResources_maxResults' - The maximum number of objects that you want Firewall Manager to return
-- for this request. If more objects are available, in the response,
-- Firewall Manager provides a @NextToken@ value that you can use in a
-- subsequent call to get the next batch of objects.
--
-- 'nextToken', 'listResourceSetResources_nextToken' - When you request a list of objects with a @MaxResults@ setting, if the
-- number of objects that are still available for retrieval exceeds the
-- maximum you requested, Firewall Manager returns a @NextToken@ value in
-- the response. To retrieve the next batch of objects, use the token
-- returned from the prior request in your next request.
--
-- 'identifier', 'listResourceSetResources_identifier' - A unique identifier for the resource set, used in a TODO to refer to the
-- resource set.
newListResourceSetResources ::
  -- | 'identifier'
  Prelude.Text ->
  ListResourceSetResources
newListResourceSetResources pIdentifier_ =
  ListResourceSetResources'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      identifier = pIdentifier_
    }

-- | The maximum number of objects that you want Firewall Manager to return
-- for this request. If more objects are available, in the response,
-- Firewall Manager provides a @NextToken@ value that you can use in a
-- subsequent call to get the next batch of objects.
listResourceSetResources_maxResults :: Lens.Lens' ListResourceSetResources (Prelude.Maybe Prelude.Natural)
listResourceSetResources_maxResults = Lens.lens (\ListResourceSetResources' {maxResults} -> maxResults) (\s@ListResourceSetResources' {} a -> s {maxResults = a} :: ListResourceSetResources)

-- | When you request a list of objects with a @MaxResults@ setting, if the
-- number of objects that are still available for retrieval exceeds the
-- maximum you requested, Firewall Manager returns a @NextToken@ value in
-- the response. To retrieve the next batch of objects, use the token
-- returned from the prior request in your next request.
listResourceSetResources_nextToken :: Lens.Lens' ListResourceSetResources (Prelude.Maybe Prelude.Text)
listResourceSetResources_nextToken = Lens.lens (\ListResourceSetResources' {nextToken} -> nextToken) (\s@ListResourceSetResources' {} a -> s {nextToken = a} :: ListResourceSetResources)

-- | A unique identifier for the resource set, used in a TODO to refer to the
-- resource set.
listResourceSetResources_identifier :: Lens.Lens' ListResourceSetResources Prelude.Text
listResourceSetResources_identifier = Lens.lens (\ListResourceSetResources' {identifier} -> identifier) (\s@ListResourceSetResources' {} a -> s {identifier = a} :: ListResourceSetResources)

instance Core.AWSRequest ListResourceSetResources where
  type
    AWSResponse ListResourceSetResources =
      ListResourceSetResourcesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListResourceSetResourcesResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "Items" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListResourceSetResources where
  hashWithSalt _salt ListResourceSetResources' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` identifier

instance Prelude.NFData ListResourceSetResources where
  rnf ListResourceSetResources' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf identifier

instance Data.ToHeaders ListResourceSetResources where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSFMS_20180101.ListResourceSetResources" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListResourceSetResources where
  toJSON ListResourceSetResources' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("Identifier" Data..= identifier)
          ]
      )

instance Data.ToPath ListResourceSetResources where
  toPath = Prelude.const "/"

instance Data.ToQuery ListResourceSetResources where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListResourceSetResourcesResponse' smart constructor.
data ListResourceSetResourcesResponse = ListResourceSetResourcesResponse'
  { -- | When you request a list of objects with a @MaxResults@ setting, if the
    -- number of objects that are still available for retrieval exceeds the
    -- maximum you requested, Firewall Manager returns a @NextToken@ value in
    -- the response. To retrieve the next batch of objects, use the token
    -- returned from the prior request in your next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | An array of the associated resources\' uniform resource identifiers
    -- (URI).
    items :: [Resource]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListResourceSetResourcesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listResourceSetResourcesResponse_nextToken' - When you request a list of objects with a @MaxResults@ setting, if the
-- number of objects that are still available for retrieval exceeds the
-- maximum you requested, Firewall Manager returns a @NextToken@ value in
-- the response. To retrieve the next batch of objects, use the token
-- returned from the prior request in your next request.
--
-- 'httpStatus', 'listResourceSetResourcesResponse_httpStatus' - The response's http status code.
--
-- 'items', 'listResourceSetResourcesResponse_items' - An array of the associated resources\' uniform resource identifiers
-- (URI).
newListResourceSetResourcesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListResourceSetResourcesResponse
newListResourceSetResourcesResponse pHttpStatus_ =
  ListResourceSetResourcesResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      items = Prelude.mempty
    }

-- | When you request a list of objects with a @MaxResults@ setting, if the
-- number of objects that are still available for retrieval exceeds the
-- maximum you requested, Firewall Manager returns a @NextToken@ value in
-- the response. To retrieve the next batch of objects, use the token
-- returned from the prior request in your next request.
listResourceSetResourcesResponse_nextToken :: Lens.Lens' ListResourceSetResourcesResponse (Prelude.Maybe Prelude.Text)
listResourceSetResourcesResponse_nextToken = Lens.lens (\ListResourceSetResourcesResponse' {nextToken} -> nextToken) (\s@ListResourceSetResourcesResponse' {} a -> s {nextToken = a} :: ListResourceSetResourcesResponse)

-- | The response's http status code.
listResourceSetResourcesResponse_httpStatus :: Lens.Lens' ListResourceSetResourcesResponse Prelude.Int
listResourceSetResourcesResponse_httpStatus = Lens.lens (\ListResourceSetResourcesResponse' {httpStatus} -> httpStatus) (\s@ListResourceSetResourcesResponse' {} a -> s {httpStatus = a} :: ListResourceSetResourcesResponse)

-- | An array of the associated resources\' uniform resource identifiers
-- (URI).
listResourceSetResourcesResponse_items :: Lens.Lens' ListResourceSetResourcesResponse [Resource]
listResourceSetResourcesResponse_items = Lens.lens (\ListResourceSetResourcesResponse' {items} -> items) (\s@ListResourceSetResourcesResponse' {} a -> s {items = a} :: ListResourceSetResourcesResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    ListResourceSetResourcesResponse
  where
  rnf ListResourceSetResourcesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf items
