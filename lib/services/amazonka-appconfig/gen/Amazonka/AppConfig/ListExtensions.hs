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
-- Module      : Amazonka.AppConfig.ListExtensions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all custom and Amazon Web Services authored AppConfig extensions
-- in the account. For more information about extensions, see
-- <https://docs.aws.amazon.com/appconfig/latest/userguide/working-with-appconfig-extensions.html Working with AppConfig extensions>
-- in the /AppConfig User Guide/.
module Amazonka.AppConfig.ListExtensions
  ( -- * Creating a Request
    ListExtensions (..),
    newListExtensions,

    -- * Request Lenses
    listExtensions_maxResults,
    listExtensions_name,
    listExtensions_nextToken,

    -- * Destructuring the Response
    ListExtensionsResponse (..),
    newListExtensionsResponse,

    -- * Response Lenses
    listExtensionsResponse_items,
    listExtensionsResponse_nextToken,
    listExtensionsResponse_httpStatus,
  )
where

import Amazonka.AppConfig.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListExtensions' smart constructor.
data ListExtensions = ListExtensions'
  { -- | The maximum number of items to return for this call. The call also
    -- returns a token that you can specify in a subsequent call to get the
    -- next set of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The extension name.
    name :: Prelude.Maybe Prelude.Text,
    -- | A token to start the list. Use this token to get the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListExtensions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listExtensions_maxResults' - The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
--
-- 'name', 'listExtensions_name' - The extension name.
--
-- 'nextToken', 'listExtensions_nextToken' - A token to start the list. Use this token to get the next set of
-- results.
newListExtensions ::
  ListExtensions
newListExtensions =
  ListExtensions'
    { maxResults = Prelude.Nothing,
      name = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
listExtensions_maxResults :: Lens.Lens' ListExtensions (Prelude.Maybe Prelude.Natural)
listExtensions_maxResults = Lens.lens (\ListExtensions' {maxResults} -> maxResults) (\s@ListExtensions' {} a -> s {maxResults = a} :: ListExtensions)

-- | The extension name.
listExtensions_name :: Lens.Lens' ListExtensions (Prelude.Maybe Prelude.Text)
listExtensions_name = Lens.lens (\ListExtensions' {name} -> name) (\s@ListExtensions' {} a -> s {name = a} :: ListExtensions)

-- | A token to start the list. Use this token to get the next set of
-- results.
listExtensions_nextToken :: Lens.Lens' ListExtensions (Prelude.Maybe Prelude.Text)
listExtensions_nextToken = Lens.lens (\ListExtensions' {nextToken} -> nextToken) (\s@ListExtensions' {} a -> s {nextToken = a} :: ListExtensions)

instance Core.AWSRequest ListExtensions where
  type
    AWSResponse ListExtensions =
      ListExtensionsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListExtensionsResponse'
            Prelude.<$> (x Data..?> "Items" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListExtensions where
  hashWithSalt _salt ListExtensions' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListExtensions where
  rnf ListExtensions' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListExtensions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListExtensions where
  toPath = Prelude.const "/extensions"

instance Data.ToQuery ListExtensions where
  toQuery ListExtensions' {..} =
    Prelude.mconcat
      [ "max_results" Data.=: maxResults,
        "name" Data.=: name,
        "next_token" Data.=: nextToken
      ]

-- | /See:/ 'newListExtensionsResponse' smart constructor.
data ListExtensionsResponse = ListExtensionsResponse'
  { -- | The list of available extensions. The list includes Amazon Web Services
    -- authored and user-created extensions.
    items :: Prelude.Maybe [ExtensionSummary],
    -- | The token for the next set of items to return. Use this token to get the
    -- next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListExtensionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'listExtensionsResponse_items' - The list of available extensions. The list includes Amazon Web Services
-- authored and user-created extensions.
--
-- 'nextToken', 'listExtensionsResponse_nextToken' - The token for the next set of items to return. Use this token to get the
-- next set of results.
--
-- 'httpStatus', 'listExtensionsResponse_httpStatus' - The response's http status code.
newListExtensionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListExtensionsResponse
newListExtensionsResponse pHttpStatus_ =
  ListExtensionsResponse'
    { items = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of available extensions. The list includes Amazon Web Services
-- authored and user-created extensions.
listExtensionsResponse_items :: Lens.Lens' ListExtensionsResponse (Prelude.Maybe [ExtensionSummary])
listExtensionsResponse_items = Lens.lens (\ListExtensionsResponse' {items} -> items) (\s@ListExtensionsResponse' {} a -> s {items = a} :: ListExtensionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next set of items to return. Use this token to get the
-- next set of results.
listExtensionsResponse_nextToken :: Lens.Lens' ListExtensionsResponse (Prelude.Maybe Prelude.Text)
listExtensionsResponse_nextToken = Lens.lens (\ListExtensionsResponse' {nextToken} -> nextToken) (\s@ListExtensionsResponse' {} a -> s {nextToken = a} :: ListExtensionsResponse)

-- | The response's http status code.
listExtensionsResponse_httpStatus :: Lens.Lens' ListExtensionsResponse Prelude.Int
listExtensionsResponse_httpStatus = Lens.lens (\ListExtensionsResponse' {httpStatus} -> httpStatus) (\s@ListExtensionsResponse' {} a -> s {httpStatus = a} :: ListExtensionsResponse)

instance Prelude.NFData ListExtensionsResponse where
  rnf ListExtensionsResponse' {..} =
    Prelude.rnf items
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
