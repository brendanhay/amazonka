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
-- Module      : Amazonka.AppConfig.ListExtensionAssociations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all AppConfig extension associations in the account. For more
-- information about extensions and associations, see
-- <https://docs.aws.amazon.com/appconfig/latest/userguide/working-with-appconfig-extensions.html Working with AppConfig extensions>
-- in the /AppConfig User Guide/.
module Amazonka.AppConfig.ListExtensionAssociations
  ( -- * Creating a Request
    ListExtensionAssociations (..),
    newListExtensionAssociations,

    -- * Request Lenses
    listExtensionAssociations_extensionIdentifier,
    listExtensionAssociations_extensionVersionNumber,
    listExtensionAssociations_maxResults,
    listExtensionAssociations_nextToken,
    listExtensionAssociations_resourceIdentifier,

    -- * Destructuring the Response
    ListExtensionAssociationsResponse (..),
    newListExtensionAssociationsResponse,

    -- * Response Lenses
    listExtensionAssociationsResponse_items,
    listExtensionAssociationsResponse_nextToken,
    listExtensionAssociationsResponse_httpStatus,
  )
where

import Amazonka.AppConfig.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListExtensionAssociations' smart constructor.
data ListExtensionAssociations = ListExtensionAssociations'
  { -- | The name, the ID, or the Amazon Resource Name (ARN) of the extension.
    extensionIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The version number for the extension defined in the association.
    extensionVersionNumber :: Prelude.Maybe Prelude.Int,
    -- | The maximum number of items to return for this call. The call also
    -- returns a token that you can specify in a subsequent call to get the
    -- next set of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A token to start the list. Use this token to get the next set of results
    -- or pass null to get the first set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ARN of an application, configuration profile, or environment.
    resourceIdentifier :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListExtensionAssociations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'extensionIdentifier', 'listExtensionAssociations_extensionIdentifier' - The name, the ID, or the Amazon Resource Name (ARN) of the extension.
--
-- 'extensionVersionNumber', 'listExtensionAssociations_extensionVersionNumber' - The version number for the extension defined in the association.
--
-- 'maxResults', 'listExtensionAssociations_maxResults' - The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
--
-- 'nextToken', 'listExtensionAssociations_nextToken' - A token to start the list. Use this token to get the next set of results
-- or pass null to get the first set of results.
--
-- 'resourceIdentifier', 'listExtensionAssociations_resourceIdentifier' - The ARN of an application, configuration profile, or environment.
newListExtensionAssociations ::
  ListExtensionAssociations
newListExtensionAssociations =
  ListExtensionAssociations'
    { extensionIdentifier =
        Prelude.Nothing,
      extensionVersionNumber = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      resourceIdentifier = Prelude.Nothing
    }

-- | The name, the ID, or the Amazon Resource Name (ARN) of the extension.
listExtensionAssociations_extensionIdentifier :: Lens.Lens' ListExtensionAssociations (Prelude.Maybe Prelude.Text)
listExtensionAssociations_extensionIdentifier = Lens.lens (\ListExtensionAssociations' {extensionIdentifier} -> extensionIdentifier) (\s@ListExtensionAssociations' {} a -> s {extensionIdentifier = a} :: ListExtensionAssociations)

-- | The version number for the extension defined in the association.
listExtensionAssociations_extensionVersionNumber :: Lens.Lens' ListExtensionAssociations (Prelude.Maybe Prelude.Int)
listExtensionAssociations_extensionVersionNumber = Lens.lens (\ListExtensionAssociations' {extensionVersionNumber} -> extensionVersionNumber) (\s@ListExtensionAssociations' {} a -> s {extensionVersionNumber = a} :: ListExtensionAssociations)

-- | The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
listExtensionAssociations_maxResults :: Lens.Lens' ListExtensionAssociations (Prelude.Maybe Prelude.Natural)
listExtensionAssociations_maxResults = Lens.lens (\ListExtensionAssociations' {maxResults} -> maxResults) (\s@ListExtensionAssociations' {} a -> s {maxResults = a} :: ListExtensionAssociations)

-- | A token to start the list. Use this token to get the next set of results
-- or pass null to get the first set of results.
listExtensionAssociations_nextToken :: Lens.Lens' ListExtensionAssociations (Prelude.Maybe Prelude.Text)
listExtensionAssociations_nextToken = Lens.lens (\ListExtensionAssociations' {nextToken} -> nextToken) (\s@ListExtensionAssociations' {} a -> s {nextToken = a} :: ListExtensionAssociations)

-- | The ARN of an application, configuration profile, or environment.
listExtensionAssociations_resourceIdentifier :: Lens.Lens' ListExtensionAssociations (Prelude.Maybe Prelude.Text)
listExtensionAssociations_resourceIdentifier = Lens.lens (\ListExtensionAssociations' {resourceIdentifier} -> resourceIdentifier) (\s@ListExtensionAssociations' {} a -> s {resourceIdentifier = a} :: ListExtensionAssociations)

instance Core.AWSRequest ListExtensionAssociations where
  type
    AWSResponse ListExtensionAssociations =
      ListExtensionAssociationsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListExtensionAssociationsResponse'
            Prelude.<$> (x Data..?> "Items" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListExtensionAssociations where
  hashWithSalt _salt ListExtensionAssociations' {..} =
    _salt
      `Prelude.hashWithSalt` extensionIdentifier
      `Prelude.hashWithSalt` extensionVersionNumber
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` resourceIdentifier

instance Prelude.NFData ListExtensionAssociations where
  rnf ListExtensionAssociations' {..} =
    Prelude.rnf extensionIdentifier
      `Prelude.seq` Prelude.rnf extensionVersionNumber
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf resourceIdentifier

instance Data.ToHeaders ListExtensionAssociations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListExtensionAssociations where
  toPath = Prelude.const "/extensionassociations"

instance Data.ToQuery ListExtensionAssociations where
  toQuery ListExtensionAssociations' {..} =
    Prelude.mconcat
      [ "extension_identifier" Data.=: extensionIdentifier,
        "extension_version_number"
          Data.=: extensionVersionNumber,
        "max_results" Data.=: maxResults,
        "next_token" Data.=: nextToken,
        "resource_identifier" Data.=: resourceIdentifier
      ]

-- | /See:/ 'newListExtensionAssociationsResponse' smart constructor.
data ListExtensionAssociationsResponse = ListExtensionAssociationsResponse'
  { -- | The list of extension associations. Each item represents an extension
    -- association to an application, environment, or configuration profile.
    items :: Prelude.Maybe [ExtensionAssociationSummary],
    -- | The token for the next set of items to return. Use this token to get the
    -- next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListExtensionAssociationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'listExtensionAssociationsResponse_items' - The list of extension associations. Each item represents an extension
-- association to an application, environment, or configuration profile.
--
-- 'nextToken', 'listExtensionAssociationsResponse_nextToken' - The token for the next set of items to return. Use this token to get the
-- next set of results.
--
-- 'httpStatus', 'listExtensionAssociationsResponse_httpStatus' - The response's http status code.
newListExtensionAssociationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListExtensionAssociationsResponse
newListExtensionAssociationsResponse pHttpStatus_ =
  ListExtensionAssociationsResponse'
    { items =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of extension associations. Each item represents an extension
-- association to an application, environment, or configuration profile.
listExtensionAssociationsResponse_items :: Lens.Lens' ListExtensionAssociationsResponse (Prelude.Maybe [ExtensionAssociationSummary])
listExtensionAssociationsResponse_items = Lens.lens (\ListExtensionAssociationsResponse' {items} -> items) (\s@ListExtensionAssociationsResponse' {} a -> s {items = a} :: ListExtensionAssociationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next set of items to return. Use this token to get the
-- next set of results.
listExtensionAssociationsResponse_nextToken :: Lens.Lens' ListExtensionAssociationsResponse (Prelude.Maybe Prelude.Text)
listExtensionAssociationsResponse_nextToken = Lens.lens (\ListExtensionAssociationsResponse' {nextToken} -> nextToken) (\s@ListExtensionAssociationsResponse' {} a -> s {nextToken = a} :: ListExtensionAssociationsResponse)

-- | The response's http status code.
listExtensionAssociationsResponse_httpStatus :: Lens.Lens' ListExtensionAssociationsResponse Prelude.Int
listExtensionAssociationsResponse_httpStatus = Lens.lens (\ListExtensionAssociationsResponse' {httpStatus} -> httpStatus) (\s@ListExtensionAssociationsResponse' {} a -> s {httpStatus = a} :: ListExtensionAssociationsResponse)

instance
  Prelude.NFData
    ListExtensionAssociationsResponse
  where
  rnf ListExtensionAssociationsResponse' {..} =
    Prelude.rnf items
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
