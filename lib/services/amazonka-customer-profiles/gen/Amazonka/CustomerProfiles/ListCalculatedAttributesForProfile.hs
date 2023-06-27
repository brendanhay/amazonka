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
-- Module      : Amazonka.CustomerProfiles.ListCalculatedAttributesForProfile
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieve a list of calculated attributes for a customer profile.
module Amazonka.CustomerProfiles.ListCalculatedAttributesForProfile
  ( -- * Creating a Request
    ListCalculatedAttributesForProfile (..),
    newListCalculatedAttributesForProfile,

    -- * Request Lenses
    listCalculatedAttributesForProfile_maxResults,
    listCalculatedAttributesForProfile_nextToken,
    listCalculatedAttributesForProfile_domainName,
    listCalculatedAttributesForProfile_profileId,

    -- * Destructuring the Response
    ListCalculatedAttributesForProfileResponse (..),
    newListCalculatedAttributesForProfileResponse,

    -- * Response Lenses
    listCalculatedAttributesForProfileResponse_items,
    listCalculatedAttributesForProfileResponse_nextToken,
    listCalculatedAttributesForProfileResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CustomerProfiles.Types
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListCalculatedAttributesForProfile' smart constructor.
data ListCalculatedAttributesForProfile = ListCalculatedAttributesForProfile'
  { -- | The maximum number of calculated attributes returned per page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The pagination token from the previous call to
    -- ListCalculatedAttributesForProfile.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The unique name of the domain.
    domainName :: Prelude.Text,
    -- | The unique identifier of a customer profile.
    profileId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCalculatedAttributesForProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listCalculatedAttributesForProfile_maxResults' - The maximum number of calculated attributes returned per page.
--
-- 'nextToken', 'listCalculatedAttributesForProfile_nextToken' - The pagination token from the previous call to
-- ListCalculatedAttributesForProfile.
--
-- 'domainName', 'listCalculatedAttributesForProfile_domainName' - The unique name of the domain.
--
-- 'profileId', 'listCalculatedAttributesForProfile_profileId' - The unique identifier of a customer profile.
newListCalculatedAttributesForProfile ::
  -- | 'domainName'
  Prelude.Text ->
  -- | 'profileId'
  Prelude.Text ->
  ListCalculatedAttributesForProfile
newListCalculatedAttributesForProfile
  pDomainName_
  pProfileId_ =
    ListCalculatedAttributesForProfile'
      { maxResults =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        domainName = pDomainName_,
        profileId = pProfileId_
      }

-- | The maximum number of calculated attributes returned per page.
listCalculatedAttributesForProfile_maxResults :: Lens.Lens' ListCalculatedAttributesForProfile (Prelude.Maybe Prelude.Natural)
listCalculatedAttributesForProfile_maxResults = Lens.lens (\ListCalculatedAttributesForProfile' {maxResults} -> maxResults) (\s@ListCalculatedAttributesForProfile' {} a -> s {maxResults = a} :: ListCalculatedAttributesForProfile)

-- | The pagination token from the previous call to
-- ListCalculatedAttributesForProfile.
listCalculatedAttributesForProfile_nextToken :: Lens.Lens' ListCalculatedAttributesForProfile (Prelude.Maybe Prelude.Text)
listCalculatedAttributesForProfile_nextToken = Lens.lens (\ListCalculatedAttributesForProfile' {nextToken} -> nextToken) (\s@ListCalculatedAttributesForProfile' {} a -> s {nextToken = a} :: ListCalculatedAttributesForProfile)

-- | The unique name of the domain.
listCalculatedAttributesForProfile_domainName :: Lens.Lens' ListCalculatedAttributesForProfile Prelude.Text
listCalculatedAttributesForProfile_domainName = Lens.lens (\ListCalculatedAttributesForProfile' {domainName} -> domainName) (\s@ListCalculatedAttributesForProfile' {} a -> s {domainName = a} :: ListCalculatedAttributesForProfile)

-- | The unique identifier of a customer profile.
listCalculatedAttributesForProfile_profileId :: Lens.Lens' ListCalculatedAttributesForProfile Prelude.Text
listCalculatedAttributesForProfile_profileId = Lens.lens (\ListCalculatedAttributesForProfile' {profileId} -> profileId) (\s@ListCalculatedAttributesForProfile' {} a -> s {profileId = a} :: ListCalculatedAttributesForProfile)

instance
  Core.AWSRequest
    ListCalculatedAttributesForProfile
  where
  type
    AWSResponse ListCalculatedAttributesForProfile =
      ListCalculatedAttributesForProfileResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListCalculatedAttributesForProfileResponse'
            Prelude.<$> (x Data..?> "Items" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListCalculatedAttributesForProfile
  where
  hashWithSalt
    _salt
    ListCalculatedAttributesForProfile' {..} =
      _salt
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` domainName
        `Prelude.hashWithSalt` profileId

instance
  Prelude.NFData
    ListCalculatedAttributesForProfile
  where
  rnf ListCalculatedAttributesForProfile' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf profileId

instance
  Data.ToHeaders
    ListCalculatedAttributesForProfile
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToPath
    ListCalculatedAttributesForProfile
  where
  toPath ListCalculatedAttributesForProfile' {..} =
    Prelude.mconcat
      [ "/domains/",
        Data.toBS domainName,
        "/profile/",
        Data.toBS profileId,
        "/calculated-attributes"
      ]

instance
  Data.ToQuery
    ListCalculatedAttributesForProfile
  where
  toQuery ListCalculatedAttributesForProfile' {..} =
    Prelude.mconcat
      [ "max-results" Data.=: maxResults,
        "next-token" Data.=: nextToken
      ]

-- | /See:/ 'newListCalculatedAttributesForProfileResponse' smart constructor.
data ListCalculatedAttributesForProfileResponse = ListCalculatedAttributesForProfileResponse'
  { -- | The list of calculated attributes.
    items :: Prelude.Maybe [ListCalculatedAttributeForProfileItem],
    -- | The pagination token from the previous call to
    -- ListCalculatedAttributesForProfile.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCalculatedAttributesForProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'listCalculatedAttributesForProfileResponse_items' - The list of calculated attributes.
--
-- 'nextToken', 'listCalculatedAttributesForProfileResponse_nextToken' - The pagination token from the previous call to
-- ListCalculatedAttributesForProfile.
--
-- 'httpStatus', 'listCalculatedAttributesForProfileResponse_httpStatus' - The response's http status code.
newListCalculatedAttributesForProfileResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListCalculatedAttributesForProfileResponse
newListCalculatedAttributesForProfileResponse
  pHttpStatus_ =
    ListCalculatedAttributesForProfileResponse'
      { items =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The list of calculated attributes.
listCalculatedAttributesForProfileResponse_items :: Lens.Lens' ListCalculatedAttributesForProfileResponse (Prelude.Maybe [ListCalculatedAttributeForProfileItem])
listCalculatedAttributesForProfileResponse_items = Lens.lens (\ListCalculatedAttributesForProfileResponse' {items} -> items) (\s@ListCalculatedAttributesForProfileResponse' {} a -> s {items = a} :: ListCalculatedAttributesForProfileResponse) Prelude.. Lens.mapping Lens.coerced

-- | The pagination token from the previous call to
-- ListCalculatedAttributesForProfile.
listCalculatedAttributesForProfileResponse_nextToken :: Lens.Lens' ListCalculatedAttributesForProfileResponse (Prelude.Maybe Prelude.Text)
listCalculatedAttributesForProfileResponse_nextToken = Lens.lens (\ListCalculatedAttributesForProfileResponse' {nextToken} -> nextToken) (\s@ListCalculatedAttributesForProfileResponse' {} a -> s {nextToken = a} :: ListCalculatedAttributesForProfileResponse)

-- | The response's http status code.
listCalculatedAttributesForProfileResponse_httpStatus :: Lens.Lens' ListCalculatedAttributesForProfileResponse Prelude.Int
listCalculatedAttributesForProfileResponse_httpStatus = Lens.lens (\ListCalculatedAttributesForProfileResponse' {httpStatus} -> httpStatus) (\s@ListCalculatedAttributesForProfileResponse' {} a -> s {httpStatus = a} :: ListCalculatedAttributesForProfileResponse)

instance
  Prelude.NFData
    ListCalculatedAttributesForProfileResponse
  where
  rnf ListCalculatedAttributesForProfileResponse' {..} =
    Prelude.rnf items
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
