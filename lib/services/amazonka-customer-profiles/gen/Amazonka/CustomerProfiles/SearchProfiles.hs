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
-- Module      : Amazonka.CustomerProfiles.SearchProfiles
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Searches for profiles within a specific domain name using name, phone
-- number, email address, account number, or a custom defined index.
module Amazonka.CustomerProfiles.SearchProfiles
  ( -- * Creating a Request
    SearchProfiles (..),
    newSearchProfiles,

    -- * Request Lenses
    searchProfiles_nextToken,
    searchProfiles_maxResults,
    searchProfiles_domainName,
    searchProfiles_keyName,
    searchProfiles_values,

    -- * Destructuring the Response
    SearchProfilesResponse (..),
    newSearchProfilesResponse,

    -- * Response Lenses
    searchProfilesResponse_items,
    searchProfilesResponse_nextToken,
    searchProfilesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.CustomerProfiles.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSearchProfiles' smart constructor.
data SearchProfiles = SearchProfiles'
  { -- | The pagination token from the previous SearchProfiles API call.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of objects returned per page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The unique name of the domain.
    domainName :: Prelude.Text,
    -- | A searchable identifier of a customer profile. The predefined keys you
    -- can use to search include: _account, _profileId, _fullName, _phone,
    -- _email, _ctrContactId, _marketoLeadId, _salesforceAccountId,
    -- _salesforceContactId, _zendeskUserId, _zendeskExternalId,
    -- _serviceNowSystemId.
    keyName :: Prelude.Text,
    -- | A list of key values.
    values :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchProfiles' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'searchProfiles_nextToken' - The pagination token from the previous SearchProfiles API call.
--
-- 'maxResults', 'searchProfiles_maxResults' - The maximum number of objects returned per page.
--
-- 'domainName', 'searchProfiles_domainName' - The unique name of the domain.
--
-- 'keyName', 'searchProfiles_keyName' - A searchable identifier of a customer profile. The predefined keys you
-- can use to search include: _account, _profileId, _fullName, _phone,
-- _email, _ctrContactId, _marketoLeadId, _salesforceAccountId,
-- _salesforceContactId, _zendeskUserId, _zendeskExternalId,
-- _serviceNowSystemId.
--
-- 'values', 'searchProfiles_values' - A list of key values.
newSearchProfiles ::
  -- | 'domainName'
  Prelude.Text ->
  -- | 'keyName'
  Prelude.Text ->
  SearchProfiles
newSearchProfiles pDomainName_ pKeyName_ =
  SearchProfiles'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      domainName = pDomainName_,
      keyName = pKeyName_,
      values = Prelude.mempty
    }

-- | The pagination token from the previous SearchProfiles API call.
searchProfiles_nextToken :: Lens.Lens' SearchProfiles (Prelude.Maybe Prelude.Text)
searchProfiles_nextToken = Lens.lens (\SearchProfiles' {nextToken} -> nextToken) (\s@SearchProfiles' {} a -> s {nextToken = a} :: SearchProfiles)

-- | The maximum number of objects returned per page.
searchProfiles_maxResults :: Lens.Lens' SearchProfiles (Prelude.Maybe Prelude.Natural)
searchProfiles_maxResults = Lens.lens (\SearchProfiles' {maxResults} -> maxResults) (\s@SearchProfiles' {} a -> s {maxResults = a} :: SearchProfiles)

-- | The unique name of the domain.
searchProfiles_domainName :: Lens.Lens' SearchProfiles Prelude.Text
searchProfiles_domainName = Lens.lens (\SearchProfiles' {domainName} -> domainName) (\s@SearchProfiles' {} a -> s {domainName = a} :: SearchProfiles)

-- | A searchable identifier of a customer profile. The predefined keys you
-- can use to search include: _account, _profileId, _fullName, _phone,
-- _email, _ctrContactId, _marketoLeadId, _salesforceAccountId,
-- _salesforceContactId, _zendeskUserId, _zendeskExternalId,
-- _serviceNowSystemId.
searchProfiles_keyName :: Lens.Lens' SearchProfiles Prelude.Text
searchProfiles_keyName = Lens.lens (\SearchProfiles' {keyName} -> keyName) (\s@SearchProfiles' {} a -> s {keyName = a} :: SearchProfiles)

-- | A list of key values.
searchProfiles_values :: Lens.Lens' SearchProfiles [Prelude.Text]
searchProfiles_values = Lens.lens (\SearchProfiles' {values} -> values) (\s@SearchProfiles' {} a -> s {values = a} :: SearchProfiles) Prelude.. Lens.coerced

instance Core.AWSRequest SearchProfiles where
  type
    AWSResponse SearchProfiles =
      SearchProfilesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          SearchProfilesResponse'
            Prelude.<$> (x Core..?> "Items" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SearchProfiles where
  hashWithSalt salt' SearchProfiles' {..} =
    salt' `Prelude.hashWithSalt` values
      `Prelude.hashWithSalt` keyName
      `Prelude.hashWithSalt` domainName
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData SearchProfiles where
  rnf SearchProfiles' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf values
      `Prelude.seq` Prelude.rnf keyName
      `Prelude.seq` Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders SearchProfiles where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON SearchProfiles where
  toJSON SearchProfiles' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("KeyName" Core..= keyName),
            Prelude.Just ("Values" Core..= values)
          ]
      )

instance Core.ToPath SearchProfiles where
  toPath SearchProfiles' {..} =
    Prelude.mconcat
      [ "/domains/",
        Core.toBS domainName,
        "/profiles/search"
      ]

instance Core.ToQuery SearchProfiles where
  toQuery SearchProfiles' {..} =
    Prelude.mconcat
      [ "next-token" Core.=: nextToken,
        "max-results" Core.=: maxResults
      ]

-- | /See:/ 'newSearchProfilesResponse' smart constructor.
data SearchProfilesResponse = SearchProfilesResponse'
  { -- | The list of SearchProfiles instances.
    items :: Prelude.Maybe [Profile],
    -- | The pagination token from the previous SearchProfiles API call.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchProfilesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'searchProfilesResponse_items' - The list of SearchProfiles instances.
--
-- 'nextToken', 'searchProfilesResponse_nextToken' - The pagination token from the previous SearchProfiles API call.
--
-- 'httpStatus', 'searchProfilesResponse_httpStatus' - The response's http status code.
newSearchProfilesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SearchProfilesResponse
newSearchProfilesResponse pHttpStatus_ =
  SearchProfilesResponse'
    { items = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of SearchProfiles instances.
searchProfilesResponse_items :: Lens.Lens' SearchProfilesResponse (Prelude.Maybe [Profile])
searchProfilesResponse_items = Lens.lens (\SearchProfilesResponse' {items} -> items) (\s@SearchProfilesResponse' {} a -> s {items = a} :: SearchProfilesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The pagination token from the previous SearchProfiles API call.
searchProfilesResponse_nextToken :: Lens.Lens' SearchProfilesResponse (Prelude.Maybe Prelude.Text)
searchProfilesResponse_nextToken = Lens.lens (\SearchProfilesResponse' {nextToken} -> nextToken) (\s@SearchProfilesResponse' {} a -> s {nextToken = a} :: SearchProfilesResponse)

-- | The response's http status code.
searchProfilesResponse_httpStatus :: Lens.Lens' SearchProfilesResponse Prelude.Int
searchProfilesResponse_httpStatus = Lens.lens (\SearchProfilesResponse' {httpStatus} -> httpStatus) (\s@SearchProfilesResponse' {} a -> s {httpStatus = a} :: SearchProfilesResponse)

instance Prelude.NFData SearchProfilesResponse where
  rnf SearchProfilesResponse' {..} =
    Prelude.rnf items
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf nextToken
