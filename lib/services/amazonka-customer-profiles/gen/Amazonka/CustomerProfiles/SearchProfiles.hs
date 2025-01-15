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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Searches for profiles within a specific domain using one or more
-- predefined search keys (e.g., _fullName, _phone, _email, _account, etc.)
-- and\/or custom-defined search keys. A search key is a data type pair
-- that consists of a @KeyName@ and @Values@ list.
--
-- This operation supports searching for profiles with a minimum of 1
-- key-value(s) pair and up to 5 key-value(s) pairs using either @AND@ or
-- @OR@ logic.
module Amazonka.CustomerProfiles.SearchProfiles
  ( -- * Creating a Request
    SearchProfiles (..),
    newSearchProfiles,

    -- * Request Lenses
    searchProfiles_additionalSearchKeys,
    searchProfiles_logicalOperator,
    searchProfiles_maxResults,
    searchProfiles_nextToken,
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
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CustomerProfiles.Types
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSearchProfiles' smart constructor.
data SearchProfiles = SearchProfiles'
  { -- | A list of @AdditionalSearchKey@ objects that are each searchable
    -- identifiers of a profile. Each @AdditionalSearchKey@ object contains a
    -- @KeyName@ and a list of @Values@ associated with that specific key
    -- (i.e., a key-value(s) pair). These additional search keys will be used
    -- in conjunction with the @LogicalOperator@ and the required @KeyName@ and
    -- @Values@ parameters to search for profiles that satisfy the search
    -- criteria.
    additionalSearchKeys :: Prelude.Maybe (Prelude.NonEmpty AdditionalSearchKey),
    -- | Relationship between all specified search keys that will be used to
    -- search for profiles. This includes the required @KeyName@ and @Values@
    -- parameters as well as any key-value(s) pairs specified in the
    -- @AdditionalSearchKeys@ list.
    --
    -- This parameter influences which profiles will be returned in the
    -- response in the following manner:
    --
    -- -   @AND@ - The response only includes profiles that match all of the
    --     search keys.
    --
    -- -   @OR@ - The response includes profiles that match at least one of the
    --     search keys.
    --
    -- The @OR@ relationship is the default behavior if this parameter is not
    -- included in the request.
    logicalOperator :: Prelude.Maybe LogicalOperator,
    -- | The maximum number of objects returned per page.
    --
    -- The default is 20 if this parameter is not included in the request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The pagination token from the previous SearchProfiles API call.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The unique name of the domain.
    domainName :: Prelude.Text,
    -- | A searchable identifier of a customer profile. The predefined keys you
    -- can use to search include: _account, _profileId, _assetId, _caseId,
    -- _orderId, _fullName, _phone, _email, _ctrContactId, _marketoLeadId,
    -- _salesforceAccountId, _salesforceContactId, _salesforceAssetId,
    -- _zendeskUserId, _zendeskExternalId, _zendeskTicketId,
    -- _serviceNowSystemId, _serviceNowIncidentId, _segmentUserId,
    -- _shopifyCustomerId, _shopifyOrderId.
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
-- 'additionalSearchKeys', 'searchProfiles_additionalSearchKeys' - A list of @AdditionalSearchKey@ objects that are each searchable
-- identifiers of a profile. Each @AdditionalSearchKey@ object contains a
-- @KeyName@ and a list of @Values@ associated with that specific key
-- (i.e., a key-value(s) pair). These additional search keys will be used
-- in conjunction with the @LogicalOperator@ and the required @KeyName@ and
-- @Values@ parameters to search for profiles that satisfy the search
-- criteria.
--
-- 'logicalOperator', 'searchProfiles_logicalOperator' - Relationship between all specified search keys that will be used to
-- search for profiles. This includes the required @KeyName@ and @Values@
-- parameters as well as any key-value(s) pairs specified in the
-- @AdditionalSearchKeys@ list.
--
-- This parameter influences which profiles will be returned in the
-- response in the following manner:
--
-- -   @AND@ - The response only includes profiles that match all of the
--     search keys.
--
-- -   @OR@ - The response includes profiles that match at least one of the
--     search keys.
--
-- The @OR@ relationship is the default behavior if this parameter is not
-- included in the request.
--
-- 'maxResults', 'searchProfiles_maxResults' - The maximum number of objects returned per page.
--
-- The default is 20 if this parameter is not included in the request.
--
-- 'nextToken', 'searchProfiles_nextToken' - The pagination token from the previous SearchProfiles API call.
--
-- 'domainName', 'searchProfiles_domainName' - The unique name of the domain.
--
-- 'keyName', 'searchProfiles_keyName' - A searchable identifier of a customer profile. The predefined keys you
-- can use to search include: _account, _profileId, _assetId, _caseId,
-- _orderId, _fullName, _phone, _email, _ctrContactId, _marketoLeadId,
-- _salesforceAccountId, _salesforceContactId, _salesforceAssetId,
-- _zendeskUserId, _zendeskExternalId, _zendeskTicketId,
-- _serviceNowSystemId, _serviceNowIncidentId, _segmentUserId,
-- _shopifyCustomerId, _shopifyOrderId.
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
    { additionalSearchKeys =
        Prelude.Nothing,
      logicalOperator = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      domainName = pDomainName_,
      keyName = pKeyName_,
      values = Prelude.mempty
    }

-- | A list of @AdditionalSearchKey@ objects that are each searchable
-- identifiers of a profile. Each @AdditionalSearchKey@ object contains a
-- @KeyName@ and a list of @Values@ associated with that specific key
-- (i.e., a key-value(s) pair). These additional search keys will be used
-- in conjunction with the @LogicalOperator@ and the required @KeyName@ and
-- @Values@ parameters to search for profiles that satisfy the search
-- criteria.
searchProfiles_additionalSearchKeys :: Lens.Lens' SearchProfiles (Prelude.Maybe (Prelude.NonEmpty AdditionalSearchKey))
searchProfiles_additionalSearchKeys = Lens.lens (\SearchProfiles' {additionalSearchKeys} -> additionalSearchKeys) (\s@SearchProfiles' {} a -> s {additionalSearchKeys = a} :: SearchProfiles) Prelude.. Lens.mapping Lens.coerced

-- | Relationship between all specified search keys that will be used to
-- search for profiles. This includes the required @KeyName@ and @Values@
-- parameters as well as any key-value(s) pairs specified in the
-- @AdditionalSearchKeys@ list.
--
-- This parameter influences which profiles will be returned in the
-- response in the following manner:
--
-- -   @AND@ - The response only includes profiles that match all of the
--     search keys.
--
-- -   @OR@ - The response includes profiles that match at least one of the
--     search keys.
--
-- The @OR@ relationship is the default behavior if this parameter is not
-- included in the request.
searchProfiles_logicalOperator :: Lens.Lens' SearchProfiles (Prelude.Maybe LogicalOperator)
searchProfiles_logicalOperator = Lens.lens (\SearchProfiles' {logicalOperator} -> logicalOperator) (\s@SearchProfiles' {} a -> s {logicalOperator = a} :: SearchProfiles)

-- | The maximum number of objects returned per page.
--
-- The default is 20 if this parameter is not included in the request.
searchProfiles_maxResults :: Lens.Lens' SearchProfiles (Prelude.Maybe Prelude.Natural)
searchProfiles_maxResults = Lens.lens (\SearchProfiles' {maxResults} -> maxResults) (\s@SearchProfiles' {} a -> s {maxResults = a} :: SearchProfiles)

-- | The pagination token from the previous SearchProfiles API call.
searchProfiles_nextToken :: Lens.Lens' SearchProfiles (Prelude.Maybe Prelude.Text)
searchProfiles_nextToken = Lens.lens (\SearchProfiles' {nextToken} -> nextToken) (\s@SearchProfiles' {} a -> s {nextToken = a} :: SearchProfiles)

-- | The unique name of the domain.
searchProfiles_domainName :: Lens.Lens' SearchProfiles Prelude.Text
searchProfiles_domainName = Lens.lens (\SearchProfiles' {domainName} -> domainName) (\s@SearchProfiles' {} a -> s {domainName = a} :: SearchProfiles)

-- | A searchable identifier of a customer profile. The predefined keys you
-- can use to search include: _account, _profileId, _assetId, _caseId,
-- _orderId, _fullName, _phone, _email, _ctrContactId, _marketoLeadId,
-- _salesforceAccountId, _salesforceContactId, _salesforceAssetId,
-- _zendeskUserId, _zendeskExternalId, _zendeskTicketId,
-- _serviceNowSystemId, _serviceNowIncidentId, _segmentUserId,
-- _shopifyCustomerId, _shopifyOrderId.
searchProfiles_keyName :: Lens.Lens' SearchProfiles Prelude.Text
searchProfiles_keyName = Lens.lens (\SearchProfiles' {keyName} -> keyName) (\s@SearchProfiles' {} a -> s {keyName = a} :: SearchProfiles)

-- | A list of key values.
searchProfiles_values :: Lens.Lens' SearchProfiles [Prelude.Text]
searchProfiles_values = Lens.lens (\SearchProfiles' {values} -> values) (\s@SearchProfiles' {} a -> s {values = a} :: SearchProfiles) Prelude.. Lens.coerced

instance Core.AWSRequest SearchProfiles where
  type
    AWSResponse SearchProfiles =
      SearchProfilesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          SearchProfilesResponse'
            Prelude.<$> (x Data..?> "Items" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SearchProfiles where
  hashWithSalt _salt SearchProfiles' {..} =
    _salt
      `Prelude.hashWithSalt` additionalSearchKeys
      `Prelude.hashWithSalt` logicalOperator
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` domainName
      `Prelude.hashWithSalt` keyName
      `Prelude.hashWithSalt` values

instance Prelude.NFData SearchProfiles where
  rnf SearchProfiles' {..} =
    Prelude.rnf additionalSearchKeys `Prelude.seq`
      Prelude.rnf logicalOperator `Prelude.seq`
        Prelude.rnf maxResults `Prelude.seq`
          Prelude.rnf nextToken `Prelude.seq`
            Prelude.rnf domainName `Prelude.seq`
              Prelude.rnf keyName `Prelude.seq`
                Prelude.rnf values

instance Data.ToHeaders SearchProfiles where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON SearchProfiles where
  toJSON SearchProfiles' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AdditionalSearchKeys" Data..=)
              Prelude.<$> additionalSearchKeys,
            ("LogicalOperator" Data..=)
              Prelude.<$> logicalOperator,
            Prelude.Just ("KeyName" Data..= keyName),
            Prelude.Just ("Values" Data..= values)
          ]
      )

instance Data.ToPath SearchProfiles where
  toPath SearchProfiles' {..} =
    Prelude.mconcat
      [ "/domains/",
        Data.toBS domainName,
        "/profiles/search"
      ]

instance Data.ToQuery SearchProfiles where
  toQuery SearchProfiles' {..} =
    Prelude.mconcat
      [ "max-results" Data.=: maxResults,
        "next-token" Data.=: nextToken
      ]

-- | /See:/ 'newSearchProfilesResponse' smart constructor.
data SearchProfilesResponse = SearchProfilesResponse'
  { -- | The list of Profiles matching the search criteria.
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
-- 'items', 'searchProfilesResponse_items' - The list of Profiles matching the search criteria.
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

-- | The list of Profiles matching the search criteria.
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
    Prelude.rnf items `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf httpStatus
