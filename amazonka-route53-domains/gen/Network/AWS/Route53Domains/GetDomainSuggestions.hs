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
-- Module      : Network.AWS.Route53Domains.GetDomainSuggestions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The GetDomainSuggestions operation returns a list of suggested domain
-- names.
module Network.AWS.Route53Domains.GetDomainSuggestions
  ( -- * Creating a Request
    GetDomainSuggestions (..),
    newGetDomainSuggestions,

    -- * Request Lenses
    getDomainSuggestions_domainName,
    getDomainSuggestions_suggestionCount,
    getDomainSuggestions_onlyAvailable,

    -- * Destructuring the Response
    GetDomainSuggestionsResponse (..),
    newGetDomainSuggestionsResponse,

    -- * Response Lenses
    getDomainSuggestionsResponse_suggestionsList,
    getDomainSuggestionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Route53Domains.Types

-- | /See:/ 'newGetDomainSuggestions' smart constructor.
data GetDomainSuggestions = GetDomainSuggestions'
  { -- | A domain name that you want to use as the basis for a list of possible
    -- domain names. The top-level domain (TLD), such as .com, must be a TLD
    -- that Route 53 supports. For a list of supported TLDs, see
    -- <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/registrar-tld-list.html Domains that You Can Register with Amazon Route 53>
    -- in the /Amazon Route 53 Developer Guide/.
    --
    -- The domain name can contain only the following characters:
    --
    -- -   Letters a through z. Domain names are not case sensitive.
    --
    -- -   Numbers 0 through 9.
    --
    -- -   Hyphen (-). You can\'t specify a hyphen at the beginning or end of a
    --     label.
    --
    -- -   Period (.) to separate the labels in the name, such as the @.@ in
    --     @example.com@.
    --
    -- Internationalized domain names are not supported for some top-level
    -- domains. To determine whether the TLD that you want to use supports
    -- internationalized domain names, see
    -- <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/registrar-tld-list.html Domains that You Can Register with Amazon Route 53>.
    domainName :: Prelude.Text,
    -- | The number of suggested domain names that you want Route 53 to return.
    -- Specify a value between 1 and 50.
    suggestionCount :: Prelude.Int,
    -- | If @OnlyAvailable@ is @true@, Route 53 returns only domain names that
    -- are available. If @OnlyAvailable@ is @false@, Route 53 returns domain
    -- names without checking whether they\'re available to be registered. To
    -- determine whether the domain is available, you can call
    -- @checkDomainAvailability@ for each suggestion.
    onlyAvailable :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDomainSuggestions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'getDomainSuggestions_domainName' - A domain name that you want to use as the basis for a list of possible
-- domain names. The top-level domain (TLD), such as .com, must be a TLD
-- that Route 53 supports. For a list of supported TLDs, see
-- <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/registrar-tld-list.html Domains that You Can Register with Amazon Route 53>
-- in the /Amazon Route 53 Developer Guide/.
--
-- The domain name can contain only the following characters:
--
-- -   Letters a through z. Domain names are not case sensitive.
--
-- -   Numbers 0 through 9.
--
-- -   Hyphen (-). You can\'t specify a hyphen at the beginning or end of a
--     label.
--
-- -   Period (.) to separate the labels in the name, such as the @.@ in
--     @example.com@.
--
-- Internationalized domain names are not supported for some top-level
-- domains. To determine whether the TLD that you want to use supports
-- internationalized domain names, see
-- <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/registrar-tld-list.html Domains that You Can Register with Amazon Route 53>.
--
-- 'suggestionCount', 'getDomainSuggestions_suggestionCount' - The number of suggested domain names that you want Route 53 to return.
-- Specify a value between 1 and 50.
--
-- 'onlyAvailable', 'getDomainSuggestions_onlyAvailable' - If @OnlyAvailable@ is @true@, Route 53 returns only domain names that
-- are available. If @OnlyAvailable@ is @false@, Route 53 returns domain
-- names without checking whether they\'re available to be registered. To
-- determine whether the domain is available, you can call
-- @checkDomainAvailability@ for each suggestion.
newGetDomainSuggestions ::
  -- | 'domainName'
  Prelude.Text ->
  -- | 'suggestionCount'
  Prelude.Int ->
  -- | 'onlyAvailable'
  Prelude.Bool ->
  GetDomainSuggestions
newGetDomainSuggestions
  pDomainName_
  pSuggestionCount_
  pOnlyAvailable_ =
    GetDomainSuggestions'
      { domainName = pDomainName_,
        suggestionCount = pSuggestionCount_,
        onlyAvailable = pOnlyAvailable_
      }

-- | A domain name that you want to use as the basis for a list of possible
-- domain names. The top-level domain (TLD), such as .com, must be a TLD
-- that Route 53 supports. For a list of supported TLDs, see
-- <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/registrar-tld-list.html Domains that You Can Register with Amazon Route 53>
-- in the /Amazon Route 53 Developer Guide/.
--
-- The domain name can contain only the following characters:
--
-- -   Letters a through z. Domain names are not case sensitive.
--
-- -   Numbers 0 through 9.
--
-- -   Hyphen (-). You can\'t specify a hyphen at the beginning or end of a
--     label.
--
-- -   Period (.) to separate the labels in the name, such as the @.@ in
--     @example.com@.
--
-- Internationalized domain names are not supported for some top-level
-- domains. To determine whether the TLD that you want to use supports
-- internationalized domain names, see
-- <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/registrar-tld-list.html Domains that You Can Register with Amazon Route 53>.
getDomainSuggestions_domainName :: Lens.Lens' GetDomainSuggestions Prelude.Text
getDomainSuggestions_domainName = Lens.lens (\GetDomainSuggestions' {domainName} -> domainName) (\s@GetDomainSuggestions' {} a -> s {domainName = a} :: GetDomainSuggestions)

-- | The number of suggested domain names that you want Route 53 to return.
-- Specify a value between 1 and 50.
getDomainSuggestions_suggestionCount :: Lens.Lens' GetDomainSuggestions Prelude.Int
getDomainSuggestions_suggestionCount = Lens.lens (\GetDomainSuggestions' {suggestionCount} -> suggestionCount) (\s@GetDomainSuggestions' {} a -> s {suggestionCount = a} :: GetDomainSuggestions)

-- | If @OnlyAvailable@ is @true@, Route 53 returns only domain names that
-- are available. If @OnlyAvailable@ is @false@, Route 53 returns domain
-- names without checking whether they\'re available to be registered. To
-- determine whether the domain is available, you can call
-- @checkDomainAvailability@ for each suggestion.
getDomainSuggestions_onlyAvailable :: Lens.Lens' GetDomainSuggestions Prelude.Bool
getDomainSuggestions_onlyAvailable = Lens.lens (\GetDomainSuggestions' {onlyAvailable} -> onlyAvailable) (\s@GetDomainSuggestions' {} a -> s {onlyAvailable = a} :: GetDomainSuggestions)

instance Core.AWSRequest GetDomainSuggestions where
  type
    AWSResponse GetDomainSuggestions =
      GetDomainSuggestionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDomainSuggestionsResponse'
            Prelude.<$> ( x Core..?> "SuggestionsList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetDomainSuggestions

instance Prelude.NFData GetDomainSuggestions

instance Core.ToHeaders GetDomainSuggestions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Route53Domains_v20140515.GetDomainSuggestions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetDomainSuggestions where
  toJSON GetDomainSuggestions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("DomainName" Core..= domainName),
            Prelude.Just
              ("SuggestionCount" Core..= suggestionCount),
            Prelude.Just
              ("OnlyAvailable" Core..= onlyAvailable)
          ]
      )

instance Core.ToPath GetDomainSuggestions where
  toPath = Prelude.const "/"

instance Core.ToQuery GetDomainSuggestions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetDomainSuggestionsResponse' smart constructor.
data GetDomainSuggestionsResponse = GetDomainSuggestionsResponse'
  { -- | A list of possible domain names. If you specified @true@ for
    -- @OnlyAvailable@ in the request, the list contains only domains that are
    -- available for registration.
    suggestionsList :: Prelude.Maybe [DomainSuggestion],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDomainSuggestionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'suggestionsList', 'getDomainSuggestionsResponse_suggestionsList' - A list of possible domain names. If you specified @true@ for
-- @OnlyAvailable@ in the request, the list contains only domains that are
-- available for registration.
--
-- 'httpStatus', 'getDomainSuggestionsResponse_httpStatus' - The response's http status code.
newGetDomainSuggestionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetDomainSuggestionsResponse
newGetDomainSuggestionsResponse pHttpStatus_ =
  GetDomainSuggestionsResponse'
    { suggestionsList =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of possible domain names. If you specified @true@ for
-- @OnlyAvailable@ in the request, the list contains only domains that are
-- available for registration.
getDomainSuggestionsResponse_suggestionsList :: Lens.Lens' GetDomainSuggestionsResponse (Prelude.Maybe [DomainSuggestion])
getDomainSuggestionsResponse_suggestionsList = Lens.lens (\GetDomainSuggestionsResponse' {suggestionsList} -> suggestionsList) (\s@GetDomainSuggestionsResponse' {} a -> s {suggestionsList = a} :: GetDomainSuggestionsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getDomainSuggestionsResponse_httpStatus :: Lens.Lens' GetDomainSuggestionsResponse Prelude.Int
getDomainSuggestionsResponse_httpStatus = Lens.lens (\GetDomainSuggestionsResponse' {httpStatus} -> httpStatus) (\s@GetDomainSuggestionsResponse' {} a -> s {httpStatus = a} :: GetDomainSuggestionsResponse)

instance Prelude.NFData GetDomainSuggestionsResponse
