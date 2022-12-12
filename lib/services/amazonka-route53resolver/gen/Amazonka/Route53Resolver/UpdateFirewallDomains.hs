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
-- Module      : Amazonka.Route53Resolver.UpdateFirewallDomains
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the firewall domain list from an array of domain specifications.
module Amazonka.Route53Resolver.UpdateFirewallDomains
  ( -- * Creating a Request
    UpdateFirewallDomains (..),
    newUpdateFirewallDomains,

    -- * Request Lenses
    updateFirewallDomains_firewallDomainListId,
    updateFirewallDomains_operation,
    updateFirewallDomains_domains,

    -- * Destructuring the Response
    UpdateFirewallDomainsResponse (..),
    newUpdateFirewallDomainsResponse,

    -- * Response Lenses
    updateFirewallDomainsResponse_id,
    updateFirewallDomainsResponse_name,
    updateFirewallDomainsResponse_status,
    updateFirewallDomainsResponse_statusMessage,
    updateFirewallDomainsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53Resolver.Types

-- | /See:/ 'newUpdateFirewallDomains' smart constructor.
data UpdateFirewallDomains = UpdateFirewallDomains'
  { -- | The ID of the domain list whose domains you want to update.
    firewallDomainListId :: Prelude.Text,
    -- | What you want DNS Firewall to do with the domains that you are
    -- providing:
    --
    -- -   @ADD@ - Add the domains to the ones that are already in the domain
    --     list.
    --
    -- -   @REMOVE@ - Search the domain list for the domains and remove them
    --     from the list.
    --
    -- -   @REPLACE@ - Update the domain list to exactly match the list that
    --     you are providing.
    operation :: FirewallDomainUpdateOperation,
    -- | A list of domains to use in the update operation.
    --
    -- Each domain specification in your domain list must satisfy the following
    -- requirements:
    --
    -- -   It can optionally start with @*@ (asterisk).
    --
    -- -   With the exception of the optional starting asterisk, it must only
    --     contain the following characters: @A-Z@, @a-z@, @0-9@, @-@ (hyphen).
    --
    -- -   It must be from 1-255 characters in length.
    domains :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateFirewallDomains' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'firewallDomainListId', 'updateFirewallDomains_firewallDomainListId' - The ID of the domain list whose domains you want to update.
--
-- 'operation', 'updateFirewallDomains_operation' - What you want DNS Firewall to do with the domains that you are
-- providing:
--
-- -   @ADD@ - Add the domains to the ones that are already in the domain
--     list.
--
-- -   @REMOVE@ - Search the domain list for the domains and remove them
--     from the list.
--
-- -   @REPLACE@ - Update the domain list to exactly match the list that
--     you are providing.
--
-- 'domains', 'updateFirewallDomains_domains' - A list of domains to use in the update operation.
--
-- Each domain specification in your domain list must satisfy the following
-- requirements:
--
-- -   It can optionally start with @*@ (asterisk).
--
-- -   With the exception of the optional starting asterisk, it must only
--     contain the following characters: @A-Z@, @a-z@, @0-9@, @-@ (hyphen).
--
-- -   It must be from 1-255 characters in length.
newUpdateFirewallDomains ::
  -- | 'firewallDomainListId'
  Prelude.Text ->
  -- | 'operation'
  FirewallDomainUpdateOperation ->
  UpdateFirewallDomains
newUpdateFirewallDomains
  pFirewallDomainListId_
  pOperation_ =
    UpdateFirewallDomains'
      { firewallDomainListId =
          pFirewallDomainListId_,
        operation = pOperation_,
        domains = Prelude.mempty
      }

-- | The ID of the domain list whose domains you want to update.
updateFirewallDomains_firewallDomainListId :: Lens.Lens' UpdateFirewallDomains Prelude.Text
updateFirewallDomains_firewallDomainListId = Lens.lens (\UpdateFirewallDomains' {firewallDomainListId} -> firewallDomainListId) (\s@UpdateFirewallDomains' {} a -> s {firewallDomainListId = a} :: UpdateFirewallDomains)

-- | What you want DNS Firewall to do with the domains that you are
-- providing:
--
-- -   @ADD@ - Add the domains to the ones that are already in the domain
--     list.
--
-- -   @REMOVE@ - Search the domain list for the domains and remove them
--     from the list.
--
-- -   @REPLACE@ - Update the domain list to exactly match the list that
--     you are providing.
updateFirewallDomains_operation :: Lens.Lens' UpdateFirewallDomains FirewallDomainUpdateOperation
updateFirewallDomains_operation = Lens.lens (\UpdateFirewallDomains' {operation} -> operation) (\s@UpdateFirewallDomains' {} a -> s {operation = a} :: UpdateFirewallDomains)

-- | A list of domains to use in the update operation.
--
-- Each domain specification in your domain list must satisfy the following
-- requirements:
--
-- -   It can optionally start with @*@ (asterisk).
--
-- -   With the exception of the optional starting asterisk, it must only
--     contain the following characters: @A-Z@, @a-z@, @0-9@, @-@ (hyphen).
--
-- -   It must be from 1-255 characters in length.
updateFirewallDomains_domains :: Lens.Lens' UpdateFirewallDomains [Prelude.Text]
updateFirewallDomains_domains = Lens.lens (\UpdateFirewallDomains' {domains} -> domains) (\s@UpdateFirewallDomains' {} a -> s {domains = a} :: UpdateFirewallDomains) Prelude.. Lens.coerced

instance Core.AWSRequest UpdateFirewallDomains where
  type
    AWSResponse UpdateFirewallDomains =
      UpdateFirewallDomainsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateFirewallDomainsResponse'
            Prelude.<$> (x Data..?> "Id")
            Prelude.<*> (x Data..?> "Name")
            Prelude.<*> (x Data..?> "Status")
            Prelude.<*> (x Data..?> "StatusMessage")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateFirewallDomains where
  hashWithSalt _salt UpdateFirewallDomains' {..} =
    _salt `Prelude.hashWithSalt` firewallDomainListId
      `Prelude.hashWithSalt` operation
      `Prelude.hashWithSalt` domains

instance Prelude.NFData UpdateFirewallDomains where
  rnf UpdateFirewallDomains' {..} =
    Prelude.rnf firewallDomainListId
      `Prelude.seq` Prelude.rnf operation
      `Prelude.seq` Prelude.rnf domains

instance Data.ToHeaders UpdateFirewallDomains where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Route53Resolver.UpdateFirewallDomains" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateFirewallDomains where
  toJSON UpdateFirewallDomains' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "FirewallDomainListId"
                  Data..= firewallDomainListId
              ),
            Prelude.Just ("Operation" Data..= operation),
            Prelude.Just ("Domains" Data..= domains)
          ]
      )

instance Data.ToPath UpdateFirewallDomains where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateFirewallDomains where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateFirewallDomainsResponse' smart constructor.
data UpdateFirewallDomainsResponse = UpdateFirewallDomainsResponse'
  { -- | The ID of the firewall domain list that DNS Firewall just updated.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name of the domain list.
    name :: Prelude.Maybe Prelude.Text,
    status :: Prelude.Maybe FirewallDomainListStatus,
    -- | Additional information about the status of the list, if available.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateFirewallDomainsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'updateFirewallDomainsResponse_id' - The ID of the firewall domain list that DNS Firewall just updated.
--
-- 'name', 'updateFirewallDomainsResponse_name' - The name of the domain list.
--
-- 'status', 'updateFirewallDomainsResponse_status' -
--
-- 'statusMessage', 'updateFirewallDomainsResponse_statusMessage' - Additional information about the status of the list, if available.
--
-- 'httpStatus', 'updateFirewallDomainsResponse_httpStatus' - The response's http status code.
newUpdateFirewallDomainsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateFirewallDomainsResponse
newUpdateFirewallDomainsResponse pHttpStatus_ =
  UpdateFirewallDomainsResponse'
    { id =
        Prelude.Nothing,
      name = Prelude.Nothing,
      status = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the firewall domain list that DNS Firewall just updated.
updateFirewallDomainsResponse_id :: Lens.Lens' UpdateFirewallDomainsResponse (Prelude.Maybe Prelude.Text)
updateFirewallDomainsResponse_id = Lens.lens (\UpdateFirewallDomainsResponse' {id} -> id) (\s@UpdateFirewallDomainsResponse' {} a -> s {id = a} :: UpdateFirewallDomainsResponse)

-- | The name of the domain list.
updateFirewallDomainsResponse_name :: Lens.Lens' UpdateFirewallDomainsResponse (Prelude.Maybe Prelude.Text)
updateFirewallDomainsResponse_name = Lens.lens (\UpdateFirewallDomainsResponse' {name} -> name) (\s@UpdateFirewallDomainsResponse' {} a -> s {name = a} :: UpdateFirewallDomainsResponse)

-- |
updateFirewallDomainsResponse_status :: Lens.Lens' UpdateFirewallDomainsResponse (Prelude.Maybe FirewallDomainListStatus)
updateFirewallDomainsResponse_status = Lens.lens (\UpdateFirewallDomainsResponse' {status} -> status) (\s@UpdateFirewallDomainsResponse' {} a -> s {status = a} :: UpdateFirewallDomainsResponse)

-- | Additional information about the status of the list, if available.
updateFirewallDomainsResponse_statusMessage :: Lens.Lens' UpdateFirewallDomainsResponse (Prelude.Maybe Prelude.Text)
updateFirewallDomainsResponse_statusMessage = Lens.lens (\UpdateFirewallDomainsResponse' {statusMessage} -> statusMessage) (\s@UpdateFirewallDomainsResponse' {} a -> s {statusMessage = a} :: UpdateFirewallDomainsResponse)

-- | The response's http status code.
updateFirewallDomainsResponse_httpStatus :: Lens.Lens' UpdateFirewallDomainsResponse Prelude.Int
updateFirewallDomainsResponse_httpStatus = Lens.lens (\UpdateFirewallDomainsResponse' {httpStatus} -> httpStatus) (\s@UpdateFirewallDomainsResponse' {} a -> s {httpStatus = a} :: UpdateFirewallDomainsResponse)

instance Prelude.NFData UpdateFirewallDomainsResponse where
  rnf UpdateFirewallDomainsResponse' {..} =
    Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf httpStatus
