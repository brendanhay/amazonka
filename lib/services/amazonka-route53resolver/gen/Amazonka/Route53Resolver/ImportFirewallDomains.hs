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
-- Module      : Amazonka.Route53Resolver.ImportFirewallDomains
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Imports domain names from a file into a domain list, for use in a DNS
-- firewall rule group.
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
module Amazonka.Route53Resolver.ImportFirewallDomains
  ( -- * Creating a Request
    ImportFirewallDomains (..),
    newImportFirewallDomains,

    -- * Request Lenses
    importFirewallDomains_firewallDomainListId,
    importFirewallDomains_operation,
    importFirewallDomains_domainFileUrl,

    -- * Destructuring the Response
    ImportFirewallDomainsResponse (..),
    newImportFirewallDomainsResponse,

    -- * Response Lenses
    importFirewallDomainsResponse_id,
    importFirewallDomainsResponse_name,
    importFirewallDomainsResponse_status,
    importFirewallDomainsResponse_statusMessage,
    importFirewallDomainsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53Resolver.Types

-- | /See:/ 'newImportFirewallDomains' smart constructor.
data ImportFirewallDomains = ImportFirewallDomains'
  { -- | The ID of the domain list that you want to modify with the import
    -- operation.
    firewallDomainListId :: Prelude.Text,
    -- | What you want DNS Firewall to do with the domains that are listed in the
    -- file. This must be set to @REPLACE@, which updates the domain list to
    -- exactly match the list in the file.
    operation :: FirewallDomainImportOperation,
    -- | The fully qualified URL or URI of the file stored in Amazon Simple
    -- Storage Service (Amazon S3) that contains the list of domains to import.
    --
    -- The file must be in an S3 bucket that\'s in the same Region as your DNS
    -- Firewall. The file must be a text file and must contain a single domain
    -- per line.
    domainFileUrl :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportFirewallDomains' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'firewallDomainListId', 'importFirewallDomains_firewallDomainListId' - The ID of the domain list that you want to modify with the import
-- operation.
--
-- 'operation', 'importFirewallDomains_operation' - What you want DNS Firewall to do with the domains that are listed in the
-- file. This must be set to @REPLACE@, which updates the domain list to
-- exactly match the list in the file.
--
-- 'domainFileUrl', 'importFirewallDomains_domainFileUrl' - The fully qualified URL or URI of the file stored in Amazon Simple
-- Storage Service (Amazon S3) that contains the list of domains to import.
--
-- The file must be in an S3 bucket that\'s in the same Region as your DNS
-- Firewall. The file must be a text file and must contain a single domain
-- per line.
newImportFirewallDomains ::
  -- | 'firewallDomainListId'
  Prelude.Text ->
  -- | 'operation'
  FirewallDomainImportOperation ->
  -- | 'domainFileUrl'
  Prelude.Text ->
  ImportFirewallDomains
newImportFirewallDomains
  pFirewallDomainListId_
  pOperation_
  pDomainFileUrl_ =
    ImportFirewallDomains'
      { firewallDomainListId =
          pFirewallDomainListId_,
        operation = pOperation_,
        domainFileUrl = pDomainFileUrl_
      }

-- | The ID of the domain list that you want to modify with the import
-- operation.
importFirewallDomains_firewallDomainListId :: Lens.Lens' ImportFirewallDomains Prelude.Text
importFirewallDomains_firewallDomainListId = Lens.lens (\ImportFirewallDomains' {firewallDomainListId} -> firewallDomainListId) (\s@ImportFirewallDomains' {} a -> s {firewallDomainListId = a} :: ImportFirewallDomains)

-- | What you want DNS Firewall to do with the domains that are listed in the
-- file. This must be set to @REPLACE@, which updates the domain list to
-- exactly match the list in the file.
importFirewallDomains_operation :: Lens.Lens' ImportFirewallDomains FirewallDomainImportOperation
importFirewallDomains_operation = Lens.lens (\ImportFirewallDomains' {operation} -> operation) (\s@ImportFirewallDomains' {} a -> s {operation = a} :: ImportFirewallDomains)

-- | The fully qualified URL or URI of the file stored in Amazon Simple
-- Storage Service (Amazon S3) that contains the list of domains to import.
--
-- The file must be in an S3 bucket that\'s in the same Region as your DNS
-- Firewall. The file must be a text file and must contain a single domain
-- per line.
importFirewallDomains_domainFileUrl :: Lens.Lens' ImportFirewallDomains Prelude.Text
importFirewallDomains_domainFileUrl = Lens.lens (\ImportFirewallDomains' {domainFileUrl} -> domainFileUrl) (\s@ImportFirewallDomains' {} a -> s {domainFileUrl = a} :: ImportFirewallDomains)

instance Core.AWSRequest ImportFirewallDomains where
  type
    AWSResponse ImportFirewallDomains =
      ImportFirewallDomainsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ImportFirewallDomainsResponse'
            Prelude.<$> (x Data..?> "Id")
            Prelude.<*> (x Data..?> "Name")
            Prelude.<*> (x Data..?> "Status")
            Prelude.<*> (x Data..?> "StatusMessage")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ImportFirewallDomains where
  hashWithSalt _salt ImportFirewallDomains' {..} =
    _salt
      `Prelude.hashWithSalt` firewallDomainListId
      `Prelude.hashWithSalt` operation
      `Prelude.hashWithSalt` domainFileUrl

instance Prelude.NFData ImportFirewallDomains where
  rnf ImportFirewallDomains' {..} =
    Prelude.rnf firewallDomainListId `Prelude.seq`
      Prelude.rnf operation `Prelude.seq`
        Prelude.rnf domainFileUrl

instance Data.ToHeaders ImportFirewallDomains where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Route53Resolver.ImportFirewallDomains" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ImportFirewallDomains where
  toJSON ImportFirewallDomains' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "FirewallDomainListId"
                  Data..= firewallDomainListId
              ),
            Prelude.Just ("Operation" Data..= operation),
            Prelude.Just
              ("DomainFileUrl" Data..= domainFileUrl)
          ]
      )

instance Data.ToPath ImportFirewallDomains where
  toPath = Prelude.const "/"

instance Data.ToQuery ImportFirewallDomains where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newImportFirewallDomainsResponse' smart constructor.
data ImportFirewallDomainsResponse = ImportFirewallDomainsResponse'
  { -- | The Id of the firewall domain list that DNS Firewall just updated.
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
-- Create a value of 'ImportFirewallDomainsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'importFirewallDomainsResponse_id' - The Id of the firewall domain list that DNS Firewall just updated.
--
-- 'name', 'importFirewallDomainsResponse_name' - The name of the domain list.
--
-- 'status', 'importFirewallDomainsResponse_status' -
--
-- 'statusMessage', 'importFirewallDomainsResponse_statusMessage' - Additional information about the status of the list, if available.
--
-- 'httpStatus', 'importFirewallDomainsResponse_httpStatus' - The response's http status code.
newImportFirewallDomainsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ImportFirewallDomainsResponse
newImportFirewallDomainsResponse pHttpStatus_ =
  ImportFirewallDomainsResponse'
    { id =
        Prelude.Nothing,
      name = Prelude.Nothing,
      status = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Id of the firewall domain list that DNS Firewall just updated.
importFirewallDomainsResponse_id :: Lens.Lens' ImportFirewallDomainsResponse (Prelude.Maybe Prelude.Text)
importFirewallDomainsResponse_id = Lens.lens (\ImportFirewallDomainsResponse' {id} -> id) (\s@ImportFirewallDomainsResponse' {} a -> s {id = a} :: ImportFirewallDomainsResponse)

-- | The name of the domain list.
importFirewallDomainsResponse_name :: Lens.Lens' ImportFirewallDomainsResponse (Prelude.Maybe Prelude.Text)
importFirewallDomainsResponse_name = Lens.lens (\ImportFirewallDomainsResponse' {name} -> name) (\s@ImportFirewallDomainsResponse' {} a -> s {name = a} :: ImportFirewallDomainsResponse)

importFirewallDomainsResponse_status :: Lens.Lens' ImportFirewallDomainsResponse (Prelude.Maybe FirewallDomainListStatus)
importFirewallDomainsResponse_status = Lens.lens (\ImportFirewallDomainsResponse' {status} -> status) (\s@ImportFirewallDomainsResponse' {} a -> s {status = a} :: ImportFirewallDomainsResponse)

-- | Additional information about the status of the list, if available.
importFirewallDomainsResponse_statusMessage :: Lens.Lens' ImportFirewallDomainsResponse (Prelude.Maybe Prelude.Text)
importFirewallDomainsResponse_statusMessage = Lens.lens (\ImportFirewallDomainsResponse' {statusMessage} -> statusMessage) (\s@ImportFirewallDomainsResponse' {} a -> s {statusMessage = a} :: ImportFirewallDomainsResponse)

-- | The response's http status code.
importFirewallDomainsResponse_httpStatus :: Lens.Lens' ImportFirewallDomainsResponse Prelude.Int
importFirewallDomainsResponse_httpStatus = Lens.lens (\ImportFirewallDomainsResponse' {httpStatus} -> httpStatus) (\s@ImportFirewallDomainsResponse' {} a -> s {httpStatus = a} :: ImportFirewallDomainsResponse)

instance Prelude.NFData ImportFirewallDomainsResponse where
  rnf ImportFirewallDomainsResponse' {..} =
    Prelude.rnf id `Prelude.seq`
      Prelude.rnf name `Prelude.seq`
        Prelude.rnf status `Prelude.seq`
          Prelude.rnf statusMessage `Prelude.seq`
            Prelude.rnf httpStatus
