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
-- Module      : Amazonka.CodeArtifact.DisassociateExternalConnection
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes an existing external connection from a repository.
module Amazonka.CodeArtifact.DisassociateExternalConnection
  ( -- * Creating a Request
    DisassociateExternalConnection (..),
    newDisassociateExternalConnection,

    -- * Request Lenses
    disassociateExternalConnection_domainOwner,
    disassociateExternalConnection_domain,
    disassociateExternalConnection_repository,
    disassociateExternalConnection_externalConnection,

    -- * Destructuring the Response
    DisassociateExternalConnectionResponse (..),
    newDisassociateExternalConnectionResponse,

    -- * Response Lenses
    disassociateExternalConnectionResponse_repository,
    disassociateExternalConnectionResponse_httpStatus,
  )
where

import Amazonka.CodeArtifact.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisassociateExternalConnection' smart constructor.
data DisassociateExternalConnection = DisassociateExternalConnection'
  { -- | The 12-digit account number of the Amazon Web Services account that owns
    -- the domain. It does not include dashes or spaces.
    domainOwner :: Prelude.Maybe Prelude.Text,
    -- | The name of the domain that contains the repository from which to remove
    -- the external repository.
    domain :: Prelude.Text,
    -- | The name of the repository from which the external connection will be
    -- removed.
    repository :: Prelude.Text,
    -- | The name of the external connection to be removed from the repository.
    externalConnection :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateExternalConnection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainOwner', 'disassociateExternalConnection_domainOwner' - The 12-digit account number of the Amazon Web Services account that owns
-- the domain. It does not include dashes or spaces.
--
-- 'domain', 'disassociateExternalConnection_domain' - The name of the domain that contains the repository from which to remove
-- the external repository.
--
-- 'repository', 'disassociateExternalConnection_repository' - The name of the repository from which the external connection will be
-- removed.
--
-- 'externalConnection', 'disassociateExternalConnection_externalConnection' - The name of the external connection to be removed from the repository.
newDisassociateExternalConnection ::
  -- | 'domain'
  Prelude.Text ->
  -- | 'repository'
  Prelude.Text ->
  -- | 'externalConnection'
  Prelude.Text ->
  DisassociateExternalConnection
newDisassociateExternalConnection
  pDomain_
  pRepository_
  pExternalConnection_ =
    DisassociateExternalConnection'
      { domainOwner =
          Prelude.Nothing,
        domain = pDomain_,
        repository = pRepository_,
        externalConnection = pExternalConnection_
      }

-- | The 12-digit account number of the Amazon Web Services account that owns
-- the domain. It does not include dashes or spaces.
disassociateExternalConnection_domainOwner :: Lens.Lens' DisassociateExternalConnection (Prelude.Maybe Prelude.Text)
disassociateExternalConnection_domainOwner = Lens.lens (\DisassociateExternalConnection' {domainOwner} -> domainOwner) (\s@DisassociateExternalConnection' {} a -> s {domainOwner = a} :: DisassociateExternalConnection)

-- | The name of the domain that contains the repository from which to remove
-- the external repository.
disassociateExternalConnection_domain :: Lens.Lens' DisassociateExternalConnection Prelude.Text
disassociateExternalConnection_domain = Lens.lens (\DisassociateExternalConnection' {domain} -> domain) (\s@DisassociateExternalConnection' {} a -> s {domain = a} :: DisassociateExternalConnection)

-- | The name of the repository from which the external connection will be
-- removed.
disassociateExternalConnection_repository :: Lens.Lens' DisassociateExternalConnection Prelude.Text
disassociateExternalConnection_repository = Lens.lens (\DisassociateExternalConnection' {repository} -> repository) (\s@DisassociateExternalConnection' {} a -> s {repository = a} :: DisassociateExternalConnection)

-- | The name of the external connection to be removed from the repository.
disassociateExternalConnection_externalConnection :: Lens.Lens' DisassociateExternalConnection Prelude.Text
disassociateExternalConnection_externalConnection = Lens.lens (\DisassociateExternalConnection' {externalConnection} -> externalConnection) (\s@DisassociateExternalConnection' {} a -> s {externalConnection = a} :: DisassociateExternalConnection)

instance
  Core.AWSRequest
    DisassociateExternalConnection
  where
  type
    AWSResponse DisassociateExternalConnection =
      DisassociateExternalConnectionResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DisassociateExternalConnectionResponse'
            Prelude.<$> (x Data..?> "repository")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DisassociateExternalConnection
  where
  hashWithSalt
    _salt
    DisassociateExternalConnection' {..} =
      _salt
        `Prelude.hashWithSalt` domainOwner
        `Prelude.hashWithSalt` domain
        `Prelude.hashWithSalt` repository
        `Prelude.hashWithSalt` externalConnection

instance
  Prelude.NFData
    DisassociateExternalConnection
  where
  rnf DisassociateExternalConnection' {..} =
    Prelude.rnf domainOwner `Prelude.seq`
      Prelude.rnf domain `Prelude.seq`
        Prelude.rnf repository `Prelude.seq`
          Prelude.rnf externalConnection

instance
  Data.ToHeaders
    DisassociateExternalConnection
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

instance Data.ToPath DisassociateExternalConnection where
  toPath =
    Prelude.const "/v1/repository/external-connection"

instance Data.ToQuery DisassociateExternalConnection where
  toQuery DisassociateExternalConnection' {..} =
    Prelude.mconcat
      [ "domain-owner" Data.=: domainOwner,
        "domain" Data.=: domain,
        "repository" Data.=: repository,
        "external-connection" Data.=: externalConnection
      ]

-- | /See:/ 'newDisassociateExternalConnectionResponse' smart constructor.
data DisassociateExternalConnectionResponse = DisassociateExternalConnectionResponse'
  { -- | The repository associated with the removed external connection.
    repository :: Prelude.Maybe RepositoryDescription,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateExternalConnectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'repository', 'disassociateExternalConnectionResponse_repository' - The repository associated with the removed external connection.
--
-- 'httpStatus', 'disassociateExternalConnectionResponse_httpStatus' - The response's http status code.
newDisassociateExternalConnectionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisassociateExternalConnectionResponse
newDisassociateExternalConnectionResponse
  pHttpStatus_ =
    DisassociateExternalConnectionResponse'
      { repository =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The repository associated with the removed external connection.
disassociateExternalConnectionResponse_repository :: Lens.Lens' DisassociateExternalConnectionResponse (Prelude.Maybe RepositoryDescription)
disassociateExternalConnectionResponse_repository = Lens.lens (\DisassociateExternalConnectionResponse' {repository} -> repository) (\s@DisassociateExternalConnectionResponse' {} a -> s {repository = a} :: DisassociateExternalConnectionResponse)

-- | The response's http status code.
disassociateExternalConnectionResponse_httpStatus :: Lens.Lens' DisassociateExternalConnectionResponse Prelude.Int
disassociateExternalConnectionResponse_httpStatus = Lens.lens (\DisassociateExternalConnectionResponse' {httpStatus} -> httpStatus) (\s@DisassociateExternalConnectionResponse' {} a -> s {httpStatus = a} :: DisassociateExternalConnectionResponse)

instance
  Prelude.NFData
    DisassociateExternalConnectionResponse
  where
  rnf DisassociateExternalConnectionResponse' {..} =
    Prelude.rnf repository `Prelude.seq`
      Prelude.rnf httpStatus
