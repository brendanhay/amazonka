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
-- Module      : Amazonka.CodeArtifact.AssociateExternalConnection
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds an existing external connection to a repository. One external
-- connection is allowed per repository.
--
-- A repository can have one or more upstream repositories, or an external
-- connection.
module Amazonka.CodeArtifact.AssociateExternalConnection
  ( -- * Creating a Request
    AssociateExternalConnection (..),
    newAssociateExternalConnection,

    -- * Request Lenses
    associateExternalConnection_domainOwner,
    associateExternalConnection_domain,
    associateExternalConnection_repository,
    associateExternalConnection_externalConnection,

    -- * Destructuring the Response
    AssociateExternalConnectionResponse (..),
    newAssociateExternalConnectionResponse,

    -- * Response Lenses
    associateExternalConnectionResponse_repository,
    associateExternalConnectionResponse_httpStatus,
  )
where

import Amazonka.CodeArtifact.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAssociateExternalConnection' smart constructor.
data AssociateExternalConnection = AssociateExternalConnection'
  { -- | The 12-digit account number of the Amazon Web Services account that owns
    -- the domain. It does not include dashes or spaces.
    domainOwner :: Prelude.Maybe Prelude.Text,
    -- | The name of the domain that contains the repository.
    domain :: Prelude.Text,
    -- | The name of the repository to which the external connection is added.
    repository :: Prelude.Text,
    -- | The name of the external connection to add to the repository. The
    -- following values are supported:
    --
    -- -   @public:npmjs@ - for the npm public repository.
    --
    -- -   @public:pypi@ - for the Python Package Index.
    --
    -- -   @public:maven-central@ - for Maven Central.
    --
    -- -   @public:maven-googleandroid@ - for the Google Android repository.
    --
    -- -   @public:maven-gradleplugins@ - for the Gradle plugins repository.
    --
    -- -   @public:maven-commonsware@ - for the CommonsWare Android repository.
    externalConnection :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateExternalConnection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainOwner', 'associateExternalConnection_domainOwner' - The 12-digit account number of the Amazon Web Services account that owns
-- the domain. It does not include dashes or spaces.
--
-- 'domain', 'associateExternalConnection_domain' - The name of the domain that contains the repository.
--
-- 'repository', 'associateExternalConnection_repository' - The name of the repository to which the external connection is added.
--
-- 'externalConnection', 'associateExternalConnection_externalConnection' - The name of the external connection to add to the repository. The
-- following values are supported:
--
-- -   @public:npmjs@ - for the npm public repository.
--
-- -   @public:pypi@ - for the Python Package Index.
--
-- -   @public:maven-central@ - for Maven Central.
--
-- -   @public:maven-googleandroid@ - for the Google Android repository.
--
-- -   @public:maven-gradleplugins@ - for the Gradle plugins repository.
--
-- -   @public:maven-commonsware@ - for the CommonsWare Android repository.
newAssociateExternalConnection ::
  -- | 'domain'
  Prelude.Text ->
  -- | 'repository'
  Prelude.Text ->
  -- | 'externalConnection'
  Prelude.Text ->
  AssociateExternalConnection
newAssociateExternalConnection
  pDomain_
  pRepository_
  pExternalConnection_ =
    AssociateExternalConnection'
      { domainOwner =
          Prelude.Nothing,
        domain = pDomain_,
        repository = pRepository_,
        externalConnection = pExternalConnection_
      }

-- | The 12-digit account number of the Amazon Web Services account that owns
-- the domain. It does not include dashes or spaces.
associateExternalConnection_domainOwner :: Lens.Lens' AssociateExternalConnection (Prelude.Maybe Prelude.Text)
associateExternalConnection_domainOwner = Lens.lens (\AssociateExternalConnection' {domainOwner} -> domainOwner) (\s@AssociateExternalConnection' {} a -> s {domainOwner = a} :: AssociateExternalConnection)

-- | The name of the domain that contains the repository.
associateExternalConnection_domain :: Lens.Lens' AssociateExternalConnection Prelude.Text
associateExternalConnection_domain = Lens.lens (\AssociateExternalConnection' {domain} -> domain) (\s@AssociateExternalConnection' {} a -> s {domain = a} :: AssociateExternalConnection)

-- | The name of the repository to which the external connection is added.
associateExternalConnection_repository :: Lens.Lens' AssociateExternalConnection Prelude.Text
associateExternalConnection_repository = Lens.lens (\AssociateExternalConnection' {repository} -> repository) (\s@AssociateExternalConnection' {} a -> s {repository = a} :: AssociateExternalConnection)

-- | The name of the external connection to add to the repository. The
-- following values are supported:
--
-- -   @public:npmjs@ - for the npm public repository.
--
-- -   @public:pypi@ - for the Python Package Index.
--
-- -   @public:maven-central@ - for Maven Central.
--
-- -   @public:maven-googleandroid@ - for the Google Android repository.
--
-- -   @public:maven-gradleplugins@ - for the Gradle plugins repository.
--
-- -   @public:maven-commonsware@ - for the CommonsWare Android repository.
associateExternalConnection_externalConnection :: Lens.Lens' AssociateExternalConnection Prelude.Text
associateExternalConnection_externalConnection = Lens.lens (\AssociateExternalConnection' {externalConnection} -> externalConnection) (\s@AssociateExternalConnection' {} a -> s {externalConnection = a} :: AssociateExternalConnection)

instance Core.AWSRequest AssociateExternalConnection where
  type
    AWSResponse AssociateExternalConnection =
      AssociateExternalConnectionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AssociateExternalConnectionResponse'
            Prelude.<$> (x Data..?> "repository")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AssociateExternalConnection where
  hashWithSalt _salt AssociateExternalConnection' {..} =
    _salt `Prelude.hashWithSalt` domainOwner
      `Prelude.hashWithSalt` domain
      `Prelude.hashWithSalt` repository
      `Prelude.hashWithSalt` externalConnection

instance Prelude.NFData AssociateExternalConnection where
  rnf AssociateExternalConnection' {..} =
    Prelude.rnf domainOwner
      `Prelude.seq` Prelude.rnf domain
      `Prelude.seq` Prelude.rnf repository
      `Prelude.seq` Prelude.rnf externalConnection

instance Data.ToHeaders AssociateExternalConnection where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AssociateExternalConnection where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath AssociateExternalConnection where
  toPath =
    Prelude.const "/v1/repository/external-connection"

instance Data.ToQuery AssociateExternalConnection where
  toQuery AssociateExternalConnection' {..} =
    Prelude.mconcat
      [ "domain-owner" Data.=: domainOwner,
        "domain" Data.=: domain,
        "repository" Data.=: repository,
        "external-connection" Data.=: externalConnection
      ]

-- | /See:/ 'newAssociateExternalConnectionResponse' smart constructor.
data AssociateExternalConnectionResponse = AssociateExternalConnectionResponse'
  { -- | Information about the connected repository after processing the request.
    repository :: Prelude.Maybe RepositoryDescription,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateExternalConnectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'repository', 'associateExternalConnectionResponse_repository' - Information about the connected repository after processing the request.
--
-- 'httpStatus', 'associateExternalConnectionResponse_httpStatus' - The response's http status code.
newAssociateExternalConnectionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssociateExternalConnectionResponse
newAssociateExternalConnectionResponse pHttpStatus_ =
  AssociateExternalConnectionResponse'
    { repository =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the connected repository after processing the request.
associateExternalConnectionResponse_repository :: Lens.Lens' AssociateExternalConnectionResponse (Prelude.Maybe RepositoryDescription)
associateExternalConnectionResponse_repository = Lens.lens (\AssociateExternalConnectionResponse' {repository} -> repository) (\s@AssociateExternalConnectionResponse' {} a -> s {repository = a} :: AssociateExternalConnectionResponse)

-- | The response's http status code.
associateExternalConnectionResponse_httpStatus :: Lens.Lens' AssociateExternalConnectionResponse Prelude.Int
associateExternalConnectionResponse_httpStatus = Lens.lens (\AssociateExternalConnectionResponse' {httpStatus} -> httpStatus) (\s@AssociateExternalConnectionResponse' {} a -> s {httpStatus = a} :: AssociateExternalConnectionResponse)

instance
  Prelude.NFData
    AssociateExternalConnectionResponse
  where
  rnf AssociateExternalConnectionResponse' {..} =
    Prelude.rnf repository
      `Prelude.seq` Prelude.rnf httpStatus
