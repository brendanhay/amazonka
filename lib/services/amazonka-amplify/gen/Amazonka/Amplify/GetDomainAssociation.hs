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
-- Module      : Amazonka.Amplify.GetDomainAssociation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the domain information for an Amplify app.
module Amazonka.Amplify.GetDomainAssociation
  ( -- * Creating a Request
    GetDomainAssociation (..),
    newGetDomainAssociation,

    -- * Request Lenses
    getDomainAssociation_appId,
    getDomainAssociation_domainName,

    -- * Destructuring the Response
    GetDomainAssociationResponse (..),
    newGetDomainAssociationResponse,

    -- * Response Lenses
    getDomainAssociationResponse_httpStatus,
    getDomainAssociationResponse_domainAssociation,
  )
where

import Amazonka.Amplify.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The request structure for the get domain association request.
--
-- /See:/ 'newGetDomainAssociation' smart constructor.
data GetDomainAssociation = GetDomainAssociation'
  { -- | The unique id for an Amplify app.
    appId :: Prelude.Text,
    -- | The name of the domain.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDomainAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appId', 'getDomainAssociation_appId' - The unique id for an Amplify app.
--
-- 'domainName', 'getDomainAssociation_domainName' - The name of the domain.
newGetDomainAssociation ::
  -- | 'appId'
  Prelude.Text ->
  -- | 'domainName'
  Prelude.Text ->
  GetDomainAssociation
newGetDomainAssociation pAppId_ pDomainName_ =
  GetDomainAssociation'
    { appId = pAppId_,
      domainName = pDomainName_
    }

-- | The unique id for an Amplify app.
getDomainAssociation_appId :: Lens.Lens' GetDomainAssociation Prelude.Text
getDomainAssociation_appId = Lens.lens (\GetDomainAssociation' {appId} -> appId) (\s@GetDomainAssociation' {} a -> s {appId = a} :: GetDomainAssociation)

-- | The name of the domain.
getDomainAssociation_domainName :: Lens.Lens' GetDomainAssociation Prelude.Text
getDomainAssociation_domainName = Lens.lens (\GetDomainAssociation' {domainName} -> domainName) (\s@GetDomainAssociation' {} a -> s {domainName = a} :: GetDomainAssociation)

instance Core.AWSRequest GetDomainAssociation where
  type
    AWSResponse GetDomainAssociation =
      GetDomainAssociationResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDomainAssociationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "domainAssociation")
      )

instance Prelude.Hashable GetDomainAssociation where
  hashWithSalt _salt GetDomainAssociation' {..} =
    _salt
      `Prelude.hashWithSalt` appId
      `Prelude.hashWithSalt` domainName

instance Prelude.NFData GetDomainAssociation where
  rnf GetDomainAssociation' {..} =
    Prelude.rnf appId `Prelude.seq`
      Prelude.rnf domainName

instance Data.ToHeaders GetDomainAssociation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetDomainAssociation where
  toPath GetDomainAssociation' {..} =
    Prelude.mconcat
      [ "/apps/",
        Data.toBS appId,
        "/domains/",
        Data.toBS domainName
      ]

instance Data.ToQuery GetDomainAssociation where
  toQuery = Prelude.const Prelude.mempty

-- | The result structure for the get domain association request.
--
-- /See:/ 'newGetDomainAssociationResponse' smart constructor.
data GetDomainAssociationResponse = GetDomainAssociationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Describes the structure of a domain association, which associates a
    -- custom domain with an Amplify app.
    domainAssociation :: DomainAssociation
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDomainAssociationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getDomainAssociationResponse_httpStatus' - The response's http status code.
--
-- 'domainAssociation', 'getDomainAssociationResponse_domainAssociation' - Describes the structure of a domain association, which associates a
-- custom domain with an Amplify app.
newGetDomainAssociationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'domainAssociation'
  DomainAssociation ->
  GetDomainAssociationResponse
newGetDomainAssociationResponse
  pHttpStatus_
  pDomainAssociation_ =
    GetDomainAssociationResponse'
      { httpStatus =
          pHttpStatus_,
        domainAssociation = pDomainAssociation_
      }

-- | The response's http status code.
getDomainAssociationResponse_httpStatus :: Lens.Lens' GetDomainAssociationResponse Prelude.Int
getDomainAssociationResponse_httpStatus = Lens.lens (\GetDomainAssociationResponse' {httpStatus} -> httpStatus) (\s@GetDomainAssociationResponse' {} a -> s {httpStatus = a} :: GetDomainAssociationResponse)

-- | Describes the structure of a domain association, which associates a
-- custom domain with an Amplify app.
getDomainAssociationResponse_domainAssociation :: Lens.Lens' GetDomainAssociationResponse DomainAssociation
getDomainAssociationResponse_domainAssociation = Lens.lens (\GetDomainAssociationResponse' {domainAssociation} -> domainAssociation) (\s@GetDomainAssociationResponse' {} a -> s {domainAssociation = a} :: GetDomainAssociationResponse)

instance Prelude.NFData GetDomainAssociationResponse where
  rnf GetDomainAssociationResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf domainAssociation
