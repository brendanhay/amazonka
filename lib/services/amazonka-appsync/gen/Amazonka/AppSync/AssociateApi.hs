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
-- Module      : Amazonka.AppSync.AssociateApi
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Maps an endpoint to your custom domain.
module Amazonka.AppSync.AssociateApi
  ( -- * Creating a Request
    AssociateApi (..),
    newAssociateApi,

    -- * Request Lenses
    associateApi_domainName,
    associateApi_apiId,

    -- * Destructuring the Response
    AssociateApiResponse (..),
    newAssociateApiResponse,

    -- * Response Lenses
    associateApiResponse_apiAssociation,
    associateApiResponse_httpStatus,
  )
where

import Amazonka.AppSync.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAssociateApi' smart constructor.
data AssociateApi = AssociateApi'
  { -- | The domain name.
    domainName :: Prelude.Text,
    -- | The API ID. Private APIs can not be associated with custom domains.
    apiId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateApi' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'associateApi_domainName' - The domain name.
--
-- 'apiId', 'associateApi_apiId' - The API ID. Private APIs can not be associated with custom domains.
newAssociateApi ::
  -- | 'domainName'
  Prelude.Text ->
  -- | 'apiId'
  Prelude.Text ->
  AssociateApi
newAssociateApi pDomainName_ pApiId_ =
  AssociateApi'
    { domainName = pDomainName_,
      apiId = pApiId_
    }

-- | The domain name.
associateApi_domainName :: Lens.Lens' AssociateApi Prelude.Text
associateApi_domainName = Lens.lens (\AssociateApi' {domainName} -> domainName) (\s@AssociateApi' {} a -> s {domainName = a} :: AssociateApi)

-- | The API ID. Private APIs can not be associated with custom domains.
associateApi_apiId :: Lens.Lens' AssociateApi Prelude.Text
associateApi_apiId = Lens.lens (\AssociateApi' {apiId} -> apiId) (\s@AssociateApi' {} a -> s {apiId = a} :: AssociateApi)

instance Core.AWSRequest AssociateApi where
  type AWSResponse AssociateApi = AssociateApiResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AssociateApiResponse'
            Prelude.<$> (x Data..?> "apiAssociation")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AssociateApi where
  hashWithSalt _salt AssociateApi' {..} =
    _salt
      `Prelude.hashWithSalt` domainName
      `Prelude.hashWithSalt` apiId

instance Prelude.NFData AssociateApi where
  rnf AssociateApi' {..} =
    Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf apiId

instance Data.ToHeaders AssociateApi where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AssociateApi where
  toJSON AssociateApi' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("apiId" Data..= apiId)]
      )

instance Data.ToPath AssociateApi where
  toPath AssociateApi' {..} =
    Prelude.mconcat
      [ "/v1/domainnames/",
        Data.toBS domainName,
        "/apiassociation"
      ]

instance Data.ToQuery AssociateApi where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateApiResponse' smart constructor.
data AssociateApiResponse = AssociateApiResponse'
  { -- | The @ApiAssociation@ object.
    apiAssociation :: Prelude.Maybe ApiAssociation,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateApiResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiAssociation', 'associateApiResponse_apiAssociation' - The @ApiAssociation@ object.
--
-- 'httpStatus', 'associateApiResponse_httpStatus' - The response's http status code.
newAssociateApiResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssociateApiResponse
newAssociateApiResponse pHttpStatus_ =
  AssociateApiResponse'
    { apiAssociation =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @ApiAssociation@ object.
associateApiResponse_apiAssociation :: Lens.Lens' AssociateApiResponse (Prelude.Maybe ApiAssociation)
associateApiResponse_apiAssociation = Lens.lens (\AssociateApiResponse' {apiAssociation} -> apiAssociation) (\s@AssociateApiResponse' {} a -> s {apiAssociation = a} :: AssociateApiResponse)

-- | The response's http status code.
associateApiResponse_httpStatus :: Lens.Lens' AssociateApiResponse Prelude.Int
associateApiResponse_httpStatus = Lens.lens (\AssociateApiResponse' {httpStatus} -> httpStatus) (\s@AssociateApiResponse' {} a -> s {httpStatus = a} :: AssociateApiResponse)

instance Prelude.NFData AssociateApiResponse where
  rnf AssociateApiResponse' {..} =
    Prelude.rnf apiAssociation
      `Prelude.seq` Prelude.rnf httpStatus
