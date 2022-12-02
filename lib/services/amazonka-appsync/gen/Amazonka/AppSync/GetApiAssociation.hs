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
-- Module      : Amazonka.AppSync.GetApiAssociation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves an @ApiAssociation@ object.
module Amazonka.AppSync.GetApiAssociation
  ( -- * Creating a Request
    GetApiAssociation (..),
    newGetApiAssociation,

    -- * Request Lenses
    getApiAssociation_domainName,

    -- * Destructuring the Response
    GetApiAssociationResponse (..),
    newGetApiAssociationResponse,

    -- * Response Lenses
    getApiAssociationResponse_apiAssociation,
    getApiAssociationResponse_httpStatus,
  )
where

import Amazonka.AppSync.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetApiAssociation' smart constructor.
data GetApiAssociation = GetApiAssociation'
  { -- | The domain name.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetApiAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'getApiAssociation_domainName' - The domain name.
newGetApiAssociation ::
  -- | 'domainName'
  Prelude.Text ->
  GetApiAssociation
newGetApiAssociation pDomainName_ =
  GetApiAssociation' {domainName = pDomainName_}

-- | The domain name.
getApiAssociation_domainName :: Lens.Lens' GetApiAssociation Prelude.Text
getApiAssociation_domainName = Lens.lens (\GetApiAssociation' {domainName} -> domainName) (\s@GetApiAssociation' {} a -> s {domainName = a} :: GetApiAssociation)

instance Core.AWSRequest GetApiAssociation where
  type
    AWSResponse GetApiAssociation =
      GetApiAssociationResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetApiAssociationResponse'
            Prelude.<$> (x Data..?> "apiAssociation")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetApiAssociation where
  hashWithSalt _salt GetApiAssociation' {..} =
    _salt `Prelude.hashWithSalt` domainName

instance Prelude.NFData GetApiAssociation where
  rnf GetApiAssociation' {..} = Prelude.rnf domainName

instance Data.ToHeaders GetApiAssociation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetApiAssociation where
  toPath GetApiAssociation' {..} =
    Prelude.mconcat
      [ "/v1/domainnames/",
        Data.toBS domainName,
        "/apiassociation"
      ]

instance Data.ToQuery GetApiAssociation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetApiAssociationResponse' smart constructor.
data GetApiAssociationResponse = GetApiAssociationResponse'
  { -- | The @ApiAssociation@ object.
    apiAssociation :: Prelude.Maybe ApiAssociation,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetApiAssociationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiAssociation', 'getApiAssociationResponse_apiAssociation' - The @ApiAssociation@ object.
--
-- 'httpStatus', 'getApiAssociationResponse_httpStatus' - The response's http status code.
newGetApiAssociationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetApiAssociationResponse
newGetApiAssociationResponse pHttpStatus_ =
  GetApiAssociationResponse'
    { apiAssociation =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @ApiAssociation@ object.
getApiAssociationResponse_apiAssociation :: Lens.Lens' GetApiAssociationResponse (Prelude.Maybe ApiAssociation)
getApiAssociationResponse_apiAssociation = Lens.lens (\GetApiAssociationResponse' {apiAssociation} -> apiAssociation) (\s@GetApiAssociationResponse' {} a -> s {apiAssociation = a} :: GetApiAssociationResponse)

-- | The response's http status code.
getApiAssociationResponse_httpStatus :: Lens.Lens' GetApiAssociationResponse Prelude.Int
getApiAssociationResponse_httpStatus = Lens.lens (\GetApiAssociationResponse' {httpStatus} -> httpStatus) (\s@GetApiAssociationResponse' {} a -> s {httpStatus = a} :: GetApiAssociationResponse)

instance Prelude.NFData GetApiAssociationResponse where
  rnf GetApiAssociationResponse' {..} =
    Prelude.rnf apiAssociation
      `Prelude.seq` Prelude.rnf httpStatus
