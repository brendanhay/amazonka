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
-- Module      : Amazonka.ApiGatewayV2.DeleteApiMapping
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an API mapping.
module Amazonka.ApiGatewayV2.DeleteApiMapping
  ( -- * Creating a Request
    DeleteApiMapping (..),
    newDeleteApiMapping,

    -- * Request Lenses
    deleteApiMapping_apiMappingId,
    deleteApiMapping_domainName,

    -- * Destructuring the Response
    DeleteApiMappingResponse (..),
    newDeleteApiMappingResponse,
  )
where

import Amazonka.ApiGatewayV2.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteApiMapping' smart constructor.
data DeleteApiMapping = DeleteApiMapping'
  { -- | The API mapping identifier.
    apiMappingId :: Prelude.Text,
    -- | The domain name.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteApiMapping' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiMappingId', 'deleteApiMapping_apiMappingId' - The API mapping identifier.
--
-- 'domainName', 'deleteApiMapping_domainName' - The domain name.
newDeleteApiMapping ::
  -- | 'apiMappingId'
  Prelude.Text ->
  -- | 'domainName'
  Prelude.Text ->
  DeleteApiMapping
newDeleteApiMapping pApiMappingId_ pDomainName_ =
  DeleteApiMapping'
    { apiMappingId = pApiMappingId_,
      domainName = pDomainName_
    }

-- | The API mapping identifier.
deleteApiMapping_apiMappingId :: Lens.Lens' DeleteApiMapping Prelude.Text
deleteApiMapping_apiMappingId = Lens.lens (\DeleteApiMapping' {apiMappingId} -> apiMappingId) (\s@DeleteApiMapping' {} a -> s {apiMappingId = a} :: DeleteApiMapping)

-- | The domain name.
deleteApiMapping_domainName :: Lens.Lens' DeleteApiMapping Prelude.Text
deleteApiMapping_domainName = Lens.lens (\DeleteApiMapping' {domainName} -> domainName) (\s@DeleteApiMapping' {} a -> s {domainName = a} :: DeleteApiMapping)

instance Core.AWSRequest DeleteApiMapping where
  type
    AWSResponse DeleteApiMapping =
      DeleteApiMappingResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull DeleteApiMappingResponse'

instance Prelude.Hashable DeleteApiMapping where
  hashWithSalt _salt DeleteApiMapping' {..} =
    _salt
      `Prelude.hashWithSalt` apiMappingId
      `Prelude.hashWithSalt` domainName

instance Prelude.NFData DeleteApiMapping where
  rnf DeleteApiMapping' {..} =
    Prelude.rnf apiMappingId `Prelude.seq`
      Prelude.rnf domainName

instance Data.ToHeaders DeleteApiMapping where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteApiMapping where
  toPath DeleteApiMapping' {..} =
    Prelude.mconcat
      [ "/v2/domainnames/",
        Data.toBS domainName,
        "/apimappings/",
        Data.toBS apiMappingId
      ]

instance Data.ToQuery DeleteApiMapping where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteApiMappingResponse' smart constructor.
data DeleteApiMappingResponse = DeleteApiMappingResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteApiMappingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteApiMappingResponse ::
  DeleteApiMappingResponse
newDeleteApiMappingResponse =
  DeleteApiMappingResponse'

instance Prelude.NFData DeleteApiMappingResponse where
  rnf _ = ()
