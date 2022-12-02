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
-- Module      : Amazonka.ApiGatewayV2.DeleteApi
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an Api resource.
module Amazonka.ApiGatewayV2.DeleteApi
  ( -- * Creating a Request
    DeleteApi (..),
    newDeleteApi,

    -- * Request Lenses
    deleteApi_apiId,

    -- * Destructuring the Response
    DeleteApiResponse (..),
    newDeleteApiResponse,
  )
where

import Amazonka.ApiGatewayV2.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteApi' smart constructor.
data DeleteApi = DeleteApi'
  { -- | The API identifier.
    apiId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteApi' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiId', 'deleteApi_apiId' - The API identifier.
newDeleteApi ::
  -- | 'apiId'
  Prelude.Text ->
  DeleteApi
newDeleteApi pApiId_ = DeleteApi' {apiId = pApiId_}

-- | The API identifier.
deleteApi_apiId :: Lens.Lens' DeleteApi Prelude.Text
deleteApi_apiId = Lens.lens (\DeleteApi' {apiId} -> apiId) (\s@DeleteApi' {} a -> s {apiId = a} :: DeleteApi)

instance Core.AWSRequest DeleteApi where
  type AWSResponse DeleteApi = DeleteApiResponse
  request overrides =
    Request.delete (overrides defaultService)
  response = Response.receiveNull DeleteApiResponse'

instance Prelude.Hashable DeleteApi where
  hashWithSalt _salt DeleteApi' {..} =
    _salt `Prelude.hashWithSalt` apiId

instance Prelude.NFData DeleteApi where
  rnf DeleteApi' {..} = Prelude.rnf apiId

instance Data.ToHeaders DeleteApi where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteApi where
  toPath DeleteApi' {..} =
    Prelude.mconcat ["/v2/apis/", Data.toBS apiId]

instance Data.ToQuery DeleteApi where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteApiResponse' smart constructor.
data DeleteApiResponse = DeleteApiResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteApiResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteApiResponse ::
  DeleteApiResponse
newDeleteApiResponse = DeleteApiResponse'

instance Prelude.NFData DeleteApiResponse where
  rnf _ = ()
