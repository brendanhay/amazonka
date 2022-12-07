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
-- Module      : Amazonka.APIGateway.DeleteRestApi
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified API.
module Amazonka.APIGateway.DeleteRestApi
  ( -- * Creating a Request
    DeleteRestApi (..),
    newDeleteRestApi,

    -- * Request Lenses
    deleteRestApi_restApiId,

    -- * Destructuring the Response
    DeleteRestApiResponse (..),
    newDeleteRestApiResponse,
  )
where

import Amazonka.APIGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Request to delete the specified API from your collection.
--
-- /See:/ 'newDeleteRestApi' smart constructor.
data DeleteRestApi = DeleteRestApi'
  { -- | The string identifier of the associated RestApi.
    restApiId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRestApi' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'restApiId', 'deleteRestApi_restApiId' - The string identifier of the associated RestApi.
newDeleteRestApi ::
  -- | 'restApiId'
  Prelude.Text ->
  DeleteRestApi
newDeleteRestApi pRestApiId_ =
  DeleteRestApi' {restApiId = pRestApiId_}

-- | The string identifier of the associated RestApi.
deleteRestApi_restApiId :: Lens.Lens' DeleteRestApi Prelude.Text
deleteRestApi_restApiId = Lens.lens (\DeleteRestApi' {restApiId} -> restApiId) (\s@DeleteRestApi' {} a -> s {restApiId = a} :: DeleteRestApi)

instance Core.AWSRequest DeleteRestApi where
  type
    AWSResponse DeleteRestApi =
      DeleteRestApiResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull DeleteRestApiResponse'

instance Prelude.Hashable DeleteRestApi where
  hashWithSalt _salt DeleteRestApi' {..} =
    _salt `Prelude.hashWithSalt` restApiId

instance Prelude.NFData DeleteRestApi where
  rnf DeleteRestApi' {..} = Prelude.rnf restApiId

instance Data.ToHeaders DeleteRestApi where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Data.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Data.ToPath DeleteRestApi where
  toPath DeleteRestApi' {..} =
    Prelude.mconcat ["/restapis/", Data.toBS restApiId]

instance Data.ToQuery DeleteRestApi where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteRestApiResponse' smart constructor.
data DeleteRestApiResponse = DeleteRestApiResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRestApiResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteRestApiResponse ::
  DeleteRestApiResponse
newDeleteRestApiResponse = DeleteRestApiResponse'

instance Prelude.NFData DeleteRestApiResponse where
  rnf _ = ()
