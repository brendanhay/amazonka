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
-- Module      : Amazonka.ApiGatewayV2.DeleteCorsConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a CORS configuration.
module Amazonka.ApiGatewayV2.DeleteCorsConfiguration
  ( -- * Creating a Request
    DeleteCorsConfiguration (..),
    newDeleteCorsConfiguration,

    -- * Request Lenses
    deleteCorsConfiguration_apiId,

    -- * Destructuring the Response
    DeleteCorsConfigurationResponse (..),
    newDeleteCorsConfigurationResponse,
  )
where

import Amazonka.ApiGatewayV2.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteCorsConfiguration' smart constructor.
data DeleteCorsConfiguration = DeleteCorsConfiguration'
  { -- | The API identifier.
    apiId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteCorsConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiId', 'deleteCorsConfiguration_apiId' - The API identifier.
newDeleteCorsConfiguration ::
  -- | 'apiId'
  Prelude.Text ->
  DeleteCorsConfiguration
newDeleteCorsConfiguration pApiId_ =
  DeleteCorsConfiguration' {apiId = pApiId_}

-- | The API identifier.
deleteCorsConfiguration_apiId :: Lens.Lens' DeleteCorsConfiguration Prelude.Text
deleteCorsConfiguration_apiId = Lens.lens (\DeleteCorsConfiguration' {apiId} -> apiId) (\s@DeleteCorsConfiguration' {} a -> s {apiId = a} :: DeleteCorsConfiguration)

instance Core.AWSRequest DeleteCorsConfiguration where
  type
    AWSResponse DeleteCorsConfiguration =
      DeleteCorsConfigurationResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull
      DeleteCorsConfigurationResponse'

instance Prelude.Hashable DeleteCorsConfiguration where
  hashWithSalt _salt DeleteCorsConfiguration' {..} =
    _salt `Prelude.hashWithSalt` apiId

instance Prelude.NFData DeleteCorsConfiguration where
  rnf DeleteCorsConfiguration' {..} = Prelude.rnf apiId

instance Data.ToHeaders DeleteCorsConfiguration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteCorsConfiguration where
  toPath DeleteCorsConfiguration' {..} =
    Prelude.mconcat
      ["/v2/apis/", Data.toBS apiId, "/cors"]

instance Data.ToQuery DeleteCorsConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteCorsConfigurationResponse' smart constructor.
data DeleteCorsConfigurationResponse = DeleteCorsConfigurationResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteCorsConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteCorsConfigurationResponse ::
  DeleteCorsConfigurationResponse
newDeleteCorsConfigurationResponse =
  DeleteCorsConfigurationResponse'

instance
  Prelude.NFData
    DeleteCorsConfigurationResponse
  where
  rnf _ = ()
