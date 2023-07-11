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
-- Module      : Amazonka.APIGateway.DeleteRequestValidator
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a RequestValidator of a given RestApi.
module Amazonka.APIGateway.DeleteRequestValidator
  ( -- * Creating a Request
    DeleteRequestValidator (..),
    newDeleteRequestValidator,

    -- * Request Lenses
    deleteRequestValidator_restApiId,
    deleteRequestValidator_requestValidatorId,

    -- * Destructuring the Response
    DeleteRequestValidatorResponse (..),
    newDeleteRequestValidatorResponse,
  )
where

import Amazonka.APIGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Deletes a specified RequestValidator of a given RestApi.
--
-- /See:/ 'newDeleteRequestValidator' smart constructor.
data DeleteRequestValidator = DeleteRequestValidator'
  { -- | The string identifier of the associated RestApi.
    restApiId :: Prelude.Text,
    -- | The identifier of the RequestValidator to be deleted.
    requestValidatorId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRequestValidator' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'restApiId', 'deleteRequestValidator_restApiId' - The string identifier of the associated RestApi.
--
-- 'requestValidatorId', 'deleteRequestValidator_requestValidatorId' - The identifier of the RequestValidator to be deleted.
newDeleteRequestValidator ::
  -- | 'restApiId'
  Prelude.Text ->
  -- | 'requestValidatorId'
  Prelude.Text ->
  DeleteRequestValidator
newDeleteRequestValidator
  pRestApiId_
  pRequestValidatorId_ =
    DeleteRequestValidator'
      { restApiId = pRestApiId_,
        requestValidatorId = pRequestValidatorId_
      }

-- | The string identifier of the associated RestApi.
deleteRequestValidator_restApiId :: Lens.Lens' DeleteRequestValidator Prelude.Text
deleteRequestValidator_restApiId = Lens.lens (\DeleteRequestValidator' {restApiId} -> restApiId) (\s@DeleteRequestValidator' {} a -> s {restApiId = a} :: DeleteRequestValidator)

-- | The identifier of the RequestValidator to be deleted.
deleteRequestValidator_requestValidatorId :: Lens.Lens' DeleteRequestValidator Prelude.Text
deleteRequestValidator_requestValidatorId = Lens.lens (\DeleteRequestValidator' {requestValidatorId} -> requestValidatorId) (\s@DeleteRequestValidator' {} a -> s {requestValidatorId = a} :: DeleteRequestValidator)

instance Core.AWSRequest DeleteRequestValidator where
  type
    AWSResponse DeleteRequestValidator =
      DeleteRequestValidatorResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull
      DeleteRequestValidatorResponse'

instance Prelude.Hashable DeleteRequestValidator where
  hashWithSalt _salt DeleteRequestValidator' {..} =
    _salt
      `Prelude.hashWithSalt` restApiId
      `Prelude.hashWithSalt` requestValidatorId

instance Prelude.NFData DeleteRequestValidator where
  rnf DeleteRequestValidator' {..} =
    Prelude.rnf restApiId
      `Prelude.seq` Prelude.rnf requestValidatorId

instance Data.ToHeaders DeleteRequestValidator where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Data.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Data.ToPath DeleteRequestValidator where
  toPath DeleteRequestValidator' {..} =
    Prelude.mconcat
      [ "/restapis/",
        Data.toBS restApiId,
        "/requestvalidators/",
        Data.toBS requestValidatorId
      ]

instance Data.ToQuery DeleteRequestValidator where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteRequestValidatorResponse' smart constructor.
data DeleteRequestValidatorResponse = DeleteRequestValidatorResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRequestValidatorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteRequestValidatorResponse ::
  DeleteRequestValidatorResponse
newDeleteRequestValidatorResponse =
  DeleteRequestValidatorResponse'

instance
  Prelude.NFData
    DeleteRequestValidatorResponse
  where
  rnf _ = ()
