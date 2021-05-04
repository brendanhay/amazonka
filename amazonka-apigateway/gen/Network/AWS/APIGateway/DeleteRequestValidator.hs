{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.APIGateway.DeleteRequestValidator
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a RequestValidator of a given RestApi.
module Network.AWS.APIGateway.DeleteRequestValidator
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

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Deletes a specified RequestValidator of a given RestApi.
--
-- /See:/ 'newDeleteRequestValidator' smart constructor.
data DeleteRequestValidator = DeleteRequestValidator'
  { -- | [Required] The string identifier of the associated RestApi.
    restApiId :: Prelude.Text,
    -- | [Required] The identifier of the RequestValidator to be deleted.
    requestValidatorId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteRequestValidator' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'restApiId', 'deleteRequestValidator_restApiId' - [Required] The string identifier of the associated RestApi.
--
-- 'requestValidatorId', 'deleteRequestValidator_requestValidatorId' - [Required] The identifier of the RequestValidator to be deleted.
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

-- | [Required] The string identifier of the associated RestApi.
deleteRequestValidator_restApiId :: Lens.Lens' DeleteRequestValidator Prelude.Text
deleteRequestValidator_restApiId = Lens.lens (\DeleteRequestValidator' {restApiId} -> restApiId) (\s@DeleteRequestValidator' {} a -> s {restApiId = a} :: DeleteRequestValidator)

-- | [Required] The identifier of the RequestValidator to be deleted.
deleteRequestValidator_requestValidatorId :: Lens.Lens' DeleteRequestValidator Prelude.Text
deleteRequestValidator_requestValidatorId = Lens.lens (\DeleteRequestValidator' {requestValidatorId} -> requestValidatorId) (\s@DeleteRequestValidator' {} a -> s {requestValidatorId = a} :: DeleteRequestValidator)

instance Prelude.AWSRequest DeleteRequestValidator where
  type
    Rs DeleteRequestValidator =
      DeleteRequestValidatorResponse
  request = Request.delete defaultService
  response =
    Response.receiveNull
      DeleteRequestValidatorResponse'

instance Prelude.Hashable DeleteRequestValidator

instance Prelude.NFData DeleteRequestValidator

instance Prelude.ToHeaders DeleteRequestValidator where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Prelude.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Prelude.ToPath DeleteRequestValidator where
  toPath DeleteRequestValidator' {..} =
    Prelude.mconcat
      [ "/restapis/",
        Prelude.toBS restApiId,
        "/requestvalidators/",
        Prelude.toBS requestValidatorId
      ]

instance Prelude.ToQuery DeleteRequestValidator where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteRequestValidatorResponse' smart constructor.
data DeleteRequestValidatorResponse = DeleteRequestValidatorResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
