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
-- Module      : Amazonka.Connect.DeleteUseCase
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a use case from an integration association.
module Amazonka.Connect.DeleteUseCase
  ( -- * Creating a Request
    DeleteUseCase (..),
    newDeleteUseCase,

    -- * Request Lenses
    deleteUseCase_instanceId,
    deleteUseCase_integrationAssociationId,
    deleteUseCase_useCaseId,

    -- * Destructuring the Response
    DeleteUseCaseResponse (..),
    newDeleteUseCaseResponse,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteUseCase' smart constructor.
data DeleteUseCase = DeleteUseCase'
  { -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    instanceId :: Prelude.Text,
    -- | The identifier for the integration association.
    integrationAssociationId :: Prelude.Text,
    -- | The identifier for the use case.
    useCaseId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteUseCase' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'deleteUseCase_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
--
-- 'integrationAssociationId', 'deleteUseCase_integrationAssociationId' - The identifier for the integration association.
--
-- 'useCaseId', 'deleteUseCase_useCaseId' - The identifier for the use case.
newDeleteUseCase ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'integrationAssociationId'
  Prelude.Text ->
  -- | 'useCaseId'
  Prelude.Text ->
  DeleteUseCase
newDeleteUseCase
  pInstanceId_
  pIntegrationAssociationId_
  pUseCaseId_ =
    DeleteUseCase'
      { instanceId = pInstanceId_,
        integrationAssociationId =
          pIntegrationAssociationId_,
        useCaseId = pUseCaseId_
      }

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
deleteUseCase_instanceId :: Lens.Lens' DeleteUseCase Prelude.Text
deleteUseCase_instanceId = Lens.lens (\DeleteUseCase' {instanceId} -> instanceId) (\s@DeleteUseCase' {} a -> s {instanceId = a} :: DeleteUseCase)

-- | The identifier for the integration association.
deleteUseCase_integrationAssociationId :: Lens.Lens' DeleteUseCase Prelude.Text
deleteUseCase_integrationAssociationId = Lens.lens (\DeleteUseCase' {integrationAssociationId} -> integrationAssociationId) (\s@DeleteUseCase' {} a -> s {integrationAssociationId = a} :: DeleteUseCase)

-- | The identifier for the use case.
deleteUseCase_useCaseId :: Lens.Lens' DeleteUseCase Prelude.Text
deleteUseCase_useCaseId = Lens.lens (\DeleteUseCase' {useCaseId} -> useCaseId) (\s@DeleteUseCase' {} a -> s {useCaseId = a} :: DeleteUseCase)

instance Core.AWSRequest DeleteUseCase where
  type
    AWSResponse DeleteUseCase =
      DeleteUseCaseResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull DeleteUseCaseResponse'

instance Prelude.Hashable DeleteUseCase where
  hashWithSalt _salt DeleteUseCase' {..} =
    _salt `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` integrationAssociationId
      `Prelude.hashWithSalt` useCaseId

instance Prelude.NFData DeleteUseCase where
  rnf DeleteUseCase' {..} =
    Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf integrationAssociationId
      `Prelude.seq` Prelude.rnf useCaseId

instance Data.ToHeaders DeleteUseCase where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteUseCase where
  toPath DeleteUseCase' {..} =
    Prelude.mconcat
      [ "/instance/",
        Data.toBS instanceId,
        "/integration-associations/",
        Data.toBS integrationAssociationId,
        "/use-cases/",
        Data.toBS useCaseId
      ]

instance Data.ToQuery DeleteUseCase where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteUseCaseResponse' smart constructor.
data DeleteUseCaseResponse = DeleteUseCaseResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteUseCaseResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteUseCaseResponse ::
  DeleteUseCaseResponse
newDeleteUseCaseResponse = DeleteUseCaseResponse'

instance Prelude.NFData DeleteUseCaseResponse where
  rnf _ = ()
