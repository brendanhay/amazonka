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
-- Module      : Network.AWS.Connect.DeleteUseCase
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This API is in preview release for Amazon Connect and is subject to
-- change.
--
-- Deletes a use case from an AppIntegration association.
module Network.AWS.Connect.DeleteUseCase
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

import Network.AWS.Connect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteUseCase' smart constructor.
data DeleteUseCase = DeleteUseCase'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Prelude.Text,
    -- | The identifier for the AppIntegration association.
    integrationAssociationId :: Prelude.Text,
    -- | The identifier for the use case.
    useCaseId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteUseCase' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'deleteUseCase_instanceId' - The identifier of the Amazon Connect instance.
--
-- 'integrationAssociationId', 'deleteUseCase_integrationAssociationId' - The identifier for the AppIntegration association.
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

-- | The identifier of the Amazon Connect instance.
deleteUseCase_instanceId :: Lens.Lens' DeleteUseCase Prelude.Text
deleteUseCase_instanceId = Lens.lens (\DeleteUseCase' {instanceId} -> instanceId) (\s@DeleteUseCase' {} a -> s {instanceId = a} :: DeleteUseCase)

-- | The identifier for the AppIntegration association.
deleteUseCase_integrationAssociationId :: Lens.Lens' DeleteUseCase Prelude.Text
deleteUseCase_integrationAssociationId = Lens.lens (\DeleteUseCase' {integrationAssociationId} -> integrationAssociationId) (\s@DeleteUseCase' {} a -> s {integrationAssociationId = a} :: DeleteUseCase)

-- | The identifier for the use case.
deleteUseCase_useCaseId :: Lens.Lens' DeleteUseCase Prelude.Text
deleteUseCase_useCaseId = Lens.lens (\DeleteUseCase' {useCaseId} -> useCaseId) (\s@DeleteUseCase' {} a -> s {useCaseId = a} :: DeleteUseCase)

instance Prelude.AWSRequest DeleteUseCase where
  type Rs DeleteUseCase = DeleteUseCaseResponse
  request = Request.delete defaultService
  response =
    Response.receiveNull DeleteUseCaseResponse'

instance Prelude.Hashable DeleteUseCase

instance Prelude.NFData DeleteUseCase

instance Prelude.ToHeaders DeleteUseCase where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToPath DeleteUseCase where
  toPath DeleteUseCase' {..} =
    Prelude.mconcat
      [ "/instance/",
        Prelude.toBS instanceId,
        "/integration-associations/",
        Prelude.toBS integrationAssociationId,
        "/use-cases/",
        Prelude.toBS useCaseId
      ]

instance Prelude.ToQuery DeleteUseCase where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteUseCaseResponse' smart constructor.
data DeleteUseCaseResponse = DeleteUseCaseResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteUseCaseResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteUseCaseResponse ::
  DeleteUseCaseResponse
newDeleteUseCaseResponse = DeleteUseCaseResponse'

instance Prelude.NFData DeleteUseCaseResponse
