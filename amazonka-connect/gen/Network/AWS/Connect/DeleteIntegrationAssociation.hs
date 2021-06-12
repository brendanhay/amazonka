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
-- Module      : Network.AWS.Connect.DeleteIntegrationAssociation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This API is in preview release for Amazon Connect and is subject to
-- change.
--
-- Deletes an AppIntegration association from an Amazon Connect instance.
-- The association must not have any use cases associated with it.
module Network.AWS.Connect.DeleteIntegrationAssociation
  ( -- * Creating a Request
    DeleteIntegrationAssociation (..),
    newDeleteIntegrationAssociation,

    -- * Request Lenses
    deleteIntegrationAssociation_instanceId,
    deleteIntegrationAssociation_integrationAssociationId,

    -- * Destructuring the Response
    DeleteIntegrationAssociationResponse (..),
    newDeleteIntegrationAssociationResponse,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteIntegrationAssociation' smart constructor.
data DeleteIntegrationAssociation = DeleteIntegrationAssociation'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Core.Text,
    -- | The identifier for the AppIntegration association.
    integrationAssociationId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteIntegrationAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'deleteIntegrationAssociation_instanceId' - The identifier of the Amazon Connect instance.
--
-- 'integrationAssociationId', 'deleteIntegrationAssociation_integrationAssociationId' - The identifier for the AppIntegration association.
newDeleteIntegrationAssociation ::
  -- | 'instanceId'
  Core.Text ->
  -- | 'integrationAssociationId'
  Core.Text ->
  DeleteIntegrationAssociation
newDeleteIntegrationAssociation
  pInstanceId_
  pIntegrationAssociationId_ =
    DeleteIntegrationAssociation'
      { instanceId =
          pInstanceId_,
        integrationAssociationId =
          pIntegrationAssociationId_
      }

-- | The identifier of the Amazon Connect instance.
deleteIntegrationAssociation_instanceId :: Lens.Lens' DeleteIntegrationAssociation Core.Text
deleteIntegrationAssociation_instanceId = Lens.lens (\DeleteIntegrationAssociation' {instanceId} -> instanceId) (\s@DeleteIntegrationAssociation' {} a -> s {instanceId = a} :: DeleteIntegrationAssociation)

-- | The identifier for the AppIntegration association.
deleteIntegrationAssociation_integrationAssociationId :: Lens.Lens' DeleteIntegrationAssociation Core.Text
deleteIntegrationAssociation_integrationAssociationId = Lens.lens (\DeleteIntegrationAssociation' {integrationAssociationId} -> integrationAssociationId) (\s@DeleteIntegrationAssociation' {} a -> s {integrationAssociationId = a} :: DeleteIntegrationAssociation)

instance Core.AWSRequest DeleteIntegrationAssociation where
  type
    AWSResponse DeleteIntegrationAssociation =
      DeleteIntegrationAssociationResponse
  request = Request.delete defaultService
  response =
    Response.receiveNull
      DeleteIntegrationAssociationResponse'

instance Core.Hashable DeleteIntegrationAssociation

instance Core.NFData DeleteIntegrationAssociation

instance Core.ToHeaders DeleteIntegrationAssociation where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath DeleteIntegrationAssociation where
  toPath DeleteIntegrationAssociation' {..} =
    Core.mconcat
      [ "/instance/",
        Core.toBS instanceId,
        "/integration-associations/",
        Core.toBS integrationAssociationId
      ]

instance Core.ToQuery DeleteIntegrationAssociation where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteIntegrationAssociationResponse' smart constructor.
data DeleteIntegrationAssociationResponse = DeleteIntegrationAssociationResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteIntegrationAssociationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteIntegrationAssociationResponse ::
  DeleteIntegrationAssociationResponse
newDeleteIntegrationAssociationResponse =
  DeleteIntegrationAssociationResponse'

instance
  Core.NFData
    DeleteIntegrationAssociationResponse
