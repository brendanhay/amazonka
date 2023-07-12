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
-- Module      : Amazonka.Connect.DeleteIntegrationAssociation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an Amazon Web Services resource association from an Amazon
-- Connect instance. The association must not have any use cases associated
-- with it.
module Amazonka.Connect.DeleteIntegrationAssociation
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

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteIntegrationAssociation' smart constructor.
data DeleteIntegrationAssociation = DeleteIntegrationAssociation'
  { -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    instanceId :: Prelude.Text,
    -- | The identifier for the integration association.
    integrationAssociationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteIntegrationAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'deleteIntegrationAssociation_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
--
-- 'integrationAssociationId', 'deleteIntegrationAssociation_integrationAssociationId' - The identifier for the integration association.
newDeleteIntegrationAssociation ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'integrationAssociationId'
  Prelude.Text ->
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

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
deleteIntegrationAssociation_instanceId :: Lens.Lens' DeleteIntegrationAssociation Prelude.Text
deleteIntegrationAssociation_instanceId = Lens.lens (\DeleteIntegrationAssociation' {instanceId} -> instanceId) (\s@DeleteIntegrationAssociation' {} a -> s {instanceId = a} :: DeleteIntegrationAssociation)

-- | The identifier for the integration association.
deleteIntegrationAssociation_integrationAssociationId :: Lens.Lens' DeleteIntegrationAssociation Prelude.Text
deleteIntegrationAssociation_integrationAssociationId = Lens.lens (\DeleteIntegrationAssociation' {integrationAssociationId} -> integrationAssociationId) (\s@DeleteIntegrationAssociation' {} a -> s {integrationAssociationId = a} :: DeleteIntegrationAssociation)

instance Core.AWSRequest DeleteIntegrationAssociation where
  type
    AWSResponse DeleteIntegrationAssociation =
      DeleteIntegrationAssociationResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull
      DeleteIntegrationAssociationResponse'

instance
  Prelude.Hashable
    DeleteIntegrationAssociation
  where
  hashWithSalt _salt DeleteIntegrationAssociation' {..} =
    _salt
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` integrationAssociationId

instance Prelude.NFData DeleteIntegrationAssociation where
  rnf DeleteIntegrationAssociation' {..} =
    Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf integrationAssociationId

instance Data.ToHeaders DeleteIntegrationAssociation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteIntegrationAssociation where
  toPath DeleteIntegrationAssociation' {..} =
    Prelude.mconcat
      [ "/instance/",
        Data.toBS instanceId,
        "/integration-associations/",
        Data.toBS integrationAssociationId
      ]

instance Data.ToQuery DeleteIntegrationAssociation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteIntegrationAssociationResponse' smart constructor.
data DeleteIntegrationAssociationResponse = DeleteIntegrationAssociationResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteIntegrationAssociationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteIntegrationAssociationResponse ::
  DeleteIntegrationAssociationResponse
newDeleteIntegrationAssociationResponse =
  DeleteIntegrationAssociationResponse'

instance
  Prelude.NFData
    DeleteIntegrationAssociationResponse
  where
  rnf _ = ()
