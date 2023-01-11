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
-- Module      : Amazonka.SSM.UpdateAssociationStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the status of the Amazon Web Services Systems Manager document
-- (SSM document) associated with the specified managed node.
--
-- @UpdateAssociationStatus@ is primarily used by the Amazon Web Services
-- Systems Manager Agent (SSM Agent) to report status updates about your
-- associations and is only used for associations created with the
-- @InstanceId@ legacy parameter.
module Amazonka.SSM.UpdateAssociationStatus
  ( -- * Creating a Request
    UpdateAssociationStatus (..),
    newUpdateAssociationStatus,

    -- * Request Lenses
    updateAssociationStatus_name,
    updateAssociationStatus_instanceId,
    updateAssociationStatus_associationStatus,

    -- * Destructuring the Response
    UpdateAssociationStatusResponse (..),
    newUpdateAssociationStatusResponse,

    -- * Response Lenses
    updateAssociationStatusResponse_associationDescription,
    updateAssociationStatusResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newUpdateAssociationStatus' smart constructor.
data UpdateAssociationStatus = UpdateAssociationStatus'
  { -- | The name of the SSM document.
    name :: Prelude.Text,
    -- | The managed node ID.
    instanceId :: Prelude.Text,
    -- | The association status.
    associationStatus :: AssociationStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAssociationStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'updateAssociationStatus_name' - The name of the SSM document.
--
-- 'instanceId', 'updateAssociationStatus_instanceId' - The managed node ID.
--
-- 'associationStatus', 'updateAssociationStatus_associationStatus' - The association status.
newUpdateAssociationStatus ::
  -- | 'name'
  Prelude.Text ->
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'associationStatus'
  AssociationStatus ->
  UpdateAssociationStatus
newUpdateAssociationStatus
  pName_
  pInstanceId_
  pAssociationStatus_ =
    UpdateAssociationStatus'
      { name = pName_,
        instanceId = pInstanceId_,
        associationStatus = pAssociationStatus_
      }

-- | The name of the SSM document.
updateAssociationStatus_name :: Lens.Lens' UpdateAssociationStatus Prelude.Text
updateAssociationStatus_name = Lens.lens (\UpdateAssociationStatus' {name} -> name) (\s@UpdateAssociationStatus' {} a -> s {name = a} :: UpdateAssociationStatus)

-- | The managed node ID.
updateAssociationStatus_instanceId :: Lens.Lens' UpdateAssociationStatus Prelude.Text
updateAssociationStatus_instanceId = Lens.lens (\UpdateAssociationStatus' {instanceId} -> instanceId) (\s@UpdateAssociationStatus' {} a -> s {instanceId = a} :: UpdateAssociationStatus)

-- | The association status.
updateAssociationStatus_associationStatus :: Lens.Lens' UpdateAssociationStatus AssociationStatus
updateAssociationStatus_associationStatus = Lens.lens (\UpdateAssociationStatus' {associationStatus} -> associationStatus) (\s@UpdateAssociationStatus' {} a -> s {associationStatus = a} :: UpdateAssociationStatus)

instance Core.AWSRequest UpdateAssociationStatus where
  type
    AWSResponse UpdateAssociationStatus =
      UpdateAssociationStatusResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateAssociationStatusResponse'
            Prelude.<$> (x Data..?> "AssociationDescription")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateAssociationStatus where
  hashWithSalt _salt UpdateAssociationStatus' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` associationStatus

instance Prelude.NFData UpdateAssociationStatus where
  rnf UpdateAssociationStatus' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf associationStatus

instance Data.ToHeaders UpdateAssociationStatus where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonSSM.UpdateAssociationStatus" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateAssociationStatus where
  toJSON UpdateAssociationStatus' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Data..= name),
            Prelude.Just ("InstanceId" Data..= instanceId),
            Prelude.Just
              ("AssociationStatus" Data..= associationStatus)
          ]
      )

instance Data.ToPath UpdateAssociationStatus where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateAssociationStatus where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateAssociationStatusResponse' smart constructor.
data UpdateAssociationStatusResponse = UpdateAssociationStatusResponse'
  { -- | Information about the association.
    associationDescription :: Prelude.Maybe AssociationDescription,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAssociationStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associationDescription', 'updateAssociationStatusResponse_associationDescription' - Information about the association.
--
-- 'httpStatus', 'updateAssociationStatusResponse_httpStatus' - The response's http status code.
newUpdateAssociationStatusResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateAssociationStatusResponse
newUpdateAssociationStatusResponse pHttpStatus_ =
  UpdateAssociationStatusResponse'
    { associationDescription =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the association.
updateAssociationStatusResponse_associationDescription :: Lens.Lens' UpdateAssociationStatusResponse (Prelude.Maybe AssociationDescription)
updateAssociationStatusResponse_associationDescription = Lens.lens (\UpdateAssociationStatusResponse' {associationDescription} -> associationDescription) (\s@UpdateAssociationStatusResponse' {} a -> s {associationDescription = a} :: UpdateAssociationStatusResponse)

-- | The response's http status code.
updateAssociationStatusResponse_httpStatus :: Lens.Lens' UpdateAssociationStatusResponse Prelude.Int
updateAssociationStatusResponse_httpStatus = Lens.lens (\UpdateAssociationStatusResponse' {httpStatus} -> httpStatus) (\s@UpdateAssociationStatusResponse' {} a -> s {httpStatus = a} :: UpdateAssociationStatusResponse)

instance
  Prelude.NFData
    UpdateAssociationStatusResponse
  where
  rnf UpdateAssociationStatusResponse' {..} =
    Prelude.rnf associationDescription
      `Prelude.seq` Prelude.rnf httpStatus
