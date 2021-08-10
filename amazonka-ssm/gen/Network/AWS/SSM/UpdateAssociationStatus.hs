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
-- Module      : Network.AWS.SSM.UpdateAssociationStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the status of the Systems Manager document associated with the
-- specified instance.
module Network.AWS.SSM.UpdateAssociationStatus
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newUpdateAssociationStatus' smart constructor.
data UpdateAssociationStatus = UpdateAssociationStatus'
  { -- | The name of the Systems Manager document.
    name :: Prelude.Text,
    -- | The ID of the instance.
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
-- 'name', 'updateAssociationStatus_name' - The name of the Systems Manager document.
--
-- 'instanceId', 'updateAssociationStatus_instanceId' - The ID of the instance.
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

-- | The name of the Systems Manager document.
updateAssociationStatus_name :: Lens.Lens' UpdateAssociationStatus Prelude.Text
updateAssociationStatus_name = Lens.lens (\UpdateAssociationStatus' {name} -> name) (\s@UpdateAssociationStatus' {} a -> s {name = a} :: UpdateAssociationStatus)

-- | The ID of the instance.
updateAssociationStatus_instanceId :: Lens.Lens' UpdateAssociationStatus Prelude.Text
updateAssociationStatus_instanceId = Lens.lens (\UpdateAssociationStatus' {instanceId} -> instanceId) (\s@UpdateAssociationStatus' {} a -> s {instanceId = a} :: UpdateAssociationStatus)

-- | The association status.
updateAssociationStatus_associationStatus :: Lens.Lens' UpdateAssociationStatus AssociationStatus
updateAssociationStatus_associationStatus = Lens.lens (\UpdateAssociationStatus' {associationStatus} -> associationStatus) (\s@UpdateAssociationStatus' {} a -> s {associationStatus = a} :: UpdateAssociationStatus)

instance Core.AWSRequest UpdateAssociationStatus where
  type
    AWSResponse UpdateAssociationStatus =
      UpdateAssociationStatusResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateAssociationStatusResponse'
            Prelude.<$> (x Core..?> "AssociationDescription")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateAssociationStatus

instance Prelude.NFData UpdateAssociationStatus

instance Core.ToHeaders UpdateAssociationStatus where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonSSM.UpdateAssociationStatus" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateAssociationStatus where
  toJSON UpdateAssociationStatus' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Core..= name),
            Prelude.Just ("InstanceId" Core..= instanceId),
            Prelude.Just
              ("AssociationStatus" Core..= associationStatus)
          ]
      )

instance Core.ToPath UpdateAssociationStatus where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateAssociationStatus where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateAssociationStatusResponse' smart constructor.
data UpdateAssociationStatusResponse = UpdateAssociationStatusResponse'
  { -- | Information about the association.
    associationDescription :: Prelude.Maybe AssociationDescription,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
