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
-- Module      : Network.AWS.SSM.DeleteAssociation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates the specified Systems Manager document from the specified
-- instance.
--
-- When you disassociate a document from an instance, it does not change
-- the configuration of the instance. To change the configuration state of
-- an instance after you disassociate a document, you must create a new
-- document with the desired configuration and associate it with the
-- instance.
module Network.AWS.SSM.DeleteAssociation
  ( -- * Creating a Request
    DeleteAssociation (..),
    newDeleteAssociation,

    -- * Request Lenses
    deleteAssociation_instanceId,
    deleteAssociation_name,
    deleteAssociation_associationId,

    -- * Destructuring the Response
    DeleteAssociationResponse (..),
    newDeleteAssociationResponse,

    -- * Response Lenses
    deleteAssociationResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newDeleteAssociation' smart constructor.
data DeleteAssociation = DeleteAssociation'
  { -- | The ID of the instance.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | The name of the Systems Manager document.
    name :: Prelude.Maybe Prelude.Text,
    -- | The association ID that you want to delete.
    associationId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'deleteAssociation_instanceId' - The ID of the instance.
--
-- 'name', 'deleteAssociation_name' - The name of the Systems Manager document.
--
-- 'associationId', 'deleteAssociation_associationId' - The association ID that you want to delete.
newDeleteAssociation ::
  DeleteAssociation
newDeleteAssociation =
  DeleteAssociation'
    { instanceId = Prelude.Nothing,
      name = Prelude.Nothing,
      associationId = Prelude.Nothing
    }

-- | The ID of the instance.
deleteAssociation_instanceId :: Lens.Lens' DeleteAssociation (Prelude.Maybe Prelude.Text)
deleteAssociation_instanceId = Lens.lens (\DeleteAssociation' {instanceId} -> instanceId) (\s@DeleteAssociation' {} a -> s {instanceId = a} :: DeleteAssociation)

-- | The name of the Systems Manager document.
deleteAssociation_name :: Lens.Lens' DeleteAssociation (Prelude.Maybe Prelude.Text)
deleteAssociation_name = Lens.lens (\DeleteAssociation' {name} -> name) (\s@DeleteAssociation' {} a -> s {name = a} :: DeleteAssociation)

-- | The association ID that you want to delete.
deleteAssociation_associationId :: Lens.Lens' DeleteAssociation (Prelude.Maybe Prelude.Text)
deleteAssociation_associationId = Lens.lens (\DeleteAssociation' {associationId} -> associationId) (\s@DeleteAssociation' {} a -> s {associationId = a} :: DeleteAssociation)

instance Prelude.AWSRequest DeleteAssociation where
  type Rs DeleteAssociation = DeleteAssociationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteAssociationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteAssociation

instance Prelude.NFData DeleteAssociation

instance Prelude.ToHeaders DeleteAssociation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AmazonSSM.DeleteAssociation" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteAssociation where
  toJSON DeleteAssociation' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("InstanceId" Prelude..=) Prelude.<$> instanceId,
            ("Name" Prelude..=) Prelude.<$> name,
            ("AssociationId" Prelude..=)
              Prelude.<$> associationId
          ]
      )

instance Prelude.ToPath DeleteAssociation where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteAssociation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteAssociationResponse' smart constructor.
data DeleteAssociationResponse = DeleteAssociationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteAssociationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteAssociationResponse_httpStatus' - The response's http status code.
newDeleteAssociationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteAssociationResponse
newDeleteAssociationResponse pHttpStatus_ =
  DeleteAssociationResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteAssociationResponse_httpStatus :: Lens.Lens' DeleteAssociationResponse Prelude.Int
deleteAssociationResponse_httpStatus = Lens.lens (\DeleteAssociationResponse' {httpStatus} -> httpStatus) (\s@DeleteAssociationResponse' {} a -> s {httpStatus = a} :: DeleteAssociationResponse)

instance Prelude.NFData DeleteAssociationResponse
