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
-- Module      : Amazonka.SSM.DeleteAssociation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates the specified Amazon Web Services Systems Manager document
-- (SSM document) from the specified managed node. If you created the
-- association by using the @Targets@ parameter, then you must delete the
-- association by using the association ID.
--
-- When you disassociate a document from a managed node, it doesn\'t change
-- the configuration of the node. To change the configuration state of a
-- managed node after you disassociate a document, you must create a new
-- document with the desired configuration and associate it with the node.
module Amazonka.SSM.DeleteAssociation
  ( -- * Creating a Request
    DeleteAssociation (..),
    newDeleteAssociation,

    -- * Request Lenses
    deleteAssociation_name,
    deleteAssociation_instanceId,
    deleteAssociation_associationId,

    -- * Destructuring the Response
    DeleteAssociationResponse (..),
    newDeleteAssociationResponse,

    -- * Response Lenses
    deleteAssociationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newDeleteAssociation' smart constructor.
data DeleteAssociation = DeleteAssociation'
  { -- | The name of the SSM document.
    name :: Prelude.Maybe Prelude.Text,
    -- | The managed node ID.
    --
    -- @InstanceId@ has been deprecated. To specify a managed node ID for an
    -- association, use the @Targets@ parameter. Requests that include the
    -- parameter @InstanceID@ with Systems Manager documents (SSM documents)
    -- that use schema version 2.0 or later will fail. In addition, if you use
    -- the parameter @InstanceId@, you can\'t use the parameters
    -- @AssociationName@, @DocumentVersion@, @MaxErrors@, @MaxConcurrency@,
    -- @OutputLocation@, or @ScheduleExpression@. To use these parameters, you
    -- must use the @Targets@ parameter.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | The association ID that you want to delete.
    associationId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'deleteAssociation_name' - The name of the SSM document.
--
-- 'instanceId', 'deleteAssociation_instanceId' - The managed node ID.
--
-- @InstanceId@ has been deprecated. To specify a managed node ID for an
-- association, use the @Targets@ parameter. Requests that include the
-- parameter @InstanceID@ with Systems Manager documents (SSM documents)
-- that use schema version 2.0 or later will fail. In addition, if you use
-- the parameter @InstanceId@, you can\'t use the parameters
-- @AssociationName@, @DocumentVersion@, @MaxErrors@, @MaxConcurrency@,
-- @OutputLocation@, or @ScheduleExpression@. To use these parameters, you
-- must use the @Targets@ parameter.
--
-- 'associationId', 'deleteAssociation_associationId' - The association ID that you want to delete.
newDeleteAssociation ::
  DeleteAssociation
newDeleteAssociation =
  DeleteAssociation'
    { name = Prelude.Nothing,
      instanceId = Prelude.Nothing,
      associationId = Prelude.Nothing
    }

-- | The name of the SSM document.
deleteAssociation_name :: Lens.Lens' DeleteAssociation (Prelude.Maybe Prelude.Text)
deleteAssociation_name = Lens.lens (\DeleteAssociation' {name} -> name) (\s@DeleteAssociation' {} a -> s {name = a} :: DeleteAssociation)

-- | The managed node ID.
--
-- @InstanceId@ has been deprecated. To specify a managed node ID for an
-- association, use the @Targets@ parameter. Requests that include the
-- parameter @InstanceID@ with Systems Manager documents (SSM documents)
-- that use schema version 2.0 or later will fail. In addition, if you use
-- the parameter @InstanceId@, you can\'t use the parameters
-- @AssociationName@, @DocumentVersion@, @MaxErrors@, @MaxConcurrency@,
-- @OutputLocation@, or @ScheduleExpression@. To use these parameters, you
-- must use the @Targets@ parameter.
deleteAssociation_instanceId :: Lens.Lens' DeleteAssociation (Prelude.Maybe Prelude.Text)
deleteAssociation_instanceId = Lens.lens (\DeleteAssociation' {instanceId} -> instanceId) (\s@DeleteAssociation' {} a -> s {instanceId = a} :: DeleteAssociation)

-- | The association ID that you want to delete.
deleteAssociation_associationId :: Lens.Lens' DeleteAssociation (Prelude.Maybe Prelude.Text)
deleteAssociation_associationId = Lens.lens (\DeleteAssociation' {associationId} -> associationId) (\s@DeleteAssociation' {} a -> s {associationId = a} :: DeleteAssociation)

instance Core.AWSRequest DeleteAssociation where
  type
    AWSResponse DeleteAssociation =
      DeleteAssociationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteAssociationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteAssociation where
  hashWithSalt _salt DeleteAssociation' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` associationId

instance Prelude.NFData DeleteAssociation where
  rnf DeleteAssociation' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf associationId

instance Data.ToHeaders DeleteAssociation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonSSM.DeleteAssociation" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteAssociation where
  toJSON DeleteAssociation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Name" Data..=) Prelude.<$> name,
            ("InstanceId" Data..=) Prelude.<$> instanceId,
            ("AssociationId" Data..=) Prelude.<$> associationId
          ]
      )

instance Data.ToPath DeleteAssociation where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteAssociation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteAssociationResponse' smart constructor.
data DeleteAssociationResponse = DeleteAssociationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData DeleteAssociationResponse where
  rnf DeleteAssociationResponse' {..} =
    Prelude.rnf httpStatus
