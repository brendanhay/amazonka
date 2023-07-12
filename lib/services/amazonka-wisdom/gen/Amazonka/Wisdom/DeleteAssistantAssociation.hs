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
-- Module      : Amazonka.Wisdom.DeleteAssistantAssociation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an assistant association.
module Amazonka.Wisdom.DeleteAssistantAssociation
  ( -- * Creating a Request
    DeleteAssistantAssociation (..),
    newDeleteAssistantAssociation,

    -- * Request Lenses
    deleteAssistantAssociation_assistantAssociationId,
    deleteAssistantAssociation_assistantId,

    -- * Destructuring the Response
    DeleteAssistantAssociationResponse (..),
    newDeleteAssistantAssociationResponse,

    -- * Response Lenses
    deleteAssistantAssociationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Wisdom.Types

-- | /See:/ 'newDeleteAssistantAssociation' smart constructor.
data DeleteAssistantAssociation = DeleteAssistantAssociation'
  { -- | The identifier of the assistant association. Can be either the ID or the
    -- ARN. URLs cannot contain the ARN.
    assistantAssociationId :: Prelude.Text,
    -- | The identifier of the Wisdom assistant. Can be either the ID or the ARN.
    -- URLs cannot contain the ARN.
    assistantId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAssistantAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assistantAssociationId', 'deleteAssistantAssociation_assistantAssociationId' - The identifier of the assistant association. Can be either the ID or the
-- ARN. URLs cannot contain the ARN.
--
-- 'assistantId', 'deleteAssistantAssociation_assistantId' - The identifier of the Wisdom assistant. Can be either the ID or the ARN.
-- URLs cannot contain the ARN.
newDeleteAssistantAssociation ::
  -- | 'assistantAssociationId'
  Prelude.Text ->
  -- | 'assistantId'
  Prelude.Text ->
  DeleteAssistantAssociation
newDeleteAssistantAssociation
  pAssistantAssociationId_
  pAssistantId_ =
    DeleteAssistantAssociation'
      { assistantAssociationId =
          pAssistantAssociationId_,
        assistantId = pAssistantId_
      }

-- | The identifier of the assistant association. Can be either the ID or the
-- ARN. URLs cannot contain the ARN.
deleteAssistantAssociation_assistantAssociationId :: Lens.Lens' DeleteAssistantAssociation Prelude.Text
deleteAssistantAssociation_assistantAssociationId = Lens.lens (\DeleteAssistantAssociation' {assistantAssociationId} -> assistantAssociationId) (\s@DeleteAssistantAssociation' {} a -> s {assistantAssociationId = a} :: DeleteAssistantAssociation)

-- | The identifier of the Wisdom assistant. Can be either the ID or the ARN.
-- URLs cannot contain the ARN.
deleteAssistantAssociation_assistantId :: Lens.Lens' DeleteAssistantAssociation Prelude.Text
deleteAssistantAssociation_assistantId = Lens.lens (\DeleteAssistantAssociation' {assistantId} -> assistantId) (\s@DeleteAssistantAssociation' {} a -> s {assistantId = a} :: DeleteAssistantAssociation)

instance Core.AWSRequest DeleteAssistantAssociation where
  type
    AWSResponse DeleteAssistantAssociation =
      DeleteAssistantAssociationResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteAssistantAssociationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteAssistantAssociation where
  hashWithSalt _salt DeleteAssistantAssociation' {..} =
    _salt
      `Prelude.hashWithSalt` assistantAssociationId
      `Prelude.hashWithSalt` assistantId

instance Prelude.NFData DeleteAssistantAssociation where
  rnf DeleteAssistantAssociation' {..} =
    Prelude.rnf assistantAssociationId
      `Prelude.seq` Prelude.rnf assistantId

instance Data.ToHeaders DeleteAssistantAssociation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteAssistantAssociation where
  toPath DeleteAssistantAssociation' {..} =
    Prelude.mconcat
      [ "/assistants/",
        Data.toBS assistantId,
        "/associations/",
        Data.toBS assistantAssociationId
      ]

instance Data.ToQuery DeleteAssistantAssociation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteAssistantAssociationResponse' smart constructor.
data DeleteAssistantAssociationResponse = DeleteAssistantAssociationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAssistantAssociationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteAssistantAssociationResponse_httpStatus' - The response's http status code.
newDeleteAssistantAssociationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteAssistantAssociationResponse
newDeleteAssistantAssociationResponse pHttpStatus_ =
  DeleteAssistantAssociationResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteAssistantAssociationResponse_httpStatus :: Lens.Lens' DeleteAssistantAssociationResponse Prelude.Int
deleteAssistantAssociationResponse_httpStatus = Lens.lens (\DeleteAssistantAssociationResponse' {httpStatus} -> httpStatus) (\s@DeleteAssistantAssociationResponse' {} a -> s {httpStatus = a} :: DeleteAssistantAssociationResponse)

instance
  Prelude.NFData
    DeleteAssistantAssociationResponse
  where
  rnf DeleteAssistantAssociationResponse' {..} =
    Prelude.rnf httpStatus
