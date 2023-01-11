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
-- Module      : Amazonka.SageMaker.DeleteFlowDefinition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified flow definition.
module Amazonka.SageMaker.DeleteFlowDefinition
  ( -- * Creating a Request
    DeleteFlowDefinition (..),
    newDeleteFlowDefinition,

    -- * Request Lenses
    deleteFlowDefinition_flowDefinitionName,

    -- * Destructuring the Response
    DeleteFlowDefinitionResponse (..),
    newDeleteFlowDefinitionResponse,

    -- * Response Lenses
    deleteFlowDefinitionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newDeleteFlowDefinition' smart constructor.
data DeleteFlowDefinition = DeleteFlowDefinition'
  { -- | The name of the flow definition you are deleting.
    flowDefinitionName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteFlowDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'flowDefinitionName', 'deleteFlowDefinition_flowDefinitionName' - The name of the flow definition you are deleting.
newDeleteFlowDefinition ::
  -- | 'flowDefinitionName'
  Prelude.Text ->
  DeleteFlowDefinition
newDeleteFlowDefinition pFlowDefinitionName_ =
  DeleteFlowDefinition'
    { flowDefinitionName =
        pFlowDefinitionName_
    }

-- | The name of the flow definition you are deleting.
deleteFlowDefinition_flowDefinitionName :: Lens.Lens' DeleteFlowDefinition Prelude.Text
deleteFlowDefinition_flowDefinitionName = Lens.lens (\DeleteFlowDefinition' {flowDefinitionName} -> flowDefinitionName) (\s@DeleteFlowDefinition' {} a -> s {flowDefinitionName = a} :: DeleteFlowDefinition)

instance Core.AWSRequest DeleteFlowDefinition where
  type
    AWSResponse DeleteFlowDefinition =
      DeleteFlowDefinitionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteFlowDefinitionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteFlowDefinition where
  hashWithSalt _salt DeleteFlowDefinition' {..} =
    _salt `Prelude.hashWithSalt` flowDefinitionName

instance Prelude.NFData DeleteFlowDefinition where
  rnf DeleteFlowDefinition' {..} =
    Prelude.rnf flowDefinitionName

instance Data.ToHeaders DeleteFlowDefinition where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.DeleteFlowDefinition" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteFlowDefinition where
  toJSON DeleteFlowDefinition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("FlowDefinitionName" Data..= flowDefinitionName)
          ]
      )

instance Data.ToPath DeleteFlowDefinition where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteFlowDefinition where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteFlowDefinitionResponse' smart constructor.
data DeleteFlowDefinitionResponse = DeleteFlowDefinitionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteFlowDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteFlowDefinitionResponse_httpStatus' - The response's http status code.
newDeleteFlowDefinitionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteFlowDefinitionResponse
newDeleteFlowDefinitionResponse pHttpStatus_ =
  DeleteFlowDefinitionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteFlowDefinitionResponse_httpStatus :: Lens.Lens' DeleteFlowDefinitionResponse Prelude.Int
deleteFlowDefinitionResponse_httpStatus = Lens.lens (\DeleteFlowDefinitionResponse' {httpStatus} -> httpStatus) (\s@DeleteFlowDefinitionResponse' {} a -> s {httpStatus = a} :: DeleteFlowDefinitionResponse)

instance Prelude.NFData DeleteFlowDefinitionResponse where
  rnf DeleteFlowDefinitionResponse' {..} =
    Prelude.rnf httpStatus
