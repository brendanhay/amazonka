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
-- Module      : Network.AWS.SageMaker.DeleteFlowDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified flow definition.
module Network.AWS.SageMaker.DeleteFlowDefinition
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newDeleteFlowDefinition' smart constructor.
data DeleteFlowDefinition = DeleteFlowDefinition'
  { -- | The name of the flow definition you are deleting.
    flowDefinitionName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest DeleteFlowDefinition where
  type
    Rs DeleteFlowDefinition =
      DeleteFlowDefinitionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteFlowDefinitionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteFlowDefinition

instance Prelude.NFData DeleteFlowDefinition

instance Prelude.ToHeaders DeleteFlowDefinition where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "SageMaker.DeleteFlowDefinition" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteFlowDefinition where
  toJSON DeleteFlowDefinition' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "FlowDefinitionName"
                  Prelude..= flowDefinitionName
              )
          ]
      )

instance Prelude.ToPath DeleteFlowDefinition where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteFlowDefinition where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteFlowDefinitionResponse' smart constructor.
data DeleteFlowDefinitionResponse = DeleteFlowDefinitionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData DeleteFlowDefinitionResponse
