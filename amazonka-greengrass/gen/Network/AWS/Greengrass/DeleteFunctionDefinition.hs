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
-- Module      : Network.AWS.Greengrass.DeleteFunctionDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a Lambda function definition.
module Network.AWS.Greengrass.DeleteFunctionDefinition
  ( -- * Creating a Request
    DeleteFunctionDefinition (..),
    newDeleteFunctionDefinition,

    -- * Request Lenses
    deleteFunctionDefinition_functionDefinitionId,

    -- * Destructuring the Response
    DeleteFunctionDefinitionResponse (..),
    newDeleteFunctionDefinitionResponse,

    -- * Response Lenses
    deleteFunctionDefinitionResponse_httpStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteFunctionDefinition' smart constructor.
data DeleteFunctionDefinition = DeleteFunctionDefinition'
  { -- | The ID of the Lambda function definition.
    functionDefinitionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteFunctionDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'functionDefinitionId', 'deleteFunctionDefinition_functionDefinitionId' - The ID of the Lambda function definition.
newDeleteFunctionDefinition ::
  -- | 'functionDefinitionId'
  Prelude.Text ->
  DeleteFunctionDefinition
newDeleteFunctionDefinition pFunctionDefinitionId_ =
  DeleteFunctionDefinition'
    { functionDefinitionId =
        pFunctionDefinitionId_
    }

-- | The ID of the Lambda function definition.
deleteFunctionDefinition_functionDefinitionId :: Lens.Lens' DeleteFunctionDefinition Prelude.Text
deleteFunctionDefinition_functionDefinitionId = Lens.lens (\DeleteFunctionDefinition' {functionDefinitionId} -> functionDefinitionId) (\s@DeleteFunctionDefinition' {} a -> s {functionDefinitionId = a} :: DeleteFunctionDefinition)

instance Prelude.AWSRequest DeleteFunctionDefinition where
  type
    Rs DeleteFunctionDefinition =
      DeleteFunctionDefinitionResponse
  request = Request.delete defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteFunctionDefinitionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteFunctionDefinition

instance Prelude.NFData DeleteFunctionDefinition

instance Prelude.ToHeaders DeleteFunctionDefinition where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToPath DeleteFunctionDefinition where
  toPath DeleteFunctionDefinition' {..} =
    Prelude.mconcat
      [ "/greengrass/definition/functions/",
        Prelude.toBS functionDefinitionId
      ]

instance Prelude.ToQuery DeleteFunctionDefinition where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteFunctionDefinitionResponse' smart constructor.
data DeleteFunctionDefinitionResponse = DeleteFunctionDefinitionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteFunctionDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteFunctionDefinitionResponse_httpStatus' - The response's http status code.
newDeleteFunctionDefinitionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteFunctionDefinitionResponse
newDeleteFunctionDefinitionResponse pHttpStatus_ =
  DeleteFunctionDefinitionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteFunctionDefinitionResponse_httpStatus :: Lens.Lens' DeleteFunctionDefinitionResponse Prelude.Int
deleteFunctionDefinitionResponse_httpStatus = Lens.lens (\DeleteFunctionDefinitionResponse' {httpStatus} -> httpStatus) (\s@DeleteFunctionDefinitionResponse' {} a -> s {httpStatus = a} :: DeleteFunctionDefinitionResponse)

instance
  Prelude.NFData
    DeleteFunctionDefinitionResponse
