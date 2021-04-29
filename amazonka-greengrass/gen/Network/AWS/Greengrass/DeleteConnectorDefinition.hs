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
-- Module      : Network.AWS.Greengrass.DeleteConnectorDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a connector definition.
module Network.AWS.Greengrass.DeleteConnectorDefinition
  ( -- * Creating a Request
    DeleteConnectorDefinition (..),
    newDeleteConnectorDefinition,

    -- * Request Lenses
    deleteConnectorDefinition_connectorDefinitionId,

    -- * Destructuring the Response
    DeleteConnectorDefinitionResponse (..),
    newDeleteConnectorDefinitionResponse,

    -- * Response Lenses
    deleteConnectorDefinitionResponse_httpStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteConnectorDefinition' smart constructor.
data DeleteConnectorDefinition = DeleteConnectorDefinition'
  { -- | The ID of the connector definition.
    connectorDefinitionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteConnectorDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectorDefinitionId', 'deleteConnectorDefinition_connectorDefinitionId' - The ID of the connector definition.
newDeleteConnectorDefinition ::
  -- | 'connectorDefinitionId'
  Prelude.Text ->
  DeleteConnectorDefinition
newDeleteConnectorDefinition pConnectorDefinitionId_ =
  DeleteConnectorDefinition'
    { connectorDefinitionId =
        pConnectorDefinitionId_
    }

-- | The ID of the connector definition.
deleteConnectorDefinition_connectorDefinitionId :: Lens.Lens' DeleteConnectorDefinition Prelude.Text
deleteConnectorDefinition_connectorDefinitionId = Lens.lens (\DeleteConnectorDefinition' {connectorDefinitionId} -> connectorDefinitionId) (\s@DeleteConnectorDefinition' {} a -> s {connectorDefinitionId = a} :: DeleteConnectorDefinition)

instance Prelude.AWSRequest DeleteConnectorDefinition where
  type
    Rs DeleteConnectorDefinition =
      DeleteConnectorDefinitionResponse
  request = Request.delete defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteConnectorDefinitionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteConnectorDefinition

instance Prelude.NFData DeleteConnectorDefinition

instance Prelude.ToHeaders DeleteConnectorDefinition where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToPath DeleteConnectorDefinition where
  toPath DeleteConnectorDefinition' {..} =
    Prelude.mconcat
      [ "/greengrass/definition/connectors/",
        Prelude.toBS connectorDefinitionId
      ]

instance Prelude.ToQuery DeleteConnectorDefinition where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteConnectorDefinitionResponse' smart constructor.
data DeleteConnectorDefinitionResponse = DeleteConnectorDefinitionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteConnectorDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteConnectorDefinitionResponse_httpStatus' - The response's http status code.
newDeleteConnectorDefinitionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteConnectorDefinitionResponse
newDeleteConnectorDefinitionResponse pHttpStatus_ =
  DeleteConnectorDefinitionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteConnectorDefinitionResponse_httpStatus :: Lens.Lens' DeleteConnectorDefinitionResponse Prelude.Int
deleteConnectorDefinitionResponse_httpStatus = Lens.lens (\DeleteConnectorDefinitionResponse' {httpStatus} -> httpStatus) (\s@DeleteConnectorDefinitionResponse' {} a -> s {httpStatus = a} :: DeleteConnectorDefinitionResponse)

instance
  Prelude.NFData
    DeleteConnectorDefinitionResponse
