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
-- Module      : Network.AWS.Greengrass.DeleteLoggerDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a logger definition.
module Network.AWS.Greengrass.DeleteLoggerDefinition
  ( -- * Creating a Request
    DeleteLoggerDefinition (..),
    newDeleteLoggerDefinition,

    -- * Request Lenses
    deleteLoggerDefinition_loggerDefinitionId,

    -- * Destructuring the Response
    DeleteLoggerDefinitionResponse (..),
    newDeleteLoggerDefinitionResponse,

    -- * Response Lenses
    deleteLoggerDefinitionResponse_httpStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteLoggerDefinition' smart constructor.
data DeleteLoggerDefinition = DeleteLoggerDefinition'
  { -- | The ID of the logger definition.
    loggerDefinitionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteLoggerDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'loggerDefinitionId', 'deleteLoggerDefinition_loggerDefinitionId' - The ID of the logger definition.
newDeleteLoggerDefinition ::
  -- | 'loggerDefinitionId'
  Prelude.Text ->
  DeleteLoggerDefinition
newDeleteLoggerDefinition pLoggerDefinitionId_ =
  DeleteLoggerDefinition'
    { loggerDefinitionId =
        pLoggerDefinitionId_
    }

-- | The ID of the logger definition.
deleteLoggerDefinition_loggerDefinitionId :: Lens.Lens' DeleteLoggerDefinition Prelude.Text
deleteLoggerDefinition_loggerDefinitionId = Lens.lens (\DeleteLoggerDefinition' {loggerDefinitionId} -> loggerDefinitionId) (\s@DeleteLoggerDefinition' {} a -> s {loggerDefinitionId = a} :: DeleteLoggerDefinition)

instance Prelude.AWSRequest DeleteLoggerDefinition where
  type
    Rs DeleteLoggerDefinition =
      DeleteLoggerDefinitionResponse
  request = Request.delete defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteLoggerDefinitionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteLoggerDefinition

instance Prelude.NFData DeleteLoggerDefinition

instance Prelude.ToHeaders DeleteLoggerDefinition where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToPath DeleteLoggerDefinition where
  toPath DeleteLoggerDefinition' {..} =
    Prelude.mconcat
      [ "/greengrass/definition/loggers/",
        Prelude.toBS loggerDefinitionId
      ]

instance Prelude.ToQuery DeleteLoggerDefinition where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteLoggerDefinitionResponse' smart constructor.
data DeleteLoggerDefinitionResponse = DeleteLoggerDefinitionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteLoggerDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteLoggerDefinitionResponse_httpStatus' - The response's http status code.
newDeleteLoggerDefinitionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteLoggerDefinitionResponse
newDeleteLoggerDefinitionResponse pHttpStatus_ =
  DeleteLoggerDefinitionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteLoggerDefinitionResponse_httpStatus :: Lens.Lens' DeleteLoggerDefinitionResponse Prelude.Int
deleteLoggerDefinitionResponse_httpStatus = Lens.lens (\DeleteLoggerDefinitionResponse' {httpStatus} -> httpStatus) (\s@DeleteLoggerDefinitionResponse' {} a -> s {httpStatus = a} :: DeleteLoggerDefinitionResponse)

instance
  Prelude.NFData
    DeleteLoggerDefinitionResponse
