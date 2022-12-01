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
-- Module      : Amazonka.Greengrass.DeleteLoggerDefinition
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a logger definition.
module Amazonka.Greengrass.DeleteLoggerDefinition
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Greengrass.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteLoggerDefinition' smart constructor.
data DeleteLoggerDefinition = DeleteLoggerDefinition'
  { -- | The ID of the logger definition.
    loggerDefinitionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest DeleteLoggerDefinition where
  type
    AWSResponse DeleteLoggerDefinition =
      DeleteLoggerDefinitionResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteLoggerDefinitionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteLoggerDefinition where
  hashWithSalt _salt DeleteLoggerDefinition' {..} =
    _salt `Prelude.hashWithSalt` loggerDefinitionId

instance Prelude.NFData DeleteLoggerDefinition where
  rnf DeleteLoggerDefinition' {..} =
    Prelude.rnf loggerDefinitionId

instance Core.ToHeaders DeleteLoggerDefinition where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DeleteLoggerDefinition where
  toPath DeleteLoggerDefinition' {..} =
    Prelude.mconcat
      [ "/greengrass/definition/loggers/",
        Core.toBS loggerDefinitionId
      ]

instance Core.ToQuery DeleteLoggerDefinition where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteLoggerDefinitionResponse' smart constructor.
data DeleteLoggerDefinitionResponse = DeleteLoggerDefinitionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf DeleteLoggerDefinitionResponse' {..} =
    Prelude.rnf httpStatus
