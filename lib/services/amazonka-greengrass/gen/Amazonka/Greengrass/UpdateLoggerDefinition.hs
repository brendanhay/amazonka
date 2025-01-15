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
-- Module      : Amazonka.Greengrass.UpdateLoggerDefinition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a logger definition.
module Amazonka.Greengrass.UpdateLoggerDefinition
  ( -- * Creating a Request
    UpdateLoggerDefinition (..),
    newUpdateLoggerDefinition,

    -- * Request Lenses
    updateLoggerDefinition_name,
    updateLoggerDefinition_loggerDefinitionId,

    -- * Destructuring the Response
    UpdateLoggerDefinitionResponse (..),
    newUpdateLoggerDefinitionResponse,

    -- * Response Lenses
    updateLoggerDefinitionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Greengrass.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateLoggerDefinition' smart constructor.
data UpdateLoggerDefinition = UpdateLoggerDefinition'
  { -- | The name of the definition.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ID of the logger definition.
    loggerDefinitionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateLoggerDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'updateLoggerDefinition_name' - The name of the definition.
--
-- 'loggerDefinitionId', 'updateLoggerDefinition_loggerDefinitionId' - The ID of the logger definition.
newUpdateLoggerDefinition ::
  -- | 'loggerDefinitionId'
  Prelude.Text ->
  UpdateLoggerDefinition
newUpdateLoggerDefinition pLoggerDefinitionId_ =
  UpdateLoggerDefinition'
    { name = Prelude.Nothing,
      loggerDefinitionId = pLoggerDefinitionId_
    }

-- | The name of the definition.
updateLoggerDefinition_name :: Lens.Lens' UpdateLoggerDefinition (Prelude.Maybe Prelude.Text)
updateLoggerDefinition_name = Lens.lens (\UpdateLoggerDefinition' {name} -> name) (\s@UpdateLoggerDefinition' {} a -> s {name = a} :: UpdateLoggerDefinition)

-- | The ID of the logger definition.
updateLoggerDefinition_loggerDefinitionId :: Lens.Lens' UpdateLoggerDefinition Prelude.Text
updateLoggerDefinition_loggerDefinitionId = Lens.lens (\UpdateLoggerDefinition' {loggerDefinitionId} -> loggerDefinitionId) (\s@UpdateLoggerDefinition' {} a -> s {loggerDefinitionId = a} :: UpdateLoggerDefinition)

instance Core.AWSRequest UpdateLoggerDefinition where
  type
    AWSResponse UpdateLoggerDefinition =
      UpdateLoggerDefinitionResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateLoggerDefinitionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateLoggerDefinition where
  hashWithSalt _salt UpdateLoggerDefinition' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` loggerDefinitionId

instance Prelude.NFData UpdateLoggerDefinition where
  rnf UpdateLoggerDefinition' {..} =
    Prelude.rnf name `Prelude.seq`
      Prelude.rnf loggerDefinitionId

instance Data.ToHeaders UpdateLoggerDefinition where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateLoggerDefinition where
  toJSON UpdateLoggerDefinition' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Name" Data..=) Prelude.<$> name]
      )

instance Data.ToPath UpdateLoggerDefinition where
  toPath UpdateLoggerDefinition' {..} =
    Prelude.mconcat
      [ "/greengrass/definition/loggers/",
        Data.toBS loggerDefinitionId
      ]

instance Data.ToQuery UpdateLoggerDefinition where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateLoggerDefinitionResponse' smart constructor.
data UpdateLoggerDefinitionResponse = UpdateLoggerDefinitionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateLoggerDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateLoggerDefinitionResponse_httpStatus' - The response's http status code.
newUpdateLoggerDefinitionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateLoggerDefinitionResponse
newUpdateLoggerDefinitionResponse pHttpStatus_ =
  UpdateLoggerDefinitionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateLoggerDefinitionResponse_httpStatus :: Lens.Lens' UpdateLoggerDefinitionResponse Prelude.Int
updateLoggerDefinitionResponse_httpStatus = Lens.lens (\UpdateLoggerDefinitionResponse' {httpStatus} -> httpStatus) (\s@UpdateLoggerDefinitionResponse' {} a -> s {httpStatus = a} :: UpdateLoggerDefinitionResponse)

instance
  Prelude.NFData
    UpdateLoggerDefinitionResponse
  where
  rnf UpdateLoggerDefinitionResponse' {..} =
    Prelude.rnf httpStatus
