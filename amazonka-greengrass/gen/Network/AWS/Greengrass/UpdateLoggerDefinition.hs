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
-- Module      : Network.AWS.Greengrass.UpdateLoggerDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a logger definition.
module Network.AWS.Greengrass.UpdateLoggerDefinition
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

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateLoggerDefinition' smart constructor.
data UpdateLoggerDefinition = UpdateLoggerDefinition'
  { -- | The name of the definition.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ID of the logger definition.
    loggerDefinitionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest UpdateLoggerDefinition where
  type
    Rs UpdateLoggerDefinition =
      UpdateLoggerDefinitionResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateLoggerDefinitionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateLoggerDefinition

instance Prelude.NFData UpdateLoggerDefinition

instance Prelude.ToHeaders UpdateLoggerDefinition where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateLoggerDefinition where
  toJSON UpdateLoggerDefinition' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("Name" Prelude..=) Prelude.<$> name]
      )

instance Prelude.ToPath UpdateLoggerDefinition where
  toPath UpdateLoggerDefinition' {..} =
    Prelude.mconcat
      [ "/greengrass/definition/loggers/",
        Prelude.toBS loggerDefinitionId
      ]

instance Prelude.ToQuery UpdateLoggerDefinition where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateLoggerDefinitionResponse' smart constructor.
data UpdateLoggerDefinitionResponse = UpdateLoggerDefinitionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
