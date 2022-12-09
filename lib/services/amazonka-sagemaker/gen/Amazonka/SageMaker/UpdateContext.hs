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
-- Module      : Amazonka.SageMaker.UpdateContext
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a context.
module Amazonka.SageMaker.UpdateContext
  ( -- * Creating a Request
    UpdateContext (..),
    newUpdateContext,

    -- * Request Lenses
    updateContext_description,
    updateContext_properties,
    updateContext_propertiesToRemove,
    updateContext_contextName,

    -- * Destructuring the Response
    UpdateContextResponse (..),
    newUpdateContextResponse,

    -- * Response Lenses
    updateContextResponse_contextArn,
    updateContextResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newUpdateContext' smart constructor.
data UpdateContext = UpdateContext'
  { -- | The new description for the context.
    description :: Prelude.Maybe Prelude.Text,
    -- | The new list of properties. Overwrites the current property list.
    properties :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A list of properties to remove.
    propertiesToRemove :: Prelude.Maybe [Prelude.Text],
    -- | The name of the context to update.
    contextName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateContext' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateContext_description' - The new description for the context.
--
-- 'properties', 'updateContext_properties' - The new list of properties. Overwrites the current property list.
--
-- 'propertiesToRemove', 'updateContext_propertiesToRemove' - A list of properties to remove.
--
-- 'contextName', 'updateContext_contextName' - The name of the context to update.
newUpdateContext ::
  -- | 'contextName'
  Prelude.Text ->
  UpdateContext
newUpdateContext pContextName_ =
  UpdateContext'
    { description = Prelude.Nothing,
      properties = Prelude.Nothing,
      propertiesToRemove = Prelude.Nothing,
      contextName = pContextName_
    }

-- | The new description for the context.
updateContext_description :: Lens.Lens' UpdateContext (Prelude.Maybe Prelude.Text)
updateContext_description = Lens.lens (\UpdateContext' {description} -> description) (\s@UpdateContext' {} a -> s {description = a} :: UpdateContext)

-- | The new list of properties. Overwrites the current property list.
updateContext_properties :: Lens.Lens' UpdateContext (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
updateContext_properties = Lens.lens (\UpdateContext' {properties} -> properties) (\s@UpdateContext' {} a -> s {properties = a} :: UpdateContext) Prelude.. Lens.mapping Lens.coerced

-- | A list of properties to remove.
updateContext_propertiesToRemove :: Lens.Lens' UpdateContext (Prelude.Maybe [Prelude.Text])
updateContext_propertiesToRemove = Lens.lens (\UpdateContext' {propertiesToRemove} -> propertiesToRemove) (\s@UpdateContext' {} a -> s {propertiesToRemove = a} :: UpdateContext) Prelude.. Lens.mapping Lens.coerced

-- | The name of the context to update.
updateContext_contextName :: Lens.Lens' UpdateContext Prelude.Text
updateContext_contextName = Lens.lens (\UpdateContext' {contextName} -> contextName) (\s@UpdateContext' {} a -> s {contextName = a} :: UpdateContext)

instance Core.AWSRequest UpdateContext where
  type
    AWSResponse UpdateContext =
      UpdateContextResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateContextResponse'
            Prelude.<$> (x Data..?> "ContextArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateContext where
  hashWithSalt _salt UpdateContext' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` properties
      `Prelude.hashWithSalt` propertiesToRemove
      `Prelude.hashWithSalt` contextName

instance Prelude.NFData UpdateContext where
  rnf UpdateContext' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf properties
      `Prelude.seq` Prelude.rnf propertiesToRemove
      `Prelude.seq` Prelude.rnf contextName

instance Data.ToHeaders UpdateContext where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("SageMaker.UpdateContext" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateContext where
  toJSON UpdateContext' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("Properties" Data..=) Prelude.<$> properties,
            ("PropertiesToRemove" Data..=)
              Prelude.<$> propertiesToRemove,
            Prelude.Just ("ContextName" Data..= contextName)
          ]
      )

instance Data.ToPath UpdateContext where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateContext where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateContextResponse' smart constructor.
data UpdateContextResponse = UpdateContextResponse'
  { -- | The Amazon Resource Name (ARN) of the context.
    contextArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateContextResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contextArn', 'updateContextResponse_contextArn' - The Amazon Resource Name (ARN) of the context.
--
-- 'httpStatus', 'updateContextResponse_httpStatus' - The response's http status code.
newUpdateContextResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateContextResponse
newUpdateContextResponse pHttpStatus_ =
  UpdateContextResponse'
    { contextArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the context.
updateContextResponse_contextArn :: Lens.Lens' UpdateContextResponse (Prelude.Maybe Prelude.Text)
updateContextResponse_contextArn = Lens.lens (\UpdateContextResponse' {contextArn} -> contextArn) (\s@UpdateContextResponse' {} a -> s {contextArn = a} :: UpdateContextResponse)

-- | The response's http status code.
updateContextResponse_httpStatus :: Lens.Lens' UpdateContextResponse Prelude.Int
updateContextResponse_httpStatus = Lens.lens (\UpdateContextResponse' {httpStatus} -> httpStatus) (\s@UpdateContextResponse' {} a -> s {httpStatus = a} :: UpdateContextResponse)

instance Prelude.NFData UpdateContextResponse where
  rnf UpdateContextResponse' {..} =
    Prelude.rnf contextArn
      `Prelude.seq` Prelude.rnf httpStatus
