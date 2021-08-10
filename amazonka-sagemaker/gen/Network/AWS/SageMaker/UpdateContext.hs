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
-- Module      : Network.AWS.SageMaker.UpdateContext
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a context.
module Network.AWS.SageMaker.UpdateContext
  ( -- * Creating a Request
    UpdateContext (..),
    newUpdateContext,

    -- * Request Lenses
    updateContext_propertiesToRemove,
    updateContext_properties,
    updateContext_description,
    updateContext_contextName,

    -- * Destructuring the Response
    UpdateContextResponse (..),
    newUpdateContextResponse,

    -- * Response Lenses
    updateContextResponse_contextArn,
    updateContextResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newUpdateContext' smart constructor.
data UpdateContext = UpdateContext'
  { -- | A list of properties to remove.
    propertiesToRemove :: Prelude.Maybe [Prelude.Text],
    -- | The new list of properties. Overwrites the current property list.
    properties :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The new description for the context.
    description :: Prelude.Maybe Prelude.Text,
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
-- 'propertiesToRemove', 'updateContext_propertiesToRemove' - A list of properties to remove.
--
-- 'properties', 'updateContext_properties' - The new list of properties. Overwrites the current property list.
--
-- 'description', 'updateContext_description' - The new description for the context.
--
-- 'contextName', 'updateContext_contextName' - The name of the context to update.
newUpdateContext ::
  -- | 'contextName'
  Prelude.Text ->
  UpdateContext
newUpdateContext pContextName_ =
  UpdateContext'
    { propertiesToRemove =
        Prelude.Nothing,
      properties = Prelude.Nothing,
      description = Prelude.Nothing,
      contextName = pContextName_
    }

-- | A list of properties to remove.
updateContext_propertiesToRemove :: Lens.Lens' UpdateContext (Prelude.Maybe [Prelude.Text])
updateContext_propertiesToRemove = Lens.lens (\UpdateContext' {propertiesToRemove} -> propertiesToRemove) (\s@UpdateContext' {} a -> s {propertiesToRemove = a} :: UpdateContext) Prelude.. Lens.mapping Lens._Coerce

-- | The new list of properties. Overwrites the current property list.
updateContext_properties :: Lens.Lens' UpdateContext (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
updateContext_properties = Lens.lens (\UpdateContext' {properties} -> properties) (\s@UpdateContext' {} a -> s {properties = a} :: UpdateContext) Prelude.. Lens.mapping Lens._Coerce

-- | The new description for the context.
updateContext_description :: Lens.Lens' UpdateContext (Prelude.Maybe Prelude.Text)
updateContext_description = Lens.lens (\UpdateContext' {description} -> description) (\s@UpdateContext' {} a -> s {description = a} :: UpdateContext)

-- | The name of the context to update.
updateContext_contextName :: Lens.Lens' UpdateContext Prelude.Text
updateContext_contextName = Lens.lens (\UpdateContext' {contextName} -> contextName) (\s@UpdateContext' {} a -> s {contextName = a} :: UpdateContext)

instance Core.AWSRequest UpdateContext where
  type
    AWSResponse UpdateContext =
      UpdateContextResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateContextResponse'
            Prelude.<$> (x Core..?> "ContextArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateContext

instance Prelude.NFData UpdateContext

instance Core.ToHeaders UpdateContext where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.UpdateContext" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateContext where
  toJSON UpdateContext' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("PropertiesToRemove" Core..=)
              Prelude.<$> propertiesToRemove,
            ("Properties" Core..=) Prelude.<$> properties,
            ("Description" Core..=) Prelude.<$> description,
            Prelude.Just ("ContextName" Core..= contextName)
          ]
      )

instance Core.ToPath UpdateContext where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateContext where
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

instance Prelude.NFData UpdateContextResponse
