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
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newUpdateContext' smart constructor.
data UpdateContext = UpdateContext'
  { -- | A list of properties to remove.
    propertiesToRemove :: Core.Maybe [Core.Text],
    -- | The new list of properties. Overwrites the current property list.
    properties :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The new description for the context.
    description :: Core.Maybe Core.Text,
    -- | The name of the context to update.
    contextName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  UpdateContext
newUpdateContext pContextName_ =
  UpdateContext'
    { propertiesToRemove = Core.Nothing,
      properties = Core.Nothing,
      description = Core.Nothing,
      contextName = pContextName_
    }

-- | A list of properties to remove.
updateContext_propertiesToRemove :: Lens.Lens' UpdateContext (Core.Maybe [Core.Text])
updateContext_propertiesToRemove = Lens.lens (\UpdateContext' {propertiesToRemove} -> propertiesToRemove) (\s@UpdateContext' {} a -> s {propertiesToRemove = a} :: UpdateContext) Core.. Lens.mapping Lens._Coerce

-- | The new list of properties. Overwrites the current property list.
updateContext_properties :: Lens.Lens' UpdateContext (Core.Maybe (Core.HashMap Core.Text Core.Text))
updateContext_properties = Lens.lens (\UpdateContext' {properties} -> properties) (\s@UpdateContext' {} a -> s {properties = a} :: UpdateContext) Core.. Lens.mapping Lens._Coerce

-- | The new description for the context.
updateContext_description :: Lens.Lens' UpdateContext (Core.Maybe Core.Text)
updateContext_description = Lens.lens (\UpdateContext' {description} -> description) (\s@UpdateContext' {} a -> s {description = a} :: UpdateContext)

-- | The name of the context to update.
updateContext_contextName :: Lens.Lens' UpdateContext Core.Text
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
            Core.<$> (x Core..?> "ContextArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateContext

instance Core.NFData UpdateContext

instance Core.ToHeaders UpdateContext where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.UpdateContext" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateContext where
  toJSON UpdateContext' {..} =
    Core.object
      ( Core.catMaybes
          [ ("PropertiesToRemove" Core..=)
              Core.<$> propertiesToRemove,
            ("Properties" Core..=) Core.<$> properties,
            ("Description" Core..=) Core.<$> description,
            Core.Just ("ContextName" Core..= contextName)
          ]
      )

instance Core.ToPath UpdateContext where
  toPath = Core.const "/"

instance Core.ToQuery UpdateContext where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateContextResponse' smart constructor.
data UpdateContextResponse = UpdateContextResponse'
  { -- | The Amazon Resource Name (ARN) of the context.
    contextArn :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  UpdateContextResponse
newUpdateContextResponse pHttpStatus_ =
  UpdateContextResponse'
    { contextArn = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the context.
updateContextResponse_contextArn :: Lens.Lens' UpdateContextResponse (Core.Maybe Core.Text)
updateContextResponse_contextArn = Lens.lens (\UpdateContextResponse' {contextArn} -> contextArn) (\s@UpdateContextResponse' {} a -> s {contextArn = a} :: UpdateContextResponse)

-- | The response's http status code.
updateContextResponse_httpStatus :: Lens.Lens' UpdateContextResponse Core.Int
updateContextResponse_httpStatus = Lens.lens (\UpdateContextResponse' {httpStatus} -> httpStatus) (\s@UpdateContextResponse' {} a -> s {httpStatus = a} :: UpdateContextResponse)

instance Core.NFData UpdateContextResponse
