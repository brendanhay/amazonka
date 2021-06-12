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
-- Module      : Network.AWS.ServiceCatalog.UpdateServiceAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a self-service action.
module Network.AWS.ServiceCatalog.UpdateServiceAction
  ( -- * Creating a Request
    UpdateServiceAction (..),
    newUpdateServiceAction,

    -- * Request Lenses
    updateServiceAction_name,
    updateServiceAction_description,
    updateServiceAction_definition,
    updateServiceAction_acceptLanguage,
    updateServiceAction_id,

    -- * Destructuring the Response
    UpdateServiceActionResponse (..),
    newUpdateServiceActionResponse,

    -- * Response Lenses
    updateServiceActionResponse_serviceActionDetail,
    updateServiceActionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'newUpdateServiceAction' smart constructor.
data UpdateServiceAction = UpdateServiceAction'
  { -- | The self-service action name.
    name :: Core.Maybe Core.Text,
    -- | The self-service action description.
    description :: Core.Maybe Core.Text,
    -- | A map that defines the self-service action.
    definition :: Core.Maybe (Core.HashMap ServiceActionDefinitionKey Core.Text),
    -- | The language code.
    --
    -- -   @en@ - English (default)
    --
    -- -   @jp@ - Japanese
    --
    -- -   @zh@ - Chinese
    acceptLanguage :: Core.Maybe Core.Text,
    -- | The self-service action identifier.
    id :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateServiceAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'updateServiceAction_name' - The self-service action name.
--
-- 'description', 'updateServiceAction_description' - The self-service action description.
--
-- 'definition', 'updateServiceAction_definition' - A map that defines the self-service action.
--
-- 'acceptLanguage', 'updateServiceAction_acceptLanguage' - The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
--
-- 'id', 'updateServiceAction_id' - The self-service action identifier.
newUpdateServiceAction ::
  -- | 'id'
  Core.Text ->
  UpdateServiceAction
newUpdateServiceAction pId_ =
  UpdateServiceAction'
    { name = Core.Nothing,
      description = Core.Nothing,
      definition = Core.Nothing,
      acceptLanguage = Core.Nothing,
      id = pId_
    }

-- | The self-service action name.
updateServiceAction_name :: Lens.Lens' UpdateServiceAction (Core.Maybe Core.Text)
updateServiceAction_name = Lens.lens (\UpdateServiceAction' {name} -> name) (\s@UpdateServiceAction' {} a -> s {name = a} :: UpdateServiceAction)

-- | The self-service action description.
updateServiceAction_description :: Lens.Lens' UpdateServiceAction (Core.Maybe Core.Text)
updateServiceAction_description = Lens.lens (\UpdateServiceAction' {description} -> description) (\s@UpdateServiceAction' {} a -> s {description = a} :: UpdateServiceAction)

-- | A map that defines the self-service action.
updateServiceAction_definition :: Lens.Lens' UpdateServiceAction (Core.Maybe (Core.HashMap ServiceActionDefinitionKey Core.Text))
updateServiceAction_definition = Lens.lens (\UpdateServiceAction' {definition} -> definition) (\s@UpdateServiceAction' {} a -> s {definition = a} :: UpdateServiceAction) Core.. Lens.mapping Lens._Coerce

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
updateServiceAction_acceptLanguage :: Lens.Lens' UpdateServiceAction (Core.Maybe Core.Text)
updateServiceAction_acceptLanguage = Lens.lens (\UpdateServiceAction' {acceptLanguage} -> acceptLanguage) (\s@UpdateServiceAction' {} a -> s {acceptLanguage = a} :: UpdateServiceAction)

-- | The self-service action identifier.
updateServiceAction_id :: Lens.Lens' UpdateServiceAction Core.Text
updateServiceAction_id = Lens.lens (\UpdateServiceAction' {id} -> id) (\s@UpdateServiceAction' {} a -> s {id = a} :: UpdateServiceAction)

instance Core.AWSRequest UpdateServiceAction where
  type
    AWSResponse UpdateServiceAction =
      UpdateServiceActionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateServiceActionResponse'
            Core.<$> (x Core..?> "ServiceActionDetail")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateServiceAction

instance Core.NFData UpdateServiceAction

instance Core.ToHeaders UpdateServiceAction where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWS242ServiceCatalogService.UpdateServiceAction" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateServiceAction where
  toJSON UpdateServiceAction' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Name" Core..=) Core.<$> name,
            ("Description" Core..=) Core.<$> description,
            ("Definition" Core..=) Core.<$> definition,
            ("AcceptLanguage" Core..=) Core.<$> acceptLanguage,
            Core.Just ("Id" Core..= id)
          ]
      )

instance Core.ToPath UpdateServiceAction where
  toPath = Core.const "/"

instance Core.ToQuery UpdateServiceAction where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateServiceActionResponse' smart constructor.
data UpdateServiceActionResponse = UpdateServiceActionResponse'
  { -- | Detailed information about the self-service action.
    serviceActionDetail :: Core.Maybe ServiceActionDetail,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateServiceActionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceActionDetail', 'updateServiceActionResponse_serviceActionDetail' - Detailed information about the self-service action.
--
-- 'httpStatus', 'updateServiceActionResponse_httpStatus' - The response's http status code.
newUpdateServiceActionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateServiceActionResponse
newUpdateServiceActionResponse pHttpStatus_ =
  UpdateServiceActionResponse'
    { serviceActionDetail =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Detailed information about the self-service action.
updateServiceActionResponse_serviceActionDetail :: Lens.Lens' UpdateServiceActionResponse (Core.Maybe ServiceActionDetail)
updateServiceActionResponse_serviceActionDetail = Lens.lens (\UpdateServiceActionResponse' {serviceActionDetail} -> serviceActionDetail) (\s@UpdateServiceActionResponse' {} a -> s {serviceActionDetail = a} :: UpdateServiceActionResponse)

-- | The response's http status code.
updateServiceActionResponse_httpStatus :: Lens.Lens' UpdateServiceActionResponse Core.Int
updateServiceActionResponse_httpStatus = Lens.lens (\UpdateServiceActionResponse' {httpStatus} -> httpStatus) (\s@UpdateServiceActionResponse' {} a -> s {httpStatus = a} :: UpdateServiceActionResponse)

instance Core.NFData UpdateServiceActionResponse
