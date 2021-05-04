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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'newUpdateServiceAction' smart constructor.
data UpdateServiceAction = UpdateServiceAction'
  { -- | The self-service action name.
    name :: Prelude.Maybe Prelude.Text,
    -- | The self-service action description.
    description :: Prelude.Maybe Prelude.Text,
    -- | A map that defines the self-service action.
    definition :: Prelude.Maybe (Prelude.HashMap ServiceActionDefinitionKey Prelude.Text),
    -- | The language code.
    --
    -- -   @en@ - English (default)
    --
    -- -   @jp@ - Japanese
    --
    -- -   @zh@ - Chinese
    acceptLanguage :: Prelude.Maybe Prelude.Text,
    -- | The self-service action identifier.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  UpdateServiceAction
newUpdateServiceAction pId_ =
  UpdateServiceAction'
    { name = Prelude.Nothing,
      description = Prelude.Nothing,
      definition = Prelude.Nothing,
      acceptLanguage = Prelude.Nothing,
      id = pId_
    }

-- | The self-service action name.
updateServiceAction_name :: Lens.Lens' UpdateServiceAction (Prelude.Maybe Prelude.Text)
updateServiceAction_name = Lens.lens (\UpdateServiceAction' {name} -> name) (\s@UpdateServiceAction' {} a -> s {name = a} :: UpdateServiceAction)

-- | The self-service action description.
updateServiceAction_description :: Lens.Lens' UpdateServiceAction (Prelude.Maybe Prelude.Text)
updateServiceAction_description = Lens.lens (\UpdateServiceAction' {description} -> description) (\s@UpdateServiceAction' {} a -> s {description = a} :: UpdateServiceAction)

-- | A map that defines the self-service action.
updateServiceAction_definition :: Lens.Lens' UpdateServiceAction (Prelude.Maybe (Prelude.HashMap ServiceActionDefinitionKey Prelude.Text))
updateServiceAction_definition = Lens.lens (\UpdateServiceAction' {definition} -> definition) (\s@UpdateServiceAction' {} a -> s {definition = a} :: UpdateServiceAction) Prelude.. Lens.mapping Prelude._Coerce

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
updateServiceAction_acceptLanguage :: Lens.Lens' UpdateServiceAction (Prelude.Maybe Prelude.Text)
updateServiceAction_acceptLanguage = Lens.lens (\UpdateServiceAction' {acceptLanguage} -> acceptLanguage) (\s@UpdateServiceAction' {} a -> s {acceptLanguage = a} :: UpdateServiceAction)

-- | The self-service action identifier.
updateServiceAction_id :: Lens.Lens' UpdateServiceAction Prelude.Text
updateServiceAction_id = Lens.lens (\UpdateServiceAction' {id} -> id) (\s@UpdateServiceAction' {} a -> s {id = a} :: UpdateServiceAction)

instance Prelude.AWSRequest UpdateServiceAction where
  type
    Rs UpdateServiceAction =
      UpdateServiceActionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateServiceActionResponse'
            Prelude.<$> (x Prelude..?> "ServiceActionDetail")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateServiceAction

instance Prelude.NFData UpdateServiceAction

instance Prelude.ToHeaders UpdateServiceAction where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWS242ServiceCatalogService.UpdateServiceAction" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateServiceAction where
  toJSON UpdateServiceAction' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Name" Prelude..=) Prelude.<$> name,
            ("Description" Prelude..=) Prelude.<$> description,
            ("Definition" Prelude..=) Prelude.<$> definition,
            ("AcceptLanguage" Prelude..=)
              Prelude.<$> acceptLanguage,
            Prelude.Just ("Id" Prelude..= id)
          ]
      )

instance Prelude.ToPath UpdateServiceAction where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UpdateServiceAction where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateServiceActionResponse' smart constructor.
data UpdateServiceActionResponse = UpdateServiceActionResponse'
  { -- | Detailed information about the self-service action.
    serviceActionDetail :: Prelude.Maybe ServiceActionDetail,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  UpdateServiceActionResponse
newUpdateServiceActionResponse pHttpStatus_ =
  UpdateServiceActionResponse'
    { serviceActionDetail =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Detailed information about the self-service action.
updateServiceActionResponse_serviceActionDetail :: Lens.Lens' UpdateServiceActionResponse (Prelude.Maybe ServiceActionDetail)
updateServiceActionResponse_serviceActionDetail = Lens.lens (\UpdateServiceActionResponse' {serviceActionDetail} -> serviceActionDetail) (\s@UpdateServiceActionResponse' {} a -> s {serviceActionDetail = a} :: UpdateServiceActionResponse)

-- | The response's http status code.
updateServiceActionResponse_httpStatus :: Lens.Lens' UpdateServiceActionResponse Prelude.Int
updateServiceActionResponse_httpStatus = Lens.lens (\UpdateServiceActionResponse' {httpStatus} -> httpStatus) (\s@UpdateServiceActionResponse' {} a -> s {httpStatus = a} :: UpdateServiceActionResponse)

instance Prelude.NFData UpdateServiceActionResponse
