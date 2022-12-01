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
-- Module      : Amazonka.IoTFleetWise.UpdateSignalCatalog
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a signal catalog.
module Amazonka.IoTFleetWise.UpdateSignalCatalog
  ( -- * Creating a Request
    UpdateSignalCatalog (..),
    newUpdateSignalCatalog,

    -- * Request Lenses
    updateSignalCatalog_nodesToRemove,
    updateSignalCatalog_nodesToAdd,
    updateSignalCatalog_description,
    updateSignalCatalog_nodesToUpdate,
    updateSignalCatalog_name,

    -- * Destructuring the Response
    UpdateSignalCatalogResponse (..),
    newUpdateSignalCatalogResponse,

    -- * Response Lenses
    updateSignalCatalogResponse_httpStatus,
    updateSignalCatalogResponse_name,
    updateSignalCatalogResponse_arn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTFleetWise.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateSignalCatalog' smart constructor.
data UpdateSignalCatalog = UpdateSignalCatalog'
  { -- | A list of @fullyQualifiedName@ of nodes to remove from the signal
    -- catalog.
    nodesToRemove :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | A list of information about nodes to add to the signal catalog.
    nodesToAdd :: Prelude.Maybe [Node],
    -- | A brief description of the signal catalog to update.
    description :: Prelude.Maybe Prelude.Text,
    -- | A list of information about nodes to update in the signal catalog.
    nodesToUpdate :: Prelude.Maybe [Node],
    -- | The name of the signal catalog to update.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSignalCatalog' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nodesToRemove', 'updateSignalCatalog_nodesToRemove' - A list of @fullyQualifiedName@ of nodes to remove from the signal
-- catalog.
--
-- 'nodesToAdd', 'updateSignalCatalog_nodesToAdd' - A list of information about nodes to add to the signal catalog.
--
-- 'description', 'updateSignalCatalog_description' - A brief description of the signal catalog to update.
--
-- 'nodesToUpdate', 'updateSignalCatalog_nodesToUpdate' - A list of information about nodes to update in the signal catalog.
--
-- 'name', 'updateSignalCatalog_name' - The name of the signal catalog to update.
newUpdateSignalCatalog ::
  -- | 'name'
  Prelude.Text ->
  UpdateSignalCatalog
newUpdateSignalCatalog pName_ =
  UpdateSignalCatalog'
    { nodesToRemove =
        Prelude.Nothing,
      nodesToAdd = Prelude.Nothing,
      description = Prelude.Nothing,
      nodesToUpdate = Prelude.Nothing,
      name = pName_
    }

-- | A list of @fullyQualifiedName@ of nodes to remove from the signal
-- catalog.
updateSignalCatalog_nodesToRemove :: Lens.Lens' UpdateSignalCatalog (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
updateSignalCatalog_nodesToRemove = Lens.lens (\UpdateSignalCatalog' {nodesToRemove} -> nodesToRemove) (\s@UpdateSignalCatalog' {} a -> s {nodesToRemove = a} :: UpdateSignalCatalog) Prelude.. Lens.mapping Lens.coerced

-- | A list of information about nodes to add to the signal catalog.
updateSignalCatalog_nodesToAdd :: Lens.Lens' UpdateSignalCatalog (Prelude.Maybe [Node])
updateSignalCatalog_nodesToAdd = Lens.lens (\UpdateSignalCatalog' {nodesToAdd} -> nodesToAdd) (\s@UpdateSignalCatalog' {} a -> s {nodesToAdd = a} :: UpdateSignalCatalog) Prelude.. Lens.mapping Lens.coerced

-- | A brief description of the signal catalog to update.
updateSignalCatalog_description :: Lens.Lens' UpdateSignalCatalog (Prelude.Maybe Prelude.Text)
updateSignalCatalog_description = Lens.lens (\UpdateSignalCatalog' {description} -> description) (\s@UpdateSignalCatalog' {} a -> s {description = a} :: UpdateSignalCatalog)

-- | A list of information about nodes to update in the signal catalog.
updateSignalCatalog_nodesToUpdate :: Lens.Lens' UpdateSignalCatalog (Prelude.Maybe [Node])
updateSignalCatalog_nodesToUpdate = Lens.lens (\UpdateSignalCatalog' {nodesToUpdate} -> nodesToUpdate) (\s@UpdateSignalCatalog' {} a -> s {nodesToUpdate = a} :: UpdateSignalCatalog) Prelude.. Lens.mapping Lens.coerced

-- | The name of the signal catalog to update.
updateSignalCatalog_name :: Lens.Lens' UpdateSignalCatalog Prelude.Text
updateSignalCatalog_name = Lens.lens (\UpdateSignalCatalog' {name} -> name) (\s@UpdateSignalCatalog' {} a -> s {name = a} :: UpdateSignalCatalog)

instance Core.AWSRequest UpdateSignalCatalog where
  type
    AWSResponse UpdateSignalCatalog =
      UpdateSignalCatalogResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateSignalCatalogResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "name")
            Prelude.<*> (x Core..:> "arn")
      )

instance Prelude.Hashable UpdateSignalCatalog where
  hashWithSalt _salt UpdateSignalCatalog' {..} =
    _salt `Prelude.hashWithSalt` nodesToRemove
      `Prelude.hashWithSalt` nodesToAdd
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` nodesToUpdate
      `Prelude.hashWithSalt` name

instance Prelude.NFData UpdateSignalCatalog where
  rnf UpdateSignalCatalog' {..} =
    Prelude.rnf nodesToRemove
      `Prelude.seq` Prelude.rnf nodesToAdd
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf nodesToUpdate
      `Prelude.seq` Prelude.rnf name

instance Core.ToHeaders UpdateSignalCatalog where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "IoTAutobahnControlPlane.UpdateSignalCatalog" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateSignalCatalog where
  toJSON UpdateSignalCatalog' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("nodesToRemove" Core..=) Prelude.<$> nodesToRemove,
            ("nodesToAdd" Core..=) Prelude.<$> nodesToAdd,
            ("description" Core..=) Prelude.<$> description,
            ("nodesToUpdate" Core..=) Prelude.<$> nodesToUpdate,
            Prelude.Just ("name" Core..= name)
          ]
      )

instance Core.ToPath UpdateSignalCatalog where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateSignalCatalog where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateSignalCatalogResponse' smart constructor.
data UpdateSignalCatalogResponse = UpdateSignalCatalogResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The name of the updated signal catalog.
    name :: Prelude.Text,
    -- | The ARN of the updated signal catalog.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSignalCatalogResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateSignalCatalogResponse_httpStatus' - The response's http status code.
--
-- 'name', 'updateSignalCatalogResponse_name' - The name of the updated signal catalog.
--
-- 'arn', 'updateSignalCatalogResponse_arn' - The ARN of the updated signal catalog.
newUpdateSignalCatalogResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'name'
  Prelude.Text ->
  -- | 'arn'
  Prelude.Text ->
  UpdateSignalCatalogResponse
newUpdateSignalCatalogResponse
  pHttpStatus_
  pName_
  pArn_ =
    UpdateSignalCatalogResponse'
      { httpStatus =
          pHttpStatus_,
        name = pName_,
        arn = pArn_
      }

-- | The response's http status code.
updateSignalCatalogResponse_httpStatus :: Lens.Lens' UpdateSignalCatalogResponse Prelude.Int
updateSignalCatalogResponse_httpStatus = Lens.lens (\UpdateSignalCatalogResponse' {httpStatus} -> httpStatus) (\s@UpdateSignalCatalogResponse' {} a -> s {httpStatus = a} :: UpdateSignalCatalogResponse)

-- | The name of the updated signal catalog.
updateSignalCatalogResponse_name :: Lens.Lens' UpdateSignalCatalogResponse Prelude.Text
updateSignalCatalogResponse_name = Lens.lens (\UpdateSignalCatalogResponse' {name} -> name) (\s@UpdateSignalCatalogResponse' {} a -> s {name = a} :: UpdateSignalCatalogResponse)

-- | The ARN of the updated signal catalog.
updateSignalCatalogResponse_arn :: Lens.Lens' UpdateSignalCatalogResponse Prelude.Text
updateSignalCatalogResponse_arn = Lens.lens (\UpdateSignalCatalogResponse' {arn} -> arn) (\s@UpdateSignalCatalogResponse' {} a -> s {arn = a} :: UpdateSignalCatalogResponse)

instance Prelude.NFData UpdateSignalCatalogResponse where
  rnf UpdateSignalCatalogResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf arn
