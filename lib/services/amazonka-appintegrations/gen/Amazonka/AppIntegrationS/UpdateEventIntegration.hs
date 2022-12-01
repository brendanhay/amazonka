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
-- Module      : Amazonka.AppIntegrationS.UpdateEventIntegration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the description of an event integration.
module Amazonka.AppIntegrationS.UpdateEventIntegration
  ( -- * Creating a Request
    UpdateEventIntegration (..),
    newUpdateEventIntegration,

    -- * Request Lenses
    updateEventIntegration_description,
    updateEventIntegration_name,

    -- * Destructuring the Response
    UpdateEventIntegrationResponse (..),
    newUpdateEventIntegrationResponse,

    -- * Response Lenses
    updateEventIntegrationResponse_httpStatus,
  )
where

import Amazonka.AppIntegrationS.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateEventIntegration' smart constructor.
data UpdateEventIntegration = UpdateEventIntegration'
  { -- | The description of the event inegration.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the event integration.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateEventIntegration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateEventIntegration_description' - The description of the event inegration.
--
-- 'name', 'updateEventIntegration_name' - The name of the event integration.
newUpdateEventIntegration ::
  -- | 'name'
  Prelude.Text ->
  UpdateEventIntegration
newUpdateEventIntegration pName_ =
  UpdateEventIntegration'
    { description =
        Prelude.Nothing,
      name = pName_
    }

-- | The description of the event inegration.
updateEventIntegration_description :: Lens.Lens' UpdateEventIntegration (Prelude.Maybe Prelude.Text)
updateEventIntegration_description = Lens.lens (\UpdateEventIntegration' {description} -> description) (\s@UpdateEventIntegration' {} a -> s {description = a} :: UpdateEventIntegration)

-- | The name of the event integration.
updateEventIntegration_name :: Lens.Lens' UpdateEventIntegration Prelude.Text
updateEventIntegration_name = Lens.lens (\UpdateEventIntegration' {name} -> name) (\s@UpdateEventIntegration' {} a -> s {name = a} :: UpdateEventIntegration)

instance Core.AWSRequest UpdateEventIntegration where
  type
    AWSResponse UpdateEventIntegration =
      UpdateEventIntegrationResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateEventIntegrationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateEventIntegration where
  hashWithSalt _salt UpdateEventIntegration' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name

instance Prelude.NFData UpdateEventIntegration where
  rnf UpdateEventIntegration' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf name

instance Core.ToHeaders UpdateEventIntegration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateEventIntegration where
  toJSON UpdateEventIntegration' {..} =
    Core.object
      ( Prelude.catMaybes
          [("Description" Core..=) Prelude.<$> description]
      )

instance Core.ToPath UpdateEventIntegration where
  toPath UpdateEventIntegration' {..} =
    Prelude.mconcat
      ["/eventIntegrations/", Core.toBS name]

instance Core.ToQuery UpdateEventIntegration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateEventIntegrationResponse' smart constructor.
data UpdateEventIntegrationResponse = UpdateEventIntegrationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateEventIntegrationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateEventIntegrationResponse_httpStatus' - The response's http status code.
newUpdateEventIntegrationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateEventIntegrationResponse
newUpdateEventIntegrationResponse pHttpStatus_ =
  UpdateEventIntegrationResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateEventIntegrationResponse_httpStatus :: Lens.Lens' UpdateEventIntegrationResponse Prelude.Int
updateEventIntegrationResponse_httpStatus = Lens.lens (\UpdateEventIntegrationResponse' {httpStatus} -> httpStatus) (\s@UpdateEventIntegrationResponse' {} a -> s {httpStatus = a} :: UpdateEventIntegrationResponse)

instance
  Prelude.NFData
    UpdateEventIntegrationResponse
  where
  rnf UpdateEventIntegrationResponse' {..} =
    Prelude.rnf httpStatus
