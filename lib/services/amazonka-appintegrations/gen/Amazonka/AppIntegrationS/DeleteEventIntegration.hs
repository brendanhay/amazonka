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
-- Module      : Amazonka.AppIntegrationS.DeleteEventIntegration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified existing event integration. If the event
-- integration is associated with clients, the request is rejected.
module Amazonka.AppIntegrationS.DeleteEventIntegration
  ( -- * Creating a Request
    DeleteEventIntegration (..),
    newDeleteEventIntegration,

    -- * Request Lenses
    deleteEventIntegration_name,

    -- * Destructuring the Response
    DeleteEventIntegrationResponse (..),
    newDeleteEventIntegrationResponse,

    -- * Response Lenses
    deleteEventIntegrationResponse_httpStatus,
  )
where

import Amazonka.AppIntegrationS.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteEventIntegration' smart constructor.
data DeleteEventIntegration = DeleteEventIntegration'
  { -- | The name of the event integration.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteEventIntegration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'deleteEventIntegration_name' - The name of the event integration.
newDeleteEventIntegration ::
  -- | 'name'
  Prelude.Text ->
  DeleteEventIntegration
newDeleteEventIntegration pName_ =
  DeleteEventIntegration' {name = pName_}

-- | The name of the event integration.
deleteEventIntegration_name :: Lens.Lens' DeleteEventIntegration Prelude.Text
deleteEventIntegration_name = Lens.lens (\DeleteEventIntegration' {name} -> name) (\s@DeleteEventIntegration' {} a -> s {name = a} :: DeleteEventIntegration)

instance Core.AWSRequest DeleteEventIntegration where
  type
    AWSResponse DeleteEventIntegration =
      DeleteEventIntegrationResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteEventIntegrationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteEventIntegration where
  hashWithSalt _salt DeleteEventIntegration' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData DeleteEventIntegration where
  rnf DeleteEventIntegration' {..} = Prelude.rnf name

instance Data.ToHeaders DeleteEventIntegration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteEventIntegration where
  toPath DeleteEventIntegration' {..} =
    Prelude.mconcat
      ["/eventIntegrations/", Data.toBS name]

instance Data.ToQuery DeleteEventIntegration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteEventIntegrationResponse' smart constructor.
data DeleteEventIntegrationResponse = DeleteEventIntegrationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteEventIntegrationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteEventIntegrationResponse_httpStatus' - The response's http status code.
newDeleteEventIntegrationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteEventIntegrationResponse
newDeleteEventIntegrationResponse pHttpStatus_ =
  DeleteEventIntegrationResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteEventIntegrationResponse_httpStatus :: Lens.Lens' DeleteEventIntegrationResponse Prelude.Int
deleteEventIntegrationResponse_httpStatus = Lens.lens (\DeleteEventIntegrationResponse' {httpStatus} -> httpStatus) (\s@DeleteEventIntegrationResponse' {} a -> s {httpStatus = a} :: DeleteEventIntegrationResponse)

instance
  Prelude.NFData
    DeleteEventIntegrationResponse
  where
  rnf DeleteEventIntegrationResponse' {..} =
    Prelude.rnf httpStatus
