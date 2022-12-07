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
-- Module      : Amazonka.Route53RecoveryControlConfig.DeleteRoutingControl
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a routing control.
module Amazonka.Route53RecoveryControlConfig.DeleteRoutingControl
  ( -- * Creating a Request
    DeleteRoutingControl (..),
    newDeleteRoutingControl,

    -- * Request Lenses
    deleteRoutingControl_routingControlArn,

    -- * Destructuring the Response
    DeleteRoutingControlResponse (..),
    newDeleteRoutingControlResponse,

    -- * Response Lenses
    deleteRoutingControlResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53RecoveryControlConfig.Types

-- | /See:/ 'newDeleteRoutingControl' smart constructor.
data DeleteRoutingControl = DeleteRoutingControl'
  { -- | The Amazon Resource Name (ARN) of the routing control that you\'re
    -- deleting.
    routingControlArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRoutingControl' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'routingControlArn', 'deleteRoutingControl_routingControlArn' - The Amazon Resource Name (ARN) of the routing control that you\'re
-- deleting.
newDeleteRoutingControl ::
  -- | 'routingControlArn'
  Prelude.Text ->
  DeleteRoutingControl
newDeleteRoutingControl pRoutingControlArn_ =
  DeleteRoutingControl'
    { routingControlArn =
        pRoutingControlArn_
    }

-- | The Amazon Resource Name (ARN) of the routing control that you\'re
-- deleting.
deleteRoutingControl_routingControlArn :: Lens.Lens' DeleteRoutingControl Prelude.Text
deleteRoutingControl_routingControlArn = Lens.lens (\DeleteRoutingControl' {routingControlArn} -> routingControlArn) (\s@DeleteRoutingControl' {} a -> s {routingControlArn = a} :: DeleteRoutingControl)

instance Core.AWSRequest DeleteRoutingControl where
  type
    AWSResponse DeleteRoutingControl =
      DeleteRoutingControlResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteRoutingControlResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteRoutingControl where
  hashWithSalt _salt DeleteRoutingControl' {..} =
    _salt `Prelude.hashWithSalt` routingControlArn

instance Prelude.NFData DeleteRoutingControl where
  rnf DeleteRoutingControl' {..} =
    Prelude.rnf routingControlArn

instance Data.ToHeaders DeleteRoutingControl where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteRoutingControl where
  toPath DeleteRoutingControl' {..} =
    Prelude.mconcat
      ["/routingcontrol/", Data.toBS routingControlArn]

instance Data.ToQuery DeleteRoutingControl where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteRoutingControlResponse' smart constructor.
data DeleteRoutingControlResponse = DeleteRoutingControlResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRoutingControlResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteRoutingControlResponse_httpStatus' - The response's http status code.
newDeleteRoutingControlResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteRoutingControlResponse
newDeleteRoutingControlResponse pHttpStatus_ =
  DeleteRoutingControlResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteRoutingControlResponse_httpStatus :: Lens.Lens' DeleteRoutingControlResponse Prelude.Int
deleteRoutingControlResponse_httpStatus = Lens.lens (\DeleteRoutingControlResponse' {httpStatus} -> httpStatus) (\s@DeleteRoutingControlResponse' {} a -> s {httpStatus = a} :: DeleteRoutingControlResponse)

instance Prelude.NFData DeleteRoutingControlResponse where
  rnf DeleteRoutingControlResponse' {..} =
    Prelude.rnf httpStatus
