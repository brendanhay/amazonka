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
-- Module      : Amazonka.Route53RecoveryControlConfig.DeleteControlPanel
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a control panel.
module Amazonka.Route53RecoveryControlConfig.DeleteControlPanel
  ( -- * Creating a Request
    DeleteControlPanel (..),
    newDeleteControlPanel,

    -- * Request Lenses
    deleteControlPanel_controlPanelArn,

    -- * Destructuring the Response
    DeleteControlPanelResponse (..),
    newDeleteControlPanelResponse,

    -- * Response Lenses
    deleteControlPanelResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53RecoveryControlConfig.Types

-- | /See:/ 'newDeleteControlPanel' smart constructor.
data DeleteControlPanel = DeleteControlPanel'
  { -- | The Amazon Resource Name (ARN) of the control panel.
    controlPanelArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteControlPanel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'controlPanelArn', 'deleteControlPanel_controlPanelArn' - The Amazon Resource Name (ARN) of the control panel.
newDeleteControlPanel ::
  -- | 'controlPanelArn'
  Prelude.Text ->
  DeleteControlPanel
newDeleteControlPanel pControlPanelArn_ =
  DeleteControlPanel'
    { controlPanelArn =
        pControlPanelArn_
    }

-- | The Amazon Resource Name (ARN) of the control panel.
deleteControlPanel_controlPanelArn :: Lens.Lens' DeleteControlPanel Prelude.Text
deleteControlPanel_controlPanelArn = Lens.lens (\DeleteControlPanel' {controlPanelArn} -> controlPanelArn) (\s@DeleteControlPanel' {} a -> s {controlPanelArn = a} :: DeleteControlPanel)

instance Core.AWSRequest DeleteControlPanel where
  type
    AWSResponse DeleteControlPanel =
      DeleteControlPanelResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteControlPanelResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteControlPanel where
  hashWithSalt _salt DeleteControlPanel' {..} =
    _salt `Prelude.hashWithSalt` controlPanelArn

instance Prelude.NFData DeleteControlPanel where
  rnf DeleteControlPanel' {..} =
    Prelude.rnf controlPanelArn

instance Core.ToHeaders DeleteControlPanel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DeleteControlPanel where
  toPath DeleteControlPanel' {..} =
    Prelude.mconcat
      ["/controlpanel/", Core.toBS controlPanelArn]

instance Core.ToQuery DeleteControlPanel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteControlPanelResponse' smart constructor.
data DeleteControlPanelResponse = DeleteControlPanelResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteControlPanelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteControlPanelResponse_httpStatus' - The response's http status code.
newDeleteControlPanelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteControlPanelResponse
newDeleteControlPanelResponse pHttpStatus_ =
  DeleteControlPanelResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteControlPanelResponse_httpStatus :: Lens.Lens' DeleteControlPanelResponse Prelude.Int
deleteControlPanelResponse_httpStatus = Lens.lens (\DeleteControlPanelResponse' {httpStatus} -> httpStatus) (\s@DeleteControlPanelResponse' {} a -> s {httpStatus = a} :: DeleteControlPanelResponse)

instance Prelude.NFData DeleteControlPanelResponse where
  rnf DeleteControlPanelResponse' {..} =
    Prelude.rnf httpStatus
