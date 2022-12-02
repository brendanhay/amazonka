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
-- Module      : Amazonka.Route53RecoveryControlConfig.DescribeControlPanel
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Displays details about a control panel.
module Amazonka.Route53RecoveryControlConfig.DescribeControlPanel
  ( -- * Creating a Request
    DescribeControlPanel (..),
    newDescribeControlPanel,

    -- * Request Lenses
    describeControlPanel_controlPanelArn,

    -- * Destructuring the Response
    DescribeControlPanelResponse (..),
    newDescribeControlPanelResponse,

    -- * Response Lenses
    describeControlPanelResponse_controlPanel,
    describeControlPanelResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53RecoveryControlConfig.Types

-- | /See:/ 'newDescribeControlPanel' smart constructor.
data DescribeControlPanel = DescribeControlPanel'
  { -- | The Amazon Resource Name (ARN) of the control panel.
    controlPanelArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeControlPanel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'controlPanelArn', 'describeControlPanel_controlPanelArn' - The Amazon Resource Name (ARN) of the control panel.
newDescribeControlPanel ::
  -- | 'controlPanelArn'
  Prelude.Text ->
  DescribeControlPanel
newDescribeControlPanel pControlPanelArn_ =
  DescribeControlPanel'
    { controlPanelArn =
        pControlPanelArn_
    }

-- | The Amazon Resource Name (ARN) of the control panel.
describeControlPanel_controlPanelArn :: Lens.Lens' DescribeControlPanel Prelude.Text
describeControlPanel_controlPanelArn = Lens.lens (\DescribeControlPanel' {controlPanelArn} -> controlPanelArn) (\s@DescribeControlPanel' {} a -> s {controlPanelArn = a} :: DescribeControlPanel)

instance Core.AWSRequest DescribeControlPanel where
  type
    AWSResponse DescribeControlPanel =
      DescribeControlPanelResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeControlPanelResponse'
            Prelude.<$> (x Data..?> "ControlPanel")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeControlPanel where
  hashWithSalt _salt DescribeControlPanel' {..} =
    _salt `Prelude.hashWithSalt` controlPanelArn

instance Prelude.NFData DescribeControlPanel where
  rnf DescribeControlPanel' {..} =
    Prelude.rnf controlPanelArn

instance Data.ToHeaders DescribeControlPanel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeControlPanel where
  toPath DescribeControlPanel' {..} =
    Prelude.mconcat
      ["/controlpanel/", Data.toBS controlPanelArn]

instance Data.ToQuery DescribeControlPanel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeControlPanelResponse' smart constructor.
data DescribeControlPanelResponse = DescribeControlPanelResponse'
  { -- | Information about the control panel.
    controlPanel :: Prelude.Maybe ControlPanel,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeControlPanelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'controlPanel', 'describeControlPanelResponse_controlPanel' - Information about the control panel.
--
-- 'httpStatus', 'describeControlPanelResponse_httpStatus' - The response's http status code.
newDescribeControlPanelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeControlPanelResponse
newDescribeControlPanelResponse pHttpStatus_ =
  DescribeControlPanelResponse'
    { controlPanel =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the control panel.
describeControlPanelResponse_controlPanel :: Lens.Lens' DescribeControlPanelResponse (Prelude.Maybe ControlPanel)
describeControlPanelResponse_controlPanel = Lens.lens (\DescribeControlPanelResponse' {controlPanel} -> controlPanel) (\s@DescribeControlPanelResponse' {} a -> s {controlPanel = a} :: DescribeControlPanelResponse)

-- | The response's http status code.
describeControlPanelResponse_httpStatus :: Lens.Lens' DescribeControlPanelResponse Prelude.Int
describeControlPanelResponse_httpStatus = Lens.lens (\DescribeControlPanelResponse' {httpStatus} -> httpStatus) (\s@DescribeControlPanelResponse' {} a -> s {httpStatus = a} :: DescribeControlPanelResponse)

instance Prelude.NFData DescribeControlPanelResponse where
  rnf DescribeControlPanelResponse' {..} =
    Prelude.rnf controlPanel
      `Prelude.seq` Prelude.rnf httpStatus
