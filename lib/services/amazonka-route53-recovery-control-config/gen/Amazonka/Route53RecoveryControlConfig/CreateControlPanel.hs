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
-- Module      : Amazonka.Route53RecoveryControlConfig.CreateControlPanel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new control panel. A control panel represents a group of
-- routing controls that can be changed together in a single transaction.
-- You can use a control panel to centrally view the operational status of
-- applications across your organization, and trigger multi-app failovers
-- in a single transaction, for example, to fail over an Availability Zone
-- or Amazon Web Services Region.
module Amazonka.Route53RecoveryControlConfig.CreateControlPanel
  ( -- * Creating a Request
    CreateControlPanel (..),
    newCreateControlPanel,

    -- * Request Lenses
    createControlPanel_clientToken,
    createControlPanel_tags,
    createControlPanel_clusterArn,
    createControlPanel_controlPanelName,

    -- * Destructuring the Response
    CreateControlPanelResponse (..),
    newCreateControlPanelResponse,

    -- * Response Lenses
    createControlPanelResponse_controlPanel,
    createControlPanelResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53RecoveryControlConfig.Types

-- | The details of the control panel that you\'re creating.
--
-- /See:/ 'newCreateControlPanel' smart constructor.
data CreateControlPanel = CreateControlPanel'
  { -- | A unique, case-sensitive string of up to 64 ASCII characters. To make an
    -- idempotent API request with an action, specify a client token in the
    -- request.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The tags associated with the control panel.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The Amazon Resource Name (ARN) of the cluster for the control panel.
    clusterArn :: Prelude.Text,
    -- | The name of the control panel.
    controlPanelName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateControlPanel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createControlPanel_clientToken' - A unique, case-sensitive string of up to 64 ASCII characters. To make an
-- idempotent API request with an action, specify a client token in the
-- request.
--
-- 'tags', 'createControlPanel_tags' - The tags associated with the control panel.
--
-- 'clusterArn', 'createControlPanel_clusterArn' - The Amazon Resource Name (ARN) of the cluster for the control panel.
--
-- 'controlPanelName', 'createControlPanel_controlPanelName' - The name of the control panel.
newCreateControlPanel ::
  -- | 'clusterArn'
  Prelude.Text ->
  -- | 'controlPanelName'
  Prelude.Text ->
  CreateControlPanel
newCreateControlPanel pClusterArn_ pControlPanelName_ =
  CreateControlPanel'
    { clientToken = Prelude.Nothing,
      tags = Prelude.Nothing,
      clusterArn = pClusterArn_,
      controlPanelName = pControlPanelName_
    }

-- | A unique, case-sensitive string of up to 64 ASCII characters. To make an
-- idempotent API request with an action, specify a client token in the
-- request.
createControlPanel_clientToken :: Lens.Lens' CreateControlPanel (Prelude.Maybe Prelude.Text)
createControlPanel_clientToken = Lens.lens (\CreateControlPanel' {clientToken} -> clientToken) (\s@CreateControlPanel' {} a -> s {clientToken = a} :: CreateControlPanel)

-- | The tags associated with the control panel.
createControlPanel_tags :: Lens.Lens' CreateControlPanel (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createControlPanel_tags = Lens.lens (\CreateControlPanel' {tags} -> tags) (\s@CreateControlPanel' {} a -> s {tags = a} :: CreateControlPanel) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the cluster for the control panel.
createControlPanel_clusterArn :: Lens.Lens' CreateControlPanel Prelude.Text
createControlPanel_clusterArn = Lens.lens (\CreateControlPanel' {clusterArn} -> clusterArn) (\s@CreateControlPanel' {} a -> s {clusterArn = a} :: CreateControlPanel)

-- | The name of the control panel.
createControlPanel_controlPanelName :: Lens.Lens' CreateControlPanel Prelude.Text
createControlPanel_controlPanelName = Lens.lens (\CreateControlPanel' {controlPanelName} -> controlPanelName) (\s@CreateControlPanel' {} a -> s {controlPanelName = a} :: CreateControlPanel)

instance Core.AWSRequest CreateControlPanel where
  type
    AWSResponse CreateControlPanel =
      CreateControlPanelResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateControlPanelResponse'
            Prelude.<$> (x Data..?> "ControlPanel")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateControlPanel where
  hashWithSalt _salt CreateControlPanel' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` clusterArn
      `Prelude.hashWithSalt` controlPanelName

instance Prelude.NFData CreateControlPanel where
  rnf CreateControlPanel' {..} =
    Prelude.rnf clientToken `Prelude.seq`
      Prelude.rnf tags `Prelude.seq`
        Prelude.rnf clusterArn `Prelude.seq`
          Prelude.rnf controlPanelName

instance Data.ToHeaders CreateControlPanel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateControlPanel where
  toJSON CreateControlPanel' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientToken" Data..=) Prelude.<$> clientToken,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("ClusterArn" Data..= clusterArn),
            Prelude.Just
              ("ControlPanelName" Data..= controlPanelName)
          ]
      )

instance Data.ToPath CreateControlPanel where
  toPath = Prelude.const "/controlpanel"

instance Data.ToQuery CreateControlPanel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateControlPanelResponse' smart constructor.
data CreateControlPanelResponse = CreateControlPanelResponse'
  { -- | Information about a control panel.
    controlPanel :: Prelude.Maybe ControlPanel,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateControlPanelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'controlPanel', 'createControlPanelResponse_controlPanel' - Information about a control panel.
--
-- 'httpStatus', 'createControlPanelResponse_httpStatus' - The response's http status code.
newCreateControlPanelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateControlPanelResponse
newCreateControlPanelResponse pHttpStatus_ =
  CreateControlPanelResponse'
    { controlPanel =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about a control panel.
createControlPanelResponse_controlPanel :: Lens.Lens' CreateControlPanelResponse (Prelude.Maybe ControlPanel)
createControlPanelResponse_controlPanel = Lens.lens (\CreateControlPanelResponse' {controlPanel} -> controlPanel) (\s@CreateControlPanelResponse' {} a -> s {controlPanel = a} :: CreateControlPanelResponse)

-- | The response's http status code.
createControlPanelResponse_httpStatus :: Lens.Lens' CreateControlPanelResponse Prelude.Int
createControlPanelResponse_httpStatus = Lens.lens (\CreateControlPanelResponse' {httpStatus} -> httpStatus) (\s@CreateControlPanelResponse' {} a -> s {httpStatus = a} :: CreateControlPanelResponse)

instance Prelude.NFData CreateControlPanelResponse where
  rnf CreateControlPanelResponse' {..} =
    Prelude.rnf controlPanel `Prelude.seq`
      Prelude.rnf httpStatus
