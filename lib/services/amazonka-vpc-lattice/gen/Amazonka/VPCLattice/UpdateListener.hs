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
-- Module      : Amazonka.VPCLattice.UpdateListener
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified listener for the specified service.
module Amazonka.VPCLattice.UpdateListener
  ( -- * Creating a Request
    UpdateListener (..),
    newUpdateListener,

    -- * Request Lenses
    updateListener_defaultAction,
    updateListener_listenerIdentifier,
    updateListener_serviceIdentifier,

    -- * Destructuring the Response
    UpdateListenerResponse (..),
    newUpdateListenerResponse,

    -- * Response Lenses
    updateListenerResponse_arn,
    updateListenerResponse_defaultAction,
    updateListenerResponse_id,
    updateListenerResponse_name,
    updateListenerResponse_port,
    updateListenerResponse_protocol,
    updateListenerResponse_serviceArn,
    updateListenerResponse_serviceId,
    updateListenerResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.VPCLattice.Types

-- | /See:/ 'newUpdateListener' smart constructor.
data UpdateListener = UpdateListener'
  { -- | The action for the default rule.
    defaultAction :: RuleAction,
    -- | The ID or Amazon Resource Name (ARN) of the listener.
    listenerIdentifier :: Prelude.Text,
    -- | The ID or Amazon Resource Name (ARN) of the service.
    serviceIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateListener' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defaultAction', 'updateListener_defaultAction' - The action for the default rule.
--
-- 'listenerIdentifier', 'updateListener_listenerIdentifier' - The ID or Amazon Resource Name (ARN) of the listener.
--
-- 'serviceIdentifier', 'updateListener_serviceIdentifier' - The ID or Amazon Resource Name (ARN) of the service.
newUpdateListener ::
  -- | 'defaultAction'
  RuleAction ->
  -- | 'listenerIdentifier'
  Prelude.Text ->
  -- | 'serviceIdentifier'
  Prelude.Text ->
  UpdateListener
newUpdateListener
  pDefaultAction_
  pListenerIdentifier_
  pServiceIdentifier_ =
    UpdateListener'
      { defaultAction = pDefaultAction_,
        listenerIdentifier = pListenerIdentifier_,
        serviceIdentifier = pServiceIdentifier_
      }

-- | The action for the default rule.
updateListener_defaultAction :: Lens.Lens' UpdateListener RuleAction
updateListener_defaultAction = Lens.lens (\UpdateListener' {defaultAction} -> defaultAction) (\s@UpdateListener' {} a -> s {defaultAction = a} :: UpdateListener)

-- | The ID or Amazon Resource Name (ARN) of the listener.
updateListener_listenerIdentifier :: Lens.Lens' UpdateListener Prelude.Text
updateListener_listenerIdentifier = Lens.lens (\UpdateListener' {listenerIdentifier} -> listenerIdentifier) (\s@UpdateListener' {} a -> s {listenerIdentifier = a} :: UpdateListener)

-- | The ID or Amazon Resource Name (ARN) of the service.
updateListener_serviceIdentifier :: Lens.Lens' UpdateListener Prelude.Text
updateListener_serviceIdentifier = Lens.lens (\UpdateListener' {serviceIdentifier} -> serviceIdentifier) (\s@UpdateListener' {} a -> s {serviceIdentifier = a} :: UpdateListener)

instance Core.AWSRequest UpdateListener where
  type
    AWSResponse UpdateListener =
      UpdateListenerResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateListenerResponse'
            Prelude.<$> (x Data..?> "arn")
            Prelude.<*> (x Data..?> "defaultAction")
            Prelude.<*> (x Data..?> "id")
            Prelude.<*> (x Data..?> "name")
            Prelude.<*> (x Data..?> "port")
            Prelude.<*> (x Data..?> "protocol")
            Prelude.<*> (x Data..?> "serviceArn")
            Prelude.<*> (x Data..?> "serviceId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateListener where
  hashWithSalt _salt UpdateListener' {..} =
    _salt
      `Prelude.hashWithSalt` defaultAction
      `Prelude.hashWithSalt` listenerIdentifier
      `Prelude.hashWithSalt` serviceIdentifier

instance Prelude.NFData UpdateListener where
  rnf UpdateListener' {..} =
    Prelude.rnf defaultAction
      `Prelude.seq` Prelude.rnf listenerIdentifier
      `Prelude.seq` Prelude.rnf serviceIdentifier

instance Data.ToHeaders UpdateListener where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateListener where
  toJSON UpdateListener' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("defaultAction" Data..= defaultAction)
          ]
      )

instance Data.ToPath UpdateListener where
  toPath UpdateListener' {..} =
    Prelude.mconcat
      [ "/services/",
        Data.toBS serviceIdentifier,
        "/listeners/",
        Data.toBS listenerIdentifier
      ]

instance Data.ToQuery UpdateListener where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateListenerResponse' smart constructor.
data UpdateListenerResponse = UpdateListenerResponse'
  { -- | The Amazon Resource Name (ARN) of the listener.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The action for the default rule.
    defaultAction :: Prelude.Maybe RuleAction,
    -- | The ID of the listener.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name of the listener.
    name :: Prelude.Maybe Prelude.Text,
    -- | The listener port.
    port :: Prelude.Maybe Prelude.Natural,
    -- | The protocol of the listener.
    protocol :: Prelude.Maybe ListenerProtocol,
    -- | The Amazon Resource Name (ARN) of the service.
    serviceArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the service.
    serviceId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateListenerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'updateListenerResponse_arn' - The Amazon Resource Name (ARN) of the listener.
--
-- 'defaultAction', 'updateListenerResponse_defaultAction' - The action for the default rule.
--
-- 'id', 'updateListenerResponse_id' - The ID of the listener.
--
-- 'name', 'updateListenerResponse_name' - The name of the listener.
--
-- 'port', 'updateListenerResponse_port' - The listener port.
--
-- 'protocol', 'updateListenerResponse_protocol' - The protocol of the listener.
--
-- 'serviceArn', 'updateListenerResponse_serviceArn' - The Amazon Resource Name (ARN) of the service.
--
-- 'serviceId', 'updateListenerResponse_serviceId' - The ID of the service.
--
-- 'httpStatus', 'updateListenerResponse_httpStatus' - The response's http status code.
newUpdateListenerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateListenerResponse
newUpdateListenerResponse pHttpStatus_ =
  UpdateListenerResponse'
    { arn = Prelude.Nothing,
      defaultAction = Prelude.Nothing,
      id = Prelude.Nothing,
      name = Prelude.Nothing,
      port = Prelude.Nothing,
      protocol = Prelude.Nothing,
      serviceArn = Prelude.Nothing,
      serviceId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the listener.
updateListenerResponse_arn :: Lens.Lens' UpdateListenerResponse (Prelude.Maybe Prelude.Text)
updateListenerResponse_arn = Lens.lens (\UpdateListenerResponse' {arn} -> arn) (\s@UpdateListenerResponse' {} a -> s {arn = a} :: UpdateListenerResponse)

-- | The action for the default rule.
updateListenerResponse_defaultAction :: Lens.Lens' UpdateListenerResponse (Prelude.Maybe RuleAction)
updateListenerResponse_defaultAction = Lens.lens (\UpdateListenerResponse' {defaultAction} -> defaultAction) (\s@UpdateListenerResponse' {} a -> s {defaultAction = a} :: UpdateListenerResponse)

-- | The ID of the listener.
updateListenerResponse_id :: Lens.Lens' UpdateListenerResponse (Prelude.Maybe Prelude.Text)
updateListenerResponse_id = Lens.lens (\UpdateListenerResponse' {id} -> id) (\s@UpdateListenerResponse' {} a -> s {id = a} :: UpdateListenerResponse)

-- | The name of the listener.
updateListenerResponse_name :: Lens.Lens' UpdateListenerResponse (Prelude.Maybe Prelude.Text)
updateListenerResponse_name = Lens.lens (\UpdateListenerResponse' {name} -> name) (\s@UpdateListenerResponse' {} a -> s {name = a} :: UpdateListenerResponse)

-- | The listener port.
updateListenerResponse_port :: Lens.Lens' UpdateListenerResponse (Prelude.Maybe Prelude.Natural)
updateListenerResponse_port = Lens.lens (\UpdateListenerResponse' {port} -> port) (\s@UpdateListenerResponse' {} a -> s {port = a} :: UpdateListenerResponse)

-- | The protocol of the listener.
updateListenerResponse_protocol :: Lens.Lens' UpdateListenerResponse (Prelude.Maybe ListenerProtocol)
updateListenerResponse_protocol = Lens.lens (\UpdateListenerResponse' {protocol} -> protocol) (\s@UpdateListenerResponse' {} a -> s {protocol = a} :: UpdateListenerResponse)

-- | The Amazon Resource Name (ARN) of the service.
updateListenerResponse_serviceArn :: Lens.Lens' UpdateListenerResponse (Prelude.Maybe Prelude.Text)
updateListenerResponse_serviceArn = Lens.lens (\UpdateListenerResponse' {serviceArn} -> serviceArn) (\s@UpdateListenerResponse' {} a -> s {serviceArn = a} :: UpdateListenerResponse)

-- | The ID of the service.
updateListenerResponse_serviceId :: Lens.Lens' UpdateListenerResponse (Prelude.Maybe Prelude.Text)
updateListenerResponse_serviceId = Lens.lens (\UpdateListenerResponse' {serviceId} -> serviceId) (\s@UpdateListenerResponse' {} a -> s {serviceId = a} :: UpdateListenerResponse)

-- | The response's http status code.
updateListenerResponse_httpStatus :: Lens.Lens' UpdateListenerResponse Prelude.Int
updateListenerResponse_httpStatus = Lens.lens (\UpdateListenerResponse' {httpStatus} -> httpStatus) (\s@UpdateListenerResponse' {} a -> s {httpStatus = a} :: UpdateListenerResponse)

instance Prelude.NFData UpdateListenerResponse where
  rnf UpdateListenerResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf defaultAction
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf port
      `Prelude.seq` Prelude.rnf protocol
      `Prelude.seq` Prelude.rnf serviceArn
      `Prelude.seq` Prelude.rnf serviceId
      `Prelude.seq` Prelude.rnf httpStatus
