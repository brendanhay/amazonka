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
-- Module      : Amazonka.VPCLattice.GetListener
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the specified listener for the specified
-- service.
module Amazonka.VPCLattice.GetListener
  ( -- * Creating a Request
    GetListener (..),
    newGetListener,

    -- * Request Lenses
    getListener_listenerIdentifier,
    getListener_serviceIdentifier,

    -- * Destructuring the Response
    GetListenerResponse (..),
    newGetListenerResponse,

    -- * Response Lenses
    getListenerResponse_arn,
    getListenerResponse_createdAt,
    getListenerResponse_defaultAction,
    getListenerResponse_id,
    getListenerResponse_lastUpdatedAt,
    getListenerResponse_name,
    getListenerResponse_port,
    getListenerResponse_protocol,
    getListenerResponse_serviceArn,
    getListenerResponse_serviceId,
    getListenerResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.VPCLattice.Types

-- | /See:/ 'newGetListener' smart constructor.
data GetListener = GetListener'
  { -- | The ID or Amazon Resource Name (ARN) of the listener.
    listenerIdentifier :: Prelude.Text,
    -- | The ID or Amazon Resource Name (ARN) of the service.
    serviceIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetListener' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'listenerIdentifier', 'getListener_listenerIdentifier' - The ID or Amazon Resource Name (ARN) of the listener.
--
-- 'serviceIdentifier', 'getListener_serviceIdentifier' - The ID or Amazon Resource Name (ARN) of the service.
newGetListener ::
  -- | 'listenerIdentifier'
  Prelude.Text ->
  -- | 'serviceIdentifier'
  Prelude.Text ->
  GetListener
newGetListener
  pListenerIdentifier_
  pServiceIdentifier_ =
    GetListener'
      { listenerIdentifier =
          pListenerIdentifier_,
        serviceIdentifier = pServiceIdentifier_
      }

-- | The ID or Amazon Resource Name (ARN) of the listener.
getListener_listenerIdentifier :: Lens.Lens' GetListener Prelude.Text
getListener_listenerIdentifier = Lens.lens (\GetListener' {listenerIdentifier} -> listenerIdentifier) (\s@GetListener' {} a -> s {listenerIdentifier = a} :: GetListener)

-- | The ID or Amazon Resource Name (ARN) of the service.
getListener_serviceIdentifier :: Lens.Lens' GetListener Prelude.Text
getListener_serviceIdentifier = Lens.lens (\GetListener' {serviceIdentifier} -> serviceIdentifier) (\s@GetListener' {} a -> s {serviceIdentifier = a} :: GetListener)

instance Core.AWSRequest GetListener where
  type AWSResponse GetListener = GetListenerResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetListenerResponse'
            Prelude.<$> (x Data..?> "arn")
            Prelude.<*> (x Data..?> "createdAt")
            Prelude.<*> (x Data..?> "defaultAction")
            Prelude.<*> (x Data..?> "id")
            Prelude.<*> (x Data..?> "lastUpdatedAt")
            Prelude.<*> (x Data..?> "name")
            Prelude.<*> (x Data..?> "port")
            Prelude.<*> (x Data..?> "protocol")
            Prelude.<*> (x Data..?> "serviceArn")
            Prelude.<*> (x Data..?> "serviceId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetListener where
  hashWithSalt _salt GetListener' {..} =
    _salt
      `Prelude.hashWithSalt` listenerIdentifier
      `Prelude.hashWithSalt` serviceIdentifier

instance Prelude.NFData GetListener where
  rnf GetListener' {..} =
    Prelude.rnf listenerIdentifier
      `Prelude.seq` Prelude.rnf serviceIdentifier

instance Data.ToHeaders GetListener where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetListener where
  toPath GetListener' {..} =
    Prelude.mconcat
      [ "/services/",
        Data.toBS serviceIdentifier,
        "/listeners/",
        Data.toBS listenerIdentifier
      ]

instance Data.ToQuery GetListener where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetListenerResponse' smart constructor.
data GetListenerResponse = GetListenerResponse'
  { -- | The Amazon Resource Name (ARN) of the listener.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the listener was created, specified in ISO-8601
    -- format.
    createdAt :: Prelude.Maybe Data.ISO8601,
    -- | The actions for the default listener rule.
    defaultAction :: Prelude.Maybe RuleAction,
    -- | The ID of the listener.
    id :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the listener was last updated, specified in
    -- ISO-8601 format.
    lastUpdatedAt :: Prelude.Maybe Data.ISO8601,
    -- | The name of the listener.
    name :: Prelude.Maybe Prelude.Text,
    -- | The listener port.
    port :: Prelude.Maybe Prelude.Natural,
    -- | The listener protocol.
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
-- Create a value of 'GetListenerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'getListenerResponse_arn' - The Amazon Resource Name (ARN) of the listener.
--
-- 'createdAt', 'getListenerResponse_createdAt' - The date and time that the listener was created, specified in ISO-8601
-- format.
--
-- 'defaultAction', 'getListenerResponse_defaultAction' - The actions for the default listener rule.
--
-- 'id', 'getListenerResponse_id' - The ID of the listener.
--
-- 'lastUpdatedAt', 'getListenerResponse_lastUpdatedAt' - The date and time that the listener was last updated, specified in
-- ISO-8601 format.
--
-- 'name', 'getListenerResponse_name' - The name of the listener.
--
-- 'port', 'getListenerResponse_port' - The listener port.
--
-- 'protocol', 'getListenerResponse_protocol' - The listener protocol.
--
-- 'serviceArn', 'getListenerResponse_serviceArn' - The Amazon Resource Name (ARN) of the service.
--
-- 'serviceId', 'getListenerResponse_serviceId' - The ID of the service.
--
-- 'httpStatus', 'getListenerResponse_httpStatus' - The response's http status code.
newGetListenerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetListenerResponse
newGetListenerResponse pHttpStatus_ =
  GetListenerResponse'
    { arn = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      defaultAction = Prelude.Nothing,
      id = Prelude.Nothing,
      lastUpdatedAt = Prelude.Nothing,
      name = Prelude.Nothing,
      port = Prelude.Nothing,
      protocol = Prelude.Nothing,
      serviceArn = Prelude.Nothing,
      serviceId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the listener.
getListenerResponse_arn :: Lens.Lens' GetListenerResponse (Prelude.Maybe Prelude.Text)
getListenerResponse_arn = Lens.lens (\GetListenerResponse' {arn} -> arn) (\s@GetListenerResponse' {} a -> s {arn = a} :: GetListenerResponse)

-- | The date and time that the listener was created, specified in ISO-8601
-- format.
getListenerResponse_createdAt :: Lens.Lens' GetListenerResponse (Prelude.Maybe Prelude.UTCTime)
getListenerResponse_createdAt = Lens.lens (\GetListenerResponse' {createdAt} -> createdAt) (\s@GetListenerResponse' {} a -> s {createdAt = a} :: GetListenerResponse) Prelude.. Lens.mapping Data._Time

-- | The actions for the default listener rule.
getListenerResponse_defaultAction :: Lens.Lens' GetListenerResponse (Prelude.Maybe RuleAction)
getListenerResponse_defaultAction = Lens.lens (\GetListenerResponse' {defaultAction} -> defaultAction) (\s@GetListenerResponse' {} a -> s {defaultAction = a} :: GetListenerResponse)

-- | The ID of the listener.
getListenerResponse_id :: Lens.Lens' GetListenerResponse (Prelude.Maybe Prelude.Text)
getListenerResponse_id = Lens.lens (\GetListenerResponse' {id} -> id) (\s@GetListenerResponse' {} a -> s {id = a} :: GetListenerResponse)

-- | The date and time that the listener was last updated, specified in
-- ISO-8601 format.
getListenerResponse_lastUpdatedAt :: Lens.Lens' GetListenerResponse (Prelude.Maybe Prelude.UTCTime)
getListenerResponse_lastUpdatedAt = Lens.lens (\GetListenerResponse' {lastUpdatedAt} -> lastUpdatedAt) (\s@GetListenerResponse' {} a -> s {lastUpdatedAt = a} :: GetListenerResponse) Prelude.. Lens.mapping Data._Time

-- | The name of the listener.
getListenerResponse_name :: Lens.Lens' GetListenerResponse (Prelude.Maybe Prelude.Text)
getListenerResponse_name = Lens.lens (\GetListenerResponse' {name} -> name) (\s@GetListenerResponse' {} a -> s {name = a} :: GetListenerResponse)

-- | The listener port.
getListenerResponse_port :: Lens.Lens' GetListenerResponse (Prelude.Maybe Prelude.Natural)
getListenerResponse_port = Lens.lens (\GetListenerResponse' {port} -> port) (\s@GetListenerResponse' {} a -> s {port = a} :: GetListenerResponse)

-- | The listener protocol.
getListenerResponse_protocol :: Lens.Lens' GetListenerResponse (Prelude.Maybe ListenerProtocol)
getListenerResponse_protocol = Lens.lens (\GetListenerResponse' {protocol} -> protocol) (\s@GetListenerResponse' {} a -> s {protocol = a} :: GetListenerResponse)

-- | The Amazon Resource Name (ARN) of the service.
getListenerResponse_serviceArn :: Lens.Lens' GetListenerResponse (Prelude.Maybe Prelude.Text)
getListenerResponse_serviceArn = Lens.lens (\GetListenerResponse' {serviceArn} -> serviceArn) (\s@GetListenerResponse' {} a -> s {serviceArn = a} :: GetListenerResponse)

-- | The ID of the service.
getListenerResponse_serviceId :: Lens.Lens' GetListenerResponse (Prelude.Maybe Prelude.Text)
getListenerResponse_serviceId = Lens.lens (\GetListenerResponse' {serviceId} -> serviceId) (\s@GetListenerResponse' {} a -> s {serviceId = a} :: GetListenerResponse)

-- | The response's http status code.
getListenerResponse_httpStatus :: Lens.Lens' GetListenerResponse Prelude.Int
getListenerResponse_httpStatus = Lens.lens (\GetListenerResponse' {httpStatus} -> httpStatus) (\s@GetListenerResponse' {} a -> s {httpStatus = a} :: GetListenerResponse)

instance Prelude.NFData GetListenerResponse where
  rnf GetListenerResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf defaultAction
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf lastUpdatedAt
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf port
      `Prelude.seq` Prelude.rnf protocol
      `Prelude.seq` Prelude.rnf serviceArn
      `Prelude.seq` Prelude.rnf serviceId
      `Prelude.seq` Prelude.rnf httpStatus
