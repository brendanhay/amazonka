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
-- Module      : Amazonka.VPCLattice.CreateListener
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a listener for a service. Before you start using your Amazon VPC
-- Lattice service, you must add one or more listeners. A listener is a
-- process that checks for connection requests to your services. For more
-- information, see
-- <https://docs.aws.amazon.com/vpc-lattice/latest/ug/listeners.html Listeners>
-- in the /Amazon VPC Lattice User Guide/.
module Amazonka.VPCLattice.CreateListener
  ( -- * Creating a Request
    CreateListener (..),
    newCreateListener,

    -- * Request Lenses
    createListener_clientToken,
    createListener_port,
    createListener_tags,
    createListener_defaultAction,
    createListener_name,
    createListener_protocol,
    createListener_serviceIdentifier,

    -- * Destructuring the Response
    CreateListenerResponse (..),
    newCreateListenerResponse,

    -- * Response Lenses
    createListenerResponse_arn,
    createListenerResponse_defaultAction,
    createListenerResponse_id,
    createListenerResponse_name,
    createListenerResponse_port,
    createListenerResponse_protocol,
    createListenerResponse_serviceArn,
    createListenerResponse_serviceId,
    createListenerResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.VPCLattice.Types

-- | /See:/ 'newCreateListener' smart constructor.
data CreateListener = CreateListener'
  { -- | A unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. If you retry a request that completed
    -- successfully using the same client token and parameters, the retry
    -- succeeds without performing any actions. If the parameters aren\'t
    -- identical, the retry fails.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The listener port. You can specify a value from @1@ to @65535@. For
    -- HTTP, the default is @80@. For HTTPS, the default is @443@.
    port :: Prelude.Maybe Prelude.Natural,
    -- | The tags for the listener.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The action for the default rule. Each listener has a default rule. Each
    -- rule consists of a priority, one or more actions, and one or more
    -- conditions. The default rule is the rule that\'s used if no other rules
    -- match. Each rule must include exactly one of the following types of
    -- actions: @forward @or @fixed-response@, and it must be the last action
    -- to be performed.
    defaultAction :: RuleAction,
    -- | The name of the listener. A listener name must be unique within a
    -- service. The valid characters are a-z, 0-9, and hyphens (-). You can\'t
    -- use a hyphen as the first or last character, or immediately after
    -- another hyphen.
    name :: Prelude.Text,
    -- | The listener protocol HTTP or HTTPS.
    protocol :: ListenerProtocol,
    -- | The ID or Amazon Resource Name (ARN) of the service.
    serviceIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateListener' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createListener_clientToken' - A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If you retry a request that completed
-- successfully using the same client token and parameters, the retry
-- succeeds without performing any actions. If the parameters aren\'t
-- identical, the retry fails.
--
-- 'port', 'createListener_port' - The listener port. You can specify a value from @1@ to @65535@. For
-- HTTP, the default is @80@. For HTTPS, the default is @443@.
--
-- 'tags', 'createListener_tags' - The tags for the listener.
--
-- 'defaultAction', 'createListener_defaultAction' - The action for the default rule. Each listener has a default rule. Each
-- rule consists of a priority, one or more actions, and one or more
-- conditions. The default rule is the rule that\'s used if no other rules
-- match. Each rule must include exactly one of the following types of
-- actions: @forward @or @fixed-response@, and it must be the last action
-- to be performed.
--
-- 'name', 'createListener_name' - The name of the listener. A listener name must be unique within a
-- service. The valid characters are a-z, 0-9, and hyphens (-). You can\'t
-- use a hyphen as the first or last character, or immediately after
-- another hyphen.
--
-- 'protocol', 'createListener_protocol' - The listener protocol HTTP or HTTPS.
--
-- 'serviceIdentifier', 'createListener_serviceIdentifier' - The ID or Amazon Resource Name (ARN) of the service.
newCreateListener ::
  -- | 'defaultAction'
  RuleAction ->
  -- | 'name'
  Prelude.Text ->
  -- | 'protocol'
  ListenerProtocol ->
  -- | 'serviceIdentifier'
  Prelude.Text ->
  CreateListener
newCreateListener
  pDefaultAction_
  pName_
  pProtocol_
  pServiceIdentifier_ =
    CreateListener'
      { clientToken = Prelude.Nothing,
        port = Prelude.Nothing,
        tags = Prelude.Nothing,
        defaultAction = pDefaultAction_,
        name = pName_,
        protocol = pProtocol_,
        serviceIdentifier = pServiceIdentifier_
      }

-- | A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If you retry a request that completed
-- successfully using the same client token and parameters, the retry
-- succeeds without performing any actions. If the parameters aren\'t
-- identical, the retry fails.
createListener_clientToken :: Lens.Lens' CreateListener (Prelude.Maybe Prelude.Text)
createListener_clientToken = Lens.lens (\CreateListener' {clientToken} -> clientToken) (\s@CreateListener' {} a -> s {clientToken = a} :: CreateListener)

-- | The listener port. You can specify a value from @1@ to @65535@. For
-- HTTP, the default is @80@. For HTTPS, the default is @443@.
createListener_port :: Lens.Lens' CreateListener (Prelude.Maybe Prelude.Natural)
createListener_port = Lens.lens (\CreateListener' {port} -> port) (\s@CreateListener' {} a -> s {port = a} :: CreateListener)

-- | The tags for the listener.
createListener_tags :: Lens.Lens' CreateListener (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createListener_tags = Lens.lens (\CreateListener' {tags} -> tags) (\s@CreateListener' {} a -> s {tags = a} :: CreateListener) Prelude.. Lens.mapping Lens.coerced

-- | The action for the default rule. Each listener has a default rule. Each
-- rule consists of a priority, one or more actions, and one or more
-- conditions. The default rule is the rule that\'s used if no other rules
-- match. Each rule must include exactly one of the following types of
-- actions: @forward @or @fixed-response@, and it must be the last action
-- to be performed.
createListener_defaultAction :: Lens.Lens' CreateListener RuleAction
createListener_defaultAction = Lens.lens (\CreateListener' {defaultAction} -> defaultAction) (\s@CreateListener' {} a -> s {defaultAction = a} :: CreateListener)

-- | The name of the listener. A listener name must be unique within a
-- service. The valid characters are a-z, 0-9, and hyphens (-). You can\'t
-- use a hyphen as the first or last character, or immediately after
-- another hyphen.
createListener_name :: Lens.Lens' CreateListener Prelude.Text
createListener_name = Lens.lens (\CreateListener' {name} -> name) (\s@CreateListener' {} a -> s {name = a} :: CreateListener)

-- | The listener protocol HTTP or HTTPS.
createListener_protocol :: Lens.Lens' CreateListener ListenerProtocol
createListener_protocol = Lens.lens (\CreateListener' {protocol} -> protocol) (\s@CreateListener' {} a -> s {protocol = a} :: CreateListener)

-- | The ID or Amazon Resource Name (ARN) of the service.
createListener_serviceIdentifier :: Lens.Lens' CreateListener Prelude.Text
createListener_serviceIdentifier = Lens.lens (\CreateListener' {serviceIdentifier} -> serviceIdentifier) (\s@CreateListener' {} a -> s {serviceIdentifier = a} :: CreateListener)

instance Core.AWSRequest CreateListener where
  type
    AWSResponse CreateListener =
      CreateListenerResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateListenerResponse'
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

instance Prelude.Hashable CreateListener where
  hashWithSalt _salt CreateListener' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` defaultAction
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` protocol
      `Prelude.hashWithSalt` serviceIdentifier

instance Prelude.NFData CreateListener where
  rnf CreateListener' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf port
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf defaultAction
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf protocol
      `Prelude.seq` Prelude.rnf serviceIdentifier

instance Data.ToHeaders CreateListener where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateListener where
  toJSON CreateListener' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            ("port" Data..=) Prelude.<$> port,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("defaultAction" Data..= defaultAction),
            Prelude.Just ("name" Data..= name),
            Prelude.Just ("protocol" Data..= protocol)
          ]
      )

instance Data.ToPath CreateListener where
  toPath CreateListener' {..} =
    Prelude.mconcat
      [ "/services/",
        Data.toBS serviceIdentifier,
        "/listeners"
      ]

instance Data.ToQuery CreateListener where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateListenerResponse' smart constructor.
data CreateListenerResponse = CreateListenerResponse'
  { -- | The Amazon Resource Name (ARN) of the listener.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The action for the default rule.
    defaultAction :: Prelude.Maybe RuleAction,
    -- | The ID of the listener.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name of the listener.
    name :: Prelude.Maybe Prelude.Text,
    -- | The port number of the listener.
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
-- Create a value of 'CreateListenerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'createListenerResponse_arn' - The Amazon Resource Name (ARN) of the listener.
--
-- 'defaultAction', 'createListenerResponse_defaultAction' - The action for the default rule.
--
-- 'id', 'createListenerResponse_id' - The ID of the listener.
--
-- 'name', 'createListenerResponse_name' - The name of the listener.
--
-- 'port', 'createListenerResponse_port' - The port number of the listener.
--
-- 'protocol', 'createListenerResponse_protocol' - The protocol of the listener.
--
-- 'serviceArn', 'createListenerResponse_serviceArn' - The Amazon Resource Name (ARN) of the service.
--
-- 'serviceId', 'createListenerResponse_serviceId' - The ID of the service.
--
-- 'httpStatus', 'createListenerResponse_httpStatus' - The response's http status code.
newCreateListenerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateListenerResponse
newCreateListenerResponse pHttpStatus_ =
  CreateListenerResponse'
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
createListenerResponse_arn :: Lens.Lens' CreateListenerResponse (Prelude.Maybe Prelude.Text)
createListenerResponse_arn = Lens.lens (\CreateListenerResponse' {arn} -> arn) (\s@CreateListenerResponse' {} a -> s {arn = a} :: CreateListenerResponse)

-- | The action for the default rule.
createListenerResponse_defaultAction :: Lens.Lens' CreateListenerResponse (Prelude.Maybe RuleAction)
createListenerResponse_defaultAction = Lens.lens (\CreateListenerResponse' {defaultAction} -> defaultAction) (\s@CreateListenerResponse' {} a -> s {defaultAction = a} :: CreateListenerResponse)

-- | The ID of the listener.
createListenerResponse_id :: Lens.Lens' CreateListenerResponse (Prelude.Maybe Prelude.Text)
createListenerResponse_id = Lens.lens (\CreateListenerResponse' {id} -> id) (\s@CreateListenerResponse' {} a -> s {id = a} :: CreateListenerResponse)

-- | The name of the listener.
createListenerResponse_name :: Lens.Lens' CreateListenerResponse (Prelude.Maybe Prelude.Text)
createListenerResponse_name = Lens.lens (\CreateListenerResponse' {name} -> name) (\s@CreateListenerResponse' {} a -> s {name = a} :: CreateListenerResponse)

-- | The port number of the listener.
createListenerResponse_port :: Lens.Lens' CreateListenerResponse (Prelude.Maybe Prelude.Natural)
createListenerResponse_port = Lens.lens (\CreateListenerResponse' {port} -> port) (\s@CreateListenerResponse' {} a -> s {port = a} :: CreateListenerResponse)

-- | The protocol of the listener.
createListenerResponse_protocol :: Lens.Lens' CreateListenerResponse (Prelude.Maybe ListenerProtocol)
createListenerResponse_protocol = Lens.lens (\CreateListenerResponse' {protocol} -> protocol) (\s@CreateListenerResponse' {} a -> s {protocol = a} :: CreateListenerResponse)

-- | The Amazon Resource Name (ARN) of the service.
createListenerResponse_serviceArn :: Lens.Lens' CreateListenerResponse (Prelude.Maybe Prelude.Text)
createListenerResponse_serviceArn = Lens.lens (\CreateListenerResponse' {serviceArn} -> serviceArn) (\s@CreateListenerResponse' {} a -> s {serviceArn = a} :: CreateListenerResponse)

-- | The ID of the service.
createListenerResponse_serviceId :: Lens.Lens' CreateListenerResponse (Prelude.Maybe Prelude.Text)
createListenerResponse_serviceId = Lens.lens (\CreateListenerResponse' {serviceId} -> serviceId) (\s@CreateListenerResponse' {} a -> s {serviceId = a} :: CreateListenerResponse)

-- | The response's http status code.
createListenerResponse_httpStatus :: Lens.Lens' CreateListenerResponse Prelude.Int
createListenerResponse_httpStatus = Lens.lens (\CreateListenerResponse' {httpStatus} -> httpStatus) (\s@CreateListenerResponse' {} a -> s {httpStatus = a} :: CreateListenerResponse)

instance Prelude.NFData CreateListenerResponse where
  rnf CreateListenerResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf defaultAction
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf port
      `Prelude.seq` Prelude.rnf protocol
      `Prelude.seq` Prelude.rnf serviceArn
      `Prelude.seq` Prelude.rnf serviceId
      `Prelude.seq` Prelude.rnf httpStatus
