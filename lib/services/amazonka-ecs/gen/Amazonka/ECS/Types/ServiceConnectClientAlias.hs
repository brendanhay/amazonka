{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ECS.Types.ServiceConnectClientAlias
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECS.Types.ServiceConnectClientAlias where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Each alias (\"endpoint\") is a fully-qualified name and port number that
-- other tasks (\"clients\") can use to connect to this service.
--
-- Each name and port mapping must be unique within the namespace.
--
-- Tasks that run in a namespace can use short names to connect to services
-- in the namespace. Tasks can connect to services across all of the
-- clusters in the namespace. Tasks connect through a managed proxy
-- container that collects logs and metrics for increased visibility. Only
-- the tasks that Amazon ECS services create are supported with Service
-- Connect. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-connect.html Service Connect>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- /See:/ 'newServiceConnectClientAlias' smart constructor.
data ServiceConnectClientAlias = ServiceConnectClientAlias'
  { -- | The @dnsName@ is the name that you use in the applications of client
    -- tasks to connect to this service. The name must be a valid DNS name but
    -- doesn\'t need to be fully-qualified. The name can include up to 127
    -- characters. The name can include lowercase letters, numbers, underscores
    -- (_), hyphens (-), and periods (.). The name can\'t start with a hyphen.
    --
    -- If this parameter isn\'t specified, the default value of
    -- @discoveryName.namespace@ is used. If the @discoveryName@ isn\'t
    -- specified, the port mapping name from the task definition is used in
    -- @portName.namespace@.
    --
    -- To avoid changing your applications in client Amazon ECS services, set
    -- this to the same name that the client application uses by default. For
    -- example, a few common names are @database@, @db@, or the lowercase name
    -- of a database, such as @mysql@ or @redis@. For more information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-connect.html Service Connect>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    dnsName :: Prelude.Maybe Prelude.Text,
    -- | The listening port number for the Service Connect proxy. This port is
    -- available inside of all of the tasks within the same namespace.
    --
    -- To avoid changing your applications in client Amazon ECS services, set
    -- this to the same port that the client application uses by default. For
    -- more information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-connect.html Service Connect>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    port :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServiceConnectClientAlias' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dnsName', 'serviceConnectClientAlias_dnsName' - The @dnsName@ is the name that you use in the applications of client
-- tasks to connect to this service. The name must be a valid DNS name but
-- doesn\'t need to be fully-qualified. The name can include up to 127
-- characters. The name can include lowercase letters, numbers, underscores
-- (_), hyphens (-), and periods (.). The name can\'t start with a hyphen.
--
-- If this parameter isn\'t specified, the default value of
-- @discoveryName.namespace@ is used. If the @discoveryName@ isn\'t
-- specified, the port mapping name from the task definition is used in
-- @portName.namespace@.
--
-- To avoid changing your applications in client Amazon ECS services, set
-- this to the same name that the client application uses by default. For
-- example, a few common names are @database@, @db@, or the lowercase name
-- of a database, such as @mysql@ or @redis@. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-connect.html Service Connect>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- 'port', 'serviceConnectClientAlias_port' - The listening port number for the Service Connect proxy. This port is
-- available inside of all of the tasks within the same namespace.
--
-- To avoid changing your applications in client Amazon ECS services, set
-- this to the same port that the client application uses by default. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-connect.html Service Connect>
-- in the /Amazon Elastic Container Service Developer Guide/.
newServiceConnectClientAlias ::
  -- | 'port'
  Prelude.Natural ->
  ServiceConnectClientAlias
newServiceConnectClientAlias pPort_ =
  ServiceConnectClientAlias'
    { dnsName =
        Prelude.Nothing,
      port = pPort_
    }

-- | The @dnsName@ is the name that you use in the applications of client
-- tasks to connect to this service. The name must be a valid DNS name but
-- doesn\'t need to be fully-qualified. The name can include up to 127
-- characters. The name can include lowercase letters, numbers, underscores
-- (_), hyphens (-), and periods (.). The name can\'t start with a hyphen.
--
-- If this parameter isn\'t specified, the default value of
-- @discoveryName.namespace@ is used. If the @discoveryName@ isn\'t
-- specified, the port mapping name from the task definition is used in
-- @portName.namespace@.
--
-- To avoid changing your applications in client Amazon ECS services, set
-- this to the same name that the client application uses by default. For
-- example, a few common names are @database@, @db@, or the lowercase name
-- of a database, such as @mysql@ or @redis@. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-connect.html Service Connect>
-- in the /Amazon Elastic Container Service Developer Guide/.
serviceConnectClientAlias_dnsName :: Lens.Lens' ServiceConnectClientAlias (Prelude.Maybe Prelude.Text)
serviceConnectClientAlias_dnsName = Lens.lens (\ServiceConnectClientAlias' {dnsName} -> dnsName) (\s@ServiceConnectClientAlias' {} a -> s {dnsName = a} :: ServiceConnectClientAlias)

-- | The listening port number for the Service Connect proxy. This port is
-- available inside of all of the tasks within the same namespace.
--
-- To avoid changing your applications in client Amazon ECS services, set
-- this to the same port that the client application uses by default. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-connect.html Service Connect>
-- in the /Amazon Elastic Container Service Developer Guide/.
serviceConnectClientAlias_port :: Lens.Lens' ServiceConnectClientAlias Prelude.Natural
serviceConnectClientAlias_port = Lens.lens (\ServiceConnectClientAlias' {port} -> port) (\s@ServiceConnectClientAlias' {} a -> s {port = a} :: ServiceConnectClientAlias)

instance Data.FromJSON ServiceConnectClientAlias where
  parseJSON =
    Data.withObject
      "ServiceConnectClientAlias"
      ( \x ->
          ServiceConnectClientAlias'
            Prelude.<$> (x Data..:? "dnsName")
            Prelude.<*> (x Data..: "port")
      )

instance Prelude.Hashable ServiceConnectClientAlias where
  hashWithSalt _salt ServiceConnectClientAlias' {..} =
    _salt `Prelude.hashWithSalt` dnsName
      `Prelude.hashWithSalt` port

instance Prelude.NFData ServiceConnectClientAlias where
  rnf ServiceConnectClientAlias' {..} =
    Prelude.rnf dnsName `Prelude.seq` Prelude.rnf port

instance Data.ToJSON ServiceConnectClientAlias where
  toJSON ServiceConnectClientAlias' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("dnsName" Data..=) Prelude.<$> dnsName,
            Prelude.Just ("port" Data..= port)
          ]
      )
