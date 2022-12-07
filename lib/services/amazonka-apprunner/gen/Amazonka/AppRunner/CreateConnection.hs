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
-- Module      : Amazonka.AppRunner.CreateConnection
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create an App Runner connection resource. App Runner requires a
-- connection resource when you create App Runner services that access
-- private repositories from certain third-party providers. You can share a
-- connection across multiple services.
--
-- A connection resource is needed to access GitHub repositories. GitHub
-- requires a user interface approval process through the App Runner
-- console before you can use the connection.
module Amazonka.AppRunner.CreateConnection
  ( -- * Creating a Request
    CreateConnection (..),
    newCreateConnection,

    -- * Request Lenses
    createConnection_tags,
    createConnection_connectionName,
    createConnection_providerType,

    -- * Destructuring the Response
    CreateConnectionResponse (..),
    newCreateConnectionResponse,

    -- * Response Lenses
    createConnectionResponse_httpStatus,
    createConnectionResponse_connection,
  )
where

import Amazonka.AppRunner.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateConnection' smart constructor.
data CreateConnection = CreateConnection'
  { -- | A list of metadata items that you can associate with your connection
    -- resource. A tag is a key-value pair.
    tags :: Prelude.Maybe [Tag],
    -- | A name for the new connection. It must be unique across all App Runner
    -- connections for the Amazon Web Services account in the Amazon Web
    -- Services Region.
    connectionName :: Prelude.Text,
    -- | The source repository provider.
    providerType :: ProviderType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateConnection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createConnection_tags' - A list of metadata items that you can associate with your connection
-- resource. A tag is a key-value pair.
--
-- 'connectionName', 'createConnection_connectionName' - A name for the new connection. It must be unique across all App Runner
-- connections for the Amazon Web Services account in the Amazon Web
-- Services Region.
--
-- 'providerType', 'createConnection_providerType' - The source repository provider.
newCreateConnection ::
  -- | 'connectionName'
  Prelude.Text ->
  -- | 'providerType'
  ProviderType ->
  CreateConnection
newCreateConnection pConnectionName_ pProviderType_ =
  CreateConnection'
    { tags = Prelude.Nothing,
      connectionName = pConnectionName_,
      providerType = pProviderType_
    }

-- | A list of metadata items that you can associate with your connection
-- resource. A tag is a key-value pair.
createConnection_tags :: Lens.Lens' CreateConnection (Prelude.Maybe [Tag])
createConnection_tags = Lens.lens (\CreateConnection' {tags} -> tags) (\s@CreateConnection' {} a -> s {tags = a} :: CreateConnection) Prelude.. Lens.mapping Lens.coerced

-- | A name for the new connection. It must be unique across all App Runner
-- connections for the Amazon Web Services account in the Amazon Web
-- Services Region.
createConnection_connectionName :: Lens.Lens' CreateConnection Prelude.Text
createConnection_connectionName = Lens.lens (\CreateConnection' {connectionName} -> connectionName) (\s@CreateConnection' {} a -> s {connectionName = a} :: CreateConnection)

-- | The source repository provider.
createConnection_providerType :: Lens.Lens' CreateConnection ProviderType
createConnection_providerType = Lens.lens (\CreateConnection' {providerType} -> providerType) (\s@CreateConnection' {} a -> s {providerType = a} :: CreateConnection)

instance Core.AWSRequest CreateConnection where
  type
    AWSResponse CreateConnection =
      CreateConnectionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateConnectionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "Connection")
      )

instance Prelude.Hashable CreateConnection where
  hashWithSalt _salt CreateConnection' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` connectionName
      `Prelude.hashWithSalt` providerType

instance Prelude.NFData CreateConnection where
  rnf CreateConnection' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf connectionName
      `Prelude.seq` Prelude.rnf providerType

instance Data.ToHeaders CreateConnection where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AppRunner.CreateConnection" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateConnection where
  toJSON CreateConnection' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ("ConnectionName" Data..= connectionName),
            Prelude.Just ("ProviderType" Data..= providerType)
          ]
      )

instance Data.ToPath CreateConnection where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateConnection where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateConnectionResponse' smart constructor.
data CreateConnectionResponse = CreateConnectionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A description of the App Runner connection that\'s created by this
    -- request.
    connection :: Connection
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateConnectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createConnectionResponse_httpStatus' - The response's http status code.
--
-- 'connection', 'createConnectionResponse_connection' - A description of the App Runner connection that\'s created by this
-- request.
newCreateConnectionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'connection'
  Connection ->
  CreateConnectionResponse
newCreateConnectionResponse pHttpStatus_ pConnection_ =
  CreateConnectionResponse'
    { httpStatus =
        pHttpStatus_,
      connection = pConnection_
    }

-- | The response's http status code.
createConnectionResponse_httpStatus :: Lens.Lens' CreateConnectionResponse Prelude.Int
createConnectionResponse_httpStatus = Lens.lens (\CreateConnectionResponse' {httpStatus} -> httpStatus) (\s@CreateConnectionResponse' {} a -> s {httpStatus = a} :: CreateConnectionResponse)

-- | A description of the App Runner connection that\'s created by this
-- request.
createConnectionResponse_connection :: Lens.Lens' CreateConnectionResponse Connection
createConnectionResponse_connection = Lens.lens (\CreateConnectionResponse' {connection} -> connection) (\s@CreateConnectionResponse' {} a -> s {connection = a} :: CreateConnectionResponse)

instance Prelude.NFData CreateConnectionResponse where
  rnf CreateConnectionResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf connection
