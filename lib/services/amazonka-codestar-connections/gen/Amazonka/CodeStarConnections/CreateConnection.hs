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
-- Module      : Amazonka.CodeStarConnections.CreateConnection
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a connection that can then be given to other AWS services like
-- CodePipeline so that it can access third-party code repositories. The
-- connection is in pending status until the third-party connection
-- handshake is completed from the console.
module Amazonka.CodeStarConnections.CreateConnection
  ( -- * Creating a Request
    CreateConnection (..),
    newCreateConnection,

    -- * Request Lenses
    createConnection_tags,
    createConnection_hostArn,
    createConnection_providerType,
    createConnection_connectionName,

    -- * Destructuring the Response
    CreateConnectionResponse (..),
    newCreateConnectionResponse,

    -- * Response Lenses
    createConnectionResponse_tags,
    createConnectionResponse_httpStatus,
    createConnectionResponse_connectionArn,
  )
where

import Amazonka.CodeStarConnections.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateConnection' smart constructor.
data CreateConnection = CreateConnection'
  { -- | The key-value pair to use when tagging the resource.
    tags :: Prelude.Maybe [Tag],
    -- | The Amazon Resource Name (ARN) of the host associated with the
    -- connection to be created.
    hostArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the external provider where your third-party code repository
    -- is configured.
    providerType :: Prelude.Maybe ProviderType,
    -- | The name of the connection to be created. The name must be unique in the
    -- calling AWS account.
    connectionName :: Prelude.Text
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
-- 'tags', 'createConnection_tags' - The key-value pair to use when tagging the resource.
--
-- 'hostArn', 'createConnection_hostArn' - The Amazon Resource Name (ARN) of the host associated with the
-- connection to be created.
--
-- 'providerType', 'createConnection_providerType' - The name of the external provider where your third-party code repository
-- is configured.
--
-- 'connectionName', 'createConnection_connectionName' - The name of the connection to be created. The name must be unique in the
-- calling AWS account.
newCreateConnection ::
  -- | 'connectionName'
  Prelude.Text ->
  CreateConnection
newCreateConnection pConnectionName_ =
  CreateConnection'
    { tags = Prelude.Nothing,
      hostArn = Prelude.Nothing,
      providerType = Prelude.Nothing,
      connectionName = pConnectionName_
    }

-- | The key-value pair to use when tagging the resource.
createConnection_tags :: Lens.Lens' CreateConnection (Prelude.Maybe [Tag])
createConnection_tags = Lens.lens (\CreateConnection' {tags} -> tags) (\s@CreateConnection' {} a -> s {tags = a} :: CreateConnection) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the host associated with the
-- connection to be created.
createConnection_hostArn :: Lens.Lens' CreateConnection (Prelude.Maybe Prelude.Text)
createConnection_hostArn = Lens.lens (\CreateConnection' {hostArn} -> hostArn) (\s@CreateConnection' {} a -> s {hostArn = a} :: CreateConnection)

-- | The name of the external provider where your third-party code repository
-- is configured.
createConnection_providerType :: Lens.Lens' CreateConnection (Prelude.Maybe ProviderType)
createConnection_providerType = Lens.lens (\CreateConnection' {providerType} -> providerType) (\s@CreateConnection' {} a -> s {providerType = a} :: CreateConnection)

-- | The name of the connection to be created. The name must be unique in the
-- calling AWS account.
createConnection_connectionName :: Lens.Lens' CreateConnection Prelude.Text
createConnection_connectionName = Lens.lens (\CreateConnection' {connectionName} -> connectionName) (\s@CreateConnection' {} a -> s {connectionName = a} :: CreateConnection)

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
            Prelude.<$> (x Core..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "ConnectionArn")
      )

instance Prelude.Hashable CreateConnection where
  hashWithSalt _salt CreateConnection' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` hostArn
      `Prelude.hashWithSalt` providerType
      `Prelude.hashWithSalt` connectionName

instance Prelude.NFData CreateConnection where
  rnf CreateConnection' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf hostArn
      `Prelude.seq` Prelude.rnf providerType
      `Prelude.seq` Prelude.rnf connectionName

instance Core.ToHeaders CreateConnection where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "com.amazonaws.codestar.connections.CodeStar_connections_20191201.CreateConnection" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateConnection where
  toJSON CreateConnection' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Tags" Core..=) Prelude.<$> tags,
            ("HostArn" Core..=) Prelude.<$> hostArn,
            ("ProviderType" Core..=) Prelude.<$> providerType,
            Prelude.Just
              ("ConnectionName" Core..= connectionName)
          ]
      )

instance Core.ToPath CreateConnection where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateConnection where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateConnectionResponse' smart constructor.
data CreateConnectionResponse = CreateConnectionResponse'
  { -- | Specifies the tags applied to the resource.
    tags :: Prelude.Maybe [Tag],
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the connection to be created. The ARN
    -- is used as the connection reference when the connection is shared
    -- between AWS services.
    --
    -- The ARN is never reused if the connection is deleted.
    connectionArn :: Prelude.Text
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
-- 'tags', 'createConnectionResponse_tags' - Specifies the tags applied to the resource.
--
-- 'httpStatus', 'createConnectionResponse_httpStatus' - The response's http status code.
--
-- 'connectionArn', 'createConnectionResponse_connectionArn' - The Amazon Resource Name (ARN) of the connection to be created. The ARN
-- is used as the connection reference when the connection is shared
-- between AWS services.
--
-- The ARN is never reused if the connection is deleted.
newCreateConnectionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'connectionArn'
  Prelude.Text ->
  CreateConnectionResponse
newCreateConnectionResponse
  pHttpStatus_
  pConnectionArn_ =
    CreateConnectionResponse'
      { tags = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        connectionArn = pConnectionArn_
      }

-- | Specifies the tags applied to the resource.
createConnectionResponse_tags :: Lens.Lens' CreateConnectionResponse (Prelude.Maybe [Tag])
createConnectionResponse_tags = Lens.lens (\CreateConnectionResponse' {tags} -> tags) (\s@CreateConnectionResponse' {} a -> s {tags = a} :: CreateConnectionResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
createConnectionResponse_httpStatus :: Lens.Lens' CreateConnectionResponse Prelude.Int
createConnectionResponse_httpStatus = Lens.lens (\CreateConnectionResponse' {httpStatus} -> httpStatus) (\s@CreateConnectionResponse' {} a -> s {httpStatus = a} :: CreateConnectionResponse)

-- | The Amazon Resource Name (ARN) of the connection to be created. The ARN
-- is used as the connection reference when the connection is shared
-- between AWS services.
--
-- The ARN is never reused if the connection is deleted.
createConnectionResponse_connectionArn :: Lens.Lens' CreateConnectionResponse Prelude.Text
createConnectionResponse_connectionArn = Lens.lens (\CreateConnectionResponse' {connectionArn} -> connectionArn) (\s@CreateConnectionResponse' {} a -> s {connectionArn = a} :: CreateConnectionResponse)

instance Prelude.NFData CreateConnectionResponse where
  rnf CreateConnectionResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf connectionArn
