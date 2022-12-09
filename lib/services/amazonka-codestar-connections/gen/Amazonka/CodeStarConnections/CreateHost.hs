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
-- Module      : Amazonka.CodeStarConnections.CreateHost
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a resource that represents the infrastructure where a
-- third-party provider is installed. The host is used when you create
-- connections to an installed third-party provider type, such as GitHub
-- Enterprise Server. You create one host for all connections to that
-- provider.
--
-- A host created through the CLI or the SDK is in \`PENDING\` status by
-- default. You can make its status \`AVAILABLE\` by setting up the host in
-- the console.
module Amazonka.CodeStarConnections.CreateHost
  ( -- * Creating a Request
    CreateHost (..),
    newCreateHost,

    -- * Request Lenses
    createHost_tags,
    createHost_vpcConfiguration,
    createHost_name,
    createHost_providerType,
    createHost_providerEndpoint,

    -- * Destructuring the Response
    CreateHostResponse (..),
    newCreateHostResponse,

    -- * Response Lenses
    createHostResponse_hostArn,
    createHostResponse_tags,
    createHostResponse_httpStatus,
  )
where

import Amazonka.CodeStarConnections.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateHost' smart constructor.
data CreateHost = CreateHost'
  { tags :: Prelude.Maybe [Tag],
    -- | The VPC configuration to be provisioned for the host. A VPC must be
    -- configured and the infrastructure to be represented by the host must
    -- already be connected to the VPC.
    vpcConfiguration :: Prelude.Maybe VpcConfiguration,
    -- | The name of the host to be created. The name must be unique in the
    -- calling AWS account.
    name :: Prelude.Text,
    -- | The name of the installed provider to be associated with your
    -- connection. The host resource represents the infrastructure where your
    -- provider type is installed. The valid provider type is GitHub Enterprise
    -- Server.
    providerType :: ProviderType,
    -- | The endpoint of the infrastructure to be represented by the host after
    -- it is created.
    providerEndpoint :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateHost' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createHost_tags' - Undocumented member.
--
-- 'vpcConfiguration', 'createHost_vpcConfiguration' - The VPC configuration to be provisioned for the host. A VPC must be
-- configured and the infrastructure to be represented by the host must
-- already be connected to the VPC.
--
-- 'name', 'createHost_name' - The name of the host to be created. The name must be unique in the
-- calling AWS account.
--
-- 'providerType', 'createHost_providerType' - The name of the installed provider to be associated with your
-- connection. The host resource represents the infrastructure where your
-- provider type is installed. The valid provider type is GitHub Enterprise
-- Server.
--
-- 'providerEndpoint', 'createHost_providerEndpoint' - The endpoint of the infrastructure to be represented by the host after
-- it is created.
newCreateHost ::
  -- | 'name'
  Prelude.Text ->
  -- | 'providerType'
  ProviderType ->
  -- | 'providerEndpoint'
  Prelude.Text ->
  CreateHost
newCreateHost
  pName_
  pProviderType_
  pProviderEndpoint_ =
    CreateHost'
      { tags = Prelude.Nothing,
        vpcConfiguration = Prelude.Nothing,
        name = pName_,
        providerType = pProviderType_,
        providerEndpoint = pProviderEndpoint_
      }

-- | Undocumented member.
createHost_tags :: Lens.Lens' CreateHost (Prelude.Maybe [Tag])
createHost_tags = Lens.lens (\CreateHost' {tags} -> tags) (\s@CreateHost' {} a -> s {tags = a} :: CreateHost) Prelude.. Lens.mapping Lens.coerced

-- | The VPC configuration to be provisioned for the host. A VPC must be
-- configured and the infrastructure to be represented by the host must
-- already be connected to the VPC.
createHost_vpcConfiguration :: Lens.Lens' CreateHost (Prelude.Maybe VpcConfiguration)
createHost_vpcConfiguration = Lens.lens (\CreateHost' {vpcConfiguration} -> vpcConfiguration) (\s@CreateHost' {} a -> s {vpcConfiguration = a} :: CreateHost)

-- | The name of the host to be created. The name must be unique in the
-- calling AWS account.
createHost_name :: Lens.Lens' CreateHost Prelude.Text
createHost_name = Lens.lens (\CreateHost' {name} -> name) (\s@CreateHost' {} a -> s {name = a} :: CreateHost)

-- | The name of the installed provider to be associated with your
-- connection. The host resource represents the infrastructure where your
-- provider type is installed. The valid provider type is GitHub Enterprise
-- Server.
createHost_providerType :: Lens.Lens' CreateHost ProviderType
createHost_providerType = Lens.lens (\CreateHost' {providerType} -> providerType) (\s@CreateHost' {} a -> s {providerType = a} :: CreateHost)

-- | The endpoint of the infrastructure to be represented by the host after
-- it is created.
createHost_providerEndpoint :: Lens.Lens' CreateHost Prelude.Text
createHost_providerEndpoint = Lens.lens (\CreateHost' {providerEndpoint} -> providerEndpoint) (\s@CreateHost' {} a -> s {providerEndpoint = a} :: CreateHost)

instance Core.AWSRequest CreateHost where
  type AWSResponse CreateHost = CreateHostResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateHostResponse'
            Prelude.<$> (x Data..?> "HostArn")
            Prelude.<*> (x Data..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateHost where
  hashWithSalt _salt CreateHost' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` vpcConfiguration
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` providerType
      `Prelude.hashWithSalt` providerEndpoint

instance Prelude.NFData CreateHost where
  rnf CreateHost' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf vpcConfiguration
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf providerType
      `Prelude.seq` Prelude.rnf providerEndpoint

instance Data.ToHeaders CreateHost where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "com.amazonaws.codestar.connections.CodeStar_connections_20191201.CreateHost" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateHost where
  toJSON CreateHost' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Tags" Data..=) Prelude.<$> tags,
            ("VpcConfiguration" Data..=)
              Prelude.<$> vpcConfiguration,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("ProviderType" Data..= providerType),
            Prelude.Just
              ("ProviderEndpoint" Data..= providerEndpoint)
          ]
      )

instance Data.ToPath CreateHost where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateHost where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateHostResponse' smart constructor.
data CreateHostResponse = CreateHostResponse'
  { -- | The Amazon Resource Name (ARN) of the host to be created.
    hostArn :: Prelude.Maybe Prelude.Text,
    tags :: Prelude.Maybe [Tag],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateHostResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hostArn', 'createHostResponse_hostArn' - The Amazon Resource Name (ARN) of the host to be created.
--
-- 'tags', 'createHostResponse_tags' - Undocumented member.
--
-- 'httpStatus', 'createHostResponse_httpStatus' - The response's http status code.
newCreateHostResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateHostResponse
newCreateHostResponse pHttpStatus_ =
  CreateHostResponse'
    { hostArn = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the host to be created.
createHostResponse_hostArn :: Lens.Lens' CreateHostResponse (Prelude.Maybe Prelude.Text)
createHostResponse_hostArn = Lens.lens (\CreateHostResponse' {hostArn} -> hostArn) (\s@CreateHostResponse' {} a -> s {hostArn = a} :: CreateHostResponse)

-- | Undocumented member.
createHostResponse_tags :: Lens.Lens' CreateHostResponse (Prelude.Maybe [Tag])
createHostResponse_tags = Lens.lens (\CreateHostResponse' {tags} -> tags) (\s@CreateHostResponse' {} a -> s {tags = a} :: CreateHostResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
createHostResponse_httpStatus :: Lens.Lens' CreateHostResponse Prelude.Int
createHostResponse_httpStatus = Lens.lens (\CreateHostResponse' {httpStatus} -> httpStatus) (\s@CreateHostResponse' {} a -> s {httpStatus = a} :: CreateHostResponse)

instance Prelude.NFData CreateHostResponse where
  rnf CreateHostResponse' {..} =
    Prelude.rnf hostArn
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
