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
-- Module      : Network.AWS.CognitoIdentityProvider.DescribeResourceServer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a resource server.
module Network.AWS.CognitoIdentityProvider.DescribeResourceServer
  ( -- * Creating a Request
    DescribeResourceServer (..),
    newDescribeResourceServer,

    -- * Request Lenses
    describeResourceServer_userPoolId,
    describeResourceServer_identifier,

    -- * Destructuring the Response
    DescribeResourceServerResponse (..),
    newDescribeResourceServerResponse,

    -- * Response Lenses
    describeResourceServerResponse_httpStatus,
    describeResourceServerResponse_resourceServer,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeResourceServer' smart constructor.
data DescribeResourceServer = DescribeResourceServer'
  { -- | The user pool ID for the user pool that hosts the resource server.
    userPoolId :: Prelude.Text,
    -- | The identifier for the resource server
    identifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeResourceServer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userPoolId', 'describeResourceServer_userPoolId' - The user pool ID for the user pool that hosts the resource server.
--
-- 'identifier', 'describeResourceServer_identifier' - The identifier for the resource server
newDescribeResourceServer ::
  -- | 'userPoolId'
  Prelude.Text ->
  -- | 'identifier'
  Prelude.Text ->
  DescribeResourceServer
newDescribeResourceServer pUserPoolId_ pIdentifier_ =
  DescribeResourceServer'
    { userPoolId = pUserPoolId_,
      identifier = pIdentifier_
    }

-- | The user pool ID for the user pool that hosts the resource server.
describeResourceServer_userPoolId :: Lens.Lens' DescribeResourceServer Prelude.Text
describeResourceServer_userPoolId = Lens.lens (\DescribeResourceServer' {userPoolId} -> userPoolId) (\s@DescribeResourceServer' {} a -> s {userPoolId = a} :: DescribeResourceServer)

-- | The identifier for the resource server
describeResourceServer_identifier :: Lens.Lens' DescribeResourceServer Prelude.Text
describeResourceServer_identifier = Lens.lens (\DescribeResourceServer' {identifier} -> identifier) (\s@DescribeResourceServer' {} a -> s {identifier = a} :: DescribeResourceServer)

instance Core.AWSRequest DescribeResourceServer where
  type
    AWSResponse DescribeResourceServer =
      DescribeResourceServerResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeResourceServerResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "ResourceServer")
      )

instance Prelude.Hashable DescribeResourceServer

instance Prelude.NFData DescribeResourceServer

instance Core.ToHeaders DescribeResourceServer where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityProviderService.DescribeResourceServer" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeResourceServer where
  toJSON DescribeResourceServer' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("UserPoolId" Core..= userPoolId),
            Prelude.Just ("Identifier" Core..= identifier)
          ]
      )

instance Core.ToPath DescribeResourceServer where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeResourceServer where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeResourceServerResponse' smart constructor.
data DescribeResourceServerResponse = DescribeResourceServerResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The resource server.
    resourceServer :: ResourceServerType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeResourceServerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'describeResourceServerResponse_httpStatus' - The response's http status code.
--
-- 'resourceServer', 'describeResourceServerResponse_resourceServer' - The resource server.
newDescribeResourceServerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'resourceServer'
  ResourceServerType ->
  DescribeResourceServerResponse
newDescribeResourceServerResponse
  pHttpStatus_
  pResourceServer_ =
    DescribeResourceServerResponse'
      { httpStatus =
          pHttpStatus_,
        resourceServer = pResourceServer_
      }

-- | The response's http status code.
describeResourceServerResponse_httpStatus :: Lens.Lens' DescribeResourceServerResponse Prelude.Int
describeResourceServerResponse_httpStatus = Lens.lens (\DescribeResourceServerResponse' {httpStatus} -> httpStatus) (\s@DescribeResourceServerResponse' {} a -> s {httpStatus = a} :: DescribeResourceServerResponse)

-- | The resource server.
describeResourceServerResponse_resourceServer :: Lens.Lens' DescribeResourceServerResponse ResourceServerType
describeResourceServerResponse_resourceServer = Lens.lens (\DescribeResourceServerResponse' {resourceServer} -> resourceServer) (\s@DescribeResourceServerResponse' {} a -> s {resourceServer = a} :: DescribeResourceServerResponse)

instance
  Prelude.NFData
    DescribeResourceServerResponse
