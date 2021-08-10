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
-- Module      : Network.AWS.Lightsail.CreateContainerServiceRegistryLogin
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a temporary set of log in credentials that you can use to log in
-- to the Docker process on your local machine. After you\'re logged in,
-- you can use the native Docker commands to push your local container
-- images to the container image registry of your Amazon Lightsail account
-- so that you can use them with your Lightsail container service. The log
-- in credentials expire 12 hours after they are created, at which point
-- you will need to create a new set of log in credentials.
--
-- You can only push container images to the container service registry of
-- your Lightsail account. You cannot pull container images or perform any
-- other container image management actions on the container service
-- registry.
--
-- After you push your container images to the container image registry of
-- your Lightsail account, use the @RegisterContainerImage@ action to
-- register the pushed images to a specific Lightsail container service.
--
-- This action is not required if you install and use the Lightsail Control
-- (lightsailctl) plugin to push container images to your Lightsail
-- container service. For more information, see
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-pushing-container-images Pushing and managing container images on your Amazon Lightsail container services>
-- in the /Lightsail Dev Guide/.
module Network.AWS.Lightsail.CreateContainerServiceRegistryLogin
  ( -- * Creating a Request
    CreateContainerServiceRegistryLogin (..),
    newCreateContainerServiceRegistryLogin,

    -- * Destructuring the Response
    CreateContainerServiceRegistryLoginResponse (..),
    newCreateContainerServiceRegistryLoginResponse,

    -- * Response Lenses
    createContainerServiceRegistryLoginResponse_registryLogin,
    createContainerServiceRegistryLoginResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateContainerServiceRegistryLogin' smart constructor.
data CreateContainerServiceRegistryLogin = CreateContainerServiceRegistryLogin'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateContainerServiceRegistryLogin' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newCreateContainerServiceRegistryLogin ::
  CreateContainerServiceRegistryLogin
newCreateContainerServiceRegistryLogin =
  CreateContainerServiceRegistryLogin'

instance
  Core.AWSRequest
    CreateContainerServiceRegistryLogin
  where
  type
    AWSResponse CreateContainerServiceRegistryLogin =
      CreateContainerServiceRegistryLoginResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateContainerServiceRegistryLoginResponse'
            Prelude.<$> (x Core..?> "registryLogin")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateContainerServiceRegistryLogin

instance
  Prelude.NFData
    CreateContainerServiceRegistryLogin

instance
  Core.ToHeaders
    CreateContainerServiceRegistryLogin
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.CreateContainerServiceRegistryLogin" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Core.ToJSON
    CreateContainerServiceRegistryLogin
  where
  toJSON = Prelude.const (Core.Object Prelude.mempty)

instance
  Core.ToPath
    CreateContainerServiceRegistryLogin
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    CreateContainerServiceRegistryLogin
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateContainerServiceRegistryLoginResponse' smart constructor.
data CreateContainerServiceRegistryLoginResponse = CreateContainerServiceRegistryLoginResponse'
  { -- | An object that describes the log in information for the container
    -- service registry of your Lightsail account.
    registryLogin :: Prelude.Maybe ContainerServiceRegistryLogin,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateContainerServiceRegistryLoginResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'registryLogin', 'createContainerServiceRegistryLoginResponse_registryLogin' - An object that describes the log in information for the container
-- service registry of your Lightsail account.
--
-- 'httpStatus', 'createContainerServiceRegistryLoginResponse_httpStatus' - The response's http status code.
newCreateContainerServiceRegistryLoginResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateContainerServiceRegistryLoginResponse
newCreateContainerServiceRegistryLoginResponse
  pHttpStatus_ =
    CreateContainerServiceRegistryLoginResponse'
      { registryLogin =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | An object that describes the log in information for the container
-- service registry of your Lightsail account.
createContainerServiceRegistryLoginResponse_registryLogin :: Lens.Lens' CreateContainerServiceRegistryLoginResponse (Prelude.Maybe ContainerServiceRegistryLogin)
createContainerServiceRegistryLoginResponse_registryLogin = Lens.lens (\CreateContainerServiceRegistryLoginResponse' {registryLogin} -> registryLogin) (\s@CreateContainerServiceRegistryLoginResponse' {} a -> s {registryLogin = a} :: CreateContainerServiceRegistryLoginResponse)

-- | The response's http status code.
createContainerServiceRegistryLoginResponse_httpStatus :: Lens.Lens' CreateContainerServiceRegistryLoginResponse Prelude.Int
createContainerServiceRegistryLoginResponse_httpStatus = Lens.lens (\CreateContainerServiceRegistryLoginResponse' {httpStatus} -> httpStatus) (\s@CreateContainerServiceRegistryLoginResponse' {} a -> s {httpStatus = a} :: CreateContainerServiceRegistryLoginResponse)

instance
  Prelude.NFData
    CreateContainerServiceRegistryLoginResponse
