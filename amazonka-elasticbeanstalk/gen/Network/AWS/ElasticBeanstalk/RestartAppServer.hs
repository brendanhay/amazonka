{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.ElasticBeanstalk.RestartAppServer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Causes the environment to restart the application container server
-- running on each Amazon EC2 instance.
module Network.AWS.ElasticBeanstalk.RestartAppServer
  ( -- * Creating a Request
    RestartAppServer (..),
    newRestartAppServer,

    -- * Request Lenses
    restartAppServer_environmentId,
    restartAppServer_environmentName,

    -- * Destructuring the Response
    RestartAppServerResponse (..),
    newRestartAppServerResponse,
  )
where

import Network.AWS.ElasticBeanstalk.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newRestartAppServer' smart constructor.
data RestartAppServer = RestartAppServer'
  { -- | The ID of the environment to restart the server for.
    --
    -- Condition: You must specify either this or an EnvironmentName, or both.
    -- If you do not specify either, AWS Elastic Beanstalk returns
    -- @MissingRequiredParameter@ error.
    environmentId :: Prelude.Maybe Prelude.Text,
    -- | The name of the environment to restart the server for.
    --
    -- Condition: You must specify either this or an EnvironmentId, or both. If
    -- you do not specify either, AWS Elastic Beanstalk returns
    -- @MissingRequiredParameter@ error.
    environmentName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RestartAppServer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'environmentId', 'restartAppServer_environmentId' - The ID of the environment to restart the server for.
--
-- Condition: You must specify either this or an EnvironmentName, or both.
-- If you do not specify either, AWS Elastic Beanstalk returns
-- @MissingRequiredParameter@ error.
--
-- 'environmentName', 'restartAppServer_environmentName' - The name of the environment to restart the server for.
--
-- Condition: You must specify either this or an EnvironmentId, or both. If
-- you do not specify either, AWS Elastic Beanstalk returns
-- @MissingRequiredParameter@ error.
newRestartAppServer ::
  RestartAppServer
newRestartAppServer =
  RestartAppServer'
    { environmentId = Prelude.Nothing,
      environmentName = Prelude.Nothing
    }

-- | The ID of the environment to restart the server for.
--
-- Condition: You must specify either this or an EnvironmentName, or both.
-- If you do not specify either, AWS Elastic Beanstalk returns
-- @MissingRequiredParameter@ error.
restartAppServer_environmentId :: Lens.Lens' RestartAppServer (Prelude.Maybe Prelude.Text)
restartAppServer_environmentId = Lens.lens (\RestartAppServer' {environmentId} -> environmentId) (\s@RestartAppServer' {} a -> s {environmentId = a} :: RestartAppServer)

-- | The name of the environment to restart the server for.
--
-- Condition: You must specify either this or an EnvironmentId, or both. If
-- you do not specify either, AWS Elastic Beanstalk returns
-- @MissingRequiredParameter@ error.
restartAppServer_environmentName :: Lens.Lens' RestartAppServer (Prelude.Maybe Prelude.Text)
restartAppServer_environmentName = Lens.lens (\RestartAppServer' {environmentName} -> environmentName) (\s@RestartAppServer' {} a -> s {environmentName = a} :: RestartAppServer)

instance Prelude.AWSRequest RestartAppServer where
  type Rs RestartAppServer = RestartAppServerResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull RestartAppServerResponse'

instance Prelude.Hashable RestartAppServer

instance Prelude.NFData RestartAppServer

instance Prelude.ToHeaders RestartAppServer where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath RestartAppServer where
  toPath = Prelude.const "/"

instance Prelude.ToQuery RestartAppServer where
  toQuery RestartAppServer' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("RestartAppServer" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-12-01" :: Prelude.ByteString),
        "EnvironmentId" Prelude.=: environmentId,
        "EnvironmentName" Prelude.=: environmentName
      ]

-- | /See:/ 'newRestartAppServerResponse' smart constructor.
data RestartAppServerResponse = RestartAppServerResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RestartAppServerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newRestartAppServerResponse ::
  RestartAppServerResponse
newRestartAppServerResponse =
  RestartAppServerResponse'

instance Prelude.NFData RestartAppServerResponse
