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
-- Module      : Network.AWS.AutoScaling.DeleteLaunchConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified launch configuration.
--
-- The launch configuration must not be attached to an Auto Scaling group.
-- When this call completes, the launch configuration is no longer
-- available for use.
module Network.AWS.AutoScaling.DeleteLaunchConfiguration
  ( -- * Creating a Request
    DeleteLaunchConfiguration (..),
    newDeleteLaunchConfiguration,

    -- * Request Lenses
    deleteLaunchConfiguration_launchConfigurationName,

    -- * Destructuring the Response
    DeleteLaunchConfigurationResponse (..),
    newDeleteLaunchConfigurationResponse,
  )
where

import Network.AWS.AutoScaling.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteLaunchConfiguration' smart constructor.
data DeleteLaunchConfiguration = DeleteLaunchConfiguration'
  { -- | The name of the launch configuration.
    launchConfigurationName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteLaunchConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'launchConfigurationName', 'deleteLaunchConfiguration_launchConfigurationName' - The name of the launch configuration.
newDeleteLaunchConfiguration ::
  -- | 'launchConfigurationName'
  Core.Text ->
  DeleteLaunchConfiguration
newDeleteLaunchConfiguration
  pLaunchConfigurationName_ =
    DeleteLaunchConfiguration'
      { launchConfigurationName =
          pLaunchConfigurationName_
      }

-- | The name of the launch configuration.
deleteLaunchConfiguration_launchConfigurationName :: Lens.Lens' DeleteLaunchConfiguration Core.Text
deleteLaunchConfiguration_launchConfigurationName = Lens.lens (\DeleteLaunchConfiguration' {launchConfigurationName} -> launchConfigurationName) (\s@DeleteLaunchConfiguration' {} a -> s {launchConfigurationName = a} :: DeleteLaunchConfiguration)

instance Core.AWSRequest DeleteLaunchConfiguration where
  type
    AWSResponse DeleteLaunchConfiguration =
      DeleteLaunchConfigurationResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull
      DeleteLaunchConfigurationResponse'

instance Core.Hashable DeleteLaunchConfiguration

instance Core.NFData DeleteLaunchConfiguration

instance Core.ToHeaders DeleteLaunchConfiguration where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DeleteLaunchConfiguration where
  toPath = Core.const "/"

instance Core.ToQuery DeleteLaunchConfiguration where
  toQuery DeleteLaunchConfiguration' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DeleteLaunchConfiguration" :: Core.ByteString),
        "Version" Core.=: ("2011-01-01" :: Core.ByteString),
        "LaunchConfigurationName"
          Core.=: launchConfigurationName
      ]

-- | /See:/ 'newDeleteLaunchConfigurationResponse' smart constructor.
data DeleteLaunchConfigurationResponse = DeleteLaunchConfigurationResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteLaunchConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteLaunchConfigurationResponse ::
  DeleteLaunchConfigurationResponse
newDeleteLaunchConfigurationResponse =
  DeleteLaunchConfigurationResponse'

instance
  Core.NFData
    DeleteLaunchConfigurationResponse
