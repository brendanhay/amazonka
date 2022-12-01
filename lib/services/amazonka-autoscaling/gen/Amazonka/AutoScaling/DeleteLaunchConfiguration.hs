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
-- Module      : Amazonka.AutoScaling.DeleteLaunchConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.AutoScaling.DeleteLaunchConfiguration
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

import Amazonka.AutoScaling.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteLaunchConfiguration' smart constructor.
data DeleteLaunchConfiguration = DeleteLaunchConfiguration'
  { -- | The name of the launch configuration.
    launchConfigurationName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DeleteLaunchConfiguration
newDeleteLaunchConfiguration
  pLaunchConfigurationName_ =
    DeleteLaunchConfiguration'
      { launchConfigurationName =
          pLaunchConfigurationName_
      }

-- | The name of the launch configuration.
deleteLaunchConfiguration_launchConfigurationName :: Lens.Lens' DeleteLaunchConfiguration Prelude.Text
deleteLaunchConfiguration_launchConfigurationName = Lens.lens (\DeleteLaunchConfiguration' {launchConfigurationName} -> launchConfigurationName) (\s@DeleteLaunchConfiguration' {} a -> s {launchConfigurationName = a} :: DeleteLaunchConfiguration)

instance Core.AWSRequest DeleteLaunchConfiguration where
  type
    AWSResponse DeleteLaunchConfiguration =
      DeleteLaunchConfigurationResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull
      DeleteLaunchConfigurationResponse'

instance Prelude.Hashable DeleteLaunchConfiguration where
  hashWithSalt _salt DeleteLaunchConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` launchConfigurationName

instance Prelude.NFData DeleteLaunchConfiguration where
  rnf DeleteLaunchConfiguration' {..} =
    Prelude.rnf launchConfigurationName

instance Core.ToHeaders DeleteLaunchConfiguration where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DeleteLaunchConfiguration where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteLaunchConfiguration where
  toQuery DeleteLaunchConfiguration' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DeleteLaunchConfiguration" :: Prelude.ByteString),
        "Version"
          Core.=: ("2011-01-01" :: Prelude.ByteString),
        "LaunchConfigurationName"
          Core.=: launchConfigurationName
      ]

-- | /See:/ 'newDeleteLaunchConfigurationResponse' smart constructor.
data DeleteLaunchConfigurationResponse = DeleteLaunchConfigurationResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteLaunchConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteLaunchConfigurationResponse ::
  DeleteLaunchConfigurationResponse
newDeleteLaunchConfigurationResponse =
  DeleteLaunchConfigurationResponse'

instance
  Prelude.NFData
    DeleteLaunchConfigurationResponse
  where
  rnf _ = ()
