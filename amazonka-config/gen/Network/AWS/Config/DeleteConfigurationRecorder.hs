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
-- Module      : Network.AWS.Config.DeleteConfigurationRecorder
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the configuration recorder.
--
-- After the configuration recorder is deleted, AWS Config will not record
-- resource configuration changes until you create a new configuration
-- recorder.
--
-- This action does not delete the configuration information that was
-- previously recorded. You will be able to access the previously recorded
-- information by using the @GetResourceConfigHistory@ action, but you will
-- not be able to access this information in the AWS Config console until
-- you create a new configuration recorder.
module Network.AWS.Config.DeleteConfigurationRecorder
  ( -- * Creating a Request
    DeleteConfigurationRecorder (..),
    newDeleteConfigurationRecorder,

    -- * Request Lenses
    deleteConfigurationRecorder_configurationRecorderName,

    -- * Destructuring the Response
    DeleteConfigurationRecorderResponse (..),
    newDeleteConfigurationRecorderResponse,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The request object for the @DeleteConfigurationRecorder@ action.
--
-- /See:/ 'newDeleteConfigurationRecorder' smart constructor.
data DeleteConfigurationRecorder = DeleteConfigurationRecorder'
  { -- | The name of the configuration recorder to be deleted. You can retrieve
    -- the name of your configuration recorder by using the
    -- @DescribeConfigurationRecorders@ action.
    configurationRecorderName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteConfigurationRecorder' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configurationRecorderName', 'deleteConfigurationRecorder_configurationRecorderName' - The name of the configuration recorder to be deleted. You can retrieve
-- the name of your configuration recorder by using the
-- @DescribeConfigurationRecorders@ action.
newDeleteConfigurationRecorder ::
  -- | 'configurationRecorderName'
  Core.Text ->
  DeleteConfigurationRecorder
newDeleteConfigurationRecorder
  pConfigurationRecorderName_ =
    DeleteConfigurationRecorder'
      { configurationRecorderName =
          pConfigurationRecorderName_
      }

-- | The name of the configuration recorder to be deleted. You can retrieve
-- the name of your configuration recorder by using the
-- @DescribeConfigurationRecorders@ action.
deleteConfigurationRecorder_configurationRecorderName :: Lens.Lens' DeleteConfigurationRecorder Core.Text
deleteConfigurationRecorder_configurationRecorderName = Lens.lens (\DeleteConfigurationRecorder' {configurationRecorderName} -> configurationRecorderName) (\s@DeleteConfigurationRecorder' {} a -> s {configurationRecorderName = a} :: DeleteConfigurationRecorder)

instance Core.AWSRequest DeleteConfigurationRecorder where
  type
    AWSResponse DeleteConfigurationRecorder =
      DeleteConfigurationRecorderResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      DeleteConfigurationRecorderResponse'

instance Core.Hashable DeleteConfigurationRecorder

instance Core.NFData DeleteConfigurationRecorder

instance Core.ToHeaders DeleteConfigurationRecorder where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StarlingDoveService.DeleteConfigurationRecorder" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteConfigurationRecorder where
  toJSON DeleteConfigurationRecorder' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "ConfigurationRecorderName"
                  Core..= configurationRecorderName
              )
          ]
      )

instance Core.ToPath DeleteConfigurationRecorder where
  toPath = Core.const "/"

instance Core.ToQuery DeleteConfigurationRecorder where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteConfigurationRecorderResponse' smart constructor.
data DeleteConfigurationRecorderResponse = DeleteConfigurationRecorderResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteConfigurationRecorderResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteConfigurationRecorderResponse ::
  DeleteConfigurationRecorderResponse
newDeleteConfigurationRecorderResponse =
  DeleteConfigurationRecorderResponse'

instance
  Core.NFData
    DeleteConfigurationRecorderResponse
