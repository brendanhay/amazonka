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
-- Module      : Amazonka.Config.DeleteConfigurationRecorder
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the configuration recorder.
--
-- After the configuration recorder is deleted, Config will not record
-- resource configuration changes until you create a new configuration
-- recorder.
--
-- This action does not delete the configuration information that was
-- previously recorded. You will be able to access the previously recorded
-- information by using the @GetResourceConfigHistory@ action, but you will
-- not be able to access this information in the Config console until you
-- create a new configuration recorder.
module Amazonka.Config.DeleteConfigurationRecorder
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

import Amazonka.Config.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The request object for the @DeleteConfigurationRecorder@ action.
--
-- /See:/ 'newDeleteConfigurationRecorder' smart constructor.
data DeleteConfigurationRecorder = DeleteConfigurationRecorder'
  { -- | The name of the configuration recorder to be deleted. You can retrieve
    -- the name of your configuration recorder by using the
    -- @DescribeConfigurationRecorders@ action.
    configurationRecorderName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
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
deleteConfigurationRecorder_configurationRecorderName :: Lens.Lens' DeleteConfigurationRecorder Prelude.Text
deleteConfigurationRecorder_configurationRecorderName = Lens.lens (\DeleteConfigurationRecorder' {configurationRecorderName} -> configurationRecorderName) (\s@DeleteConfigurationRecorder' {} a -> s {configurationRecorderName = a} :: DeleteConfigurationRecorder)

instance Core.AWSRequest DeleteConfigurationRecorder where
  type
    AWSResponse DeleteConfigurationRecorder =
      DeleteConfigurationRecorderResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      DeleteConfigurationRecorderResponse'

instance Prelude.Hashable DeleteConfigurationRecorder where
  hashWithSalt _salt DeleteConfigurationRecorder' {..} =
    _salt
      `Prelude.hashWithSalt` configurationRecorderName

instance Prelude.NFData DeleteConfigurationRecorder where
  rnf DeleteConfigurationRecorder' {..} =
    Prelude.rnf configurationRecorderName

instance Data.ToHeaders DeleteConfigurationRecorder where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StarlingDoveService.DeleteConfigurationRecorder" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteConfigurationRecorder where
  toJSON DeleteConfigurationRecorder' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "ConfigurationRecorderName"
                  Data..= configurationRecorderName
              )
          ]
      )

instance Data.ToPath DeleteConfigurationRecorder where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteConfigurationRecorder where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteConfigurationRecorderResponse' smart constructor.
data DeleteConfigurationRecorderResponse = DeleteConfigurationRecorderResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteConfigurationRecorderResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteConfigurationRecorderResponse ::
  DeleteConfigurationRecorderResponse
newDeleteConfigurationRecorderResponse =
  DeleteConfigurationRecorderResponse'

instance
  Prelude.NFData
    DeleteConfigurationRecorderResponse
  where
  rnf _ = ()
