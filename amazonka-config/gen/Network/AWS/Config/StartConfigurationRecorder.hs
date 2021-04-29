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
-- Module      : Network.AWS.Config.StartConfigurationRecorder
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts recording configurations of the AWS resources you have selected
-- to record in your AWS account.
--
-- You must have created at least one delivery channel to successfully
-- start the configuration recorder.
module Network.AWS.Config.StartConfigurationRecorder
  ( -- * Creating a Request
    StartConfigurationRecorder (..),
    newStartConfigurationRecorder,

    -- * Request Lenses
    startConfigurationRecorder_configurationRecorderName,

    -- * Destructuring the Response
    StartConfigurationRecorderResponse (..),
    newStartConfigurationRecorderResponse,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the StartConfigurationRecorder action.
--
-- /See:/ 'newStartConfigurationRecorder' smart constructor.
data StartConfigurationRecorder = StartConfigurationRecorder'
  { -- | The name of the recorder object that records each configuration change
    -- made to the resources.
    configurationRecorderName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StartConfigurationRecorder' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configurationRecorderName', 'startConfigurationRecorder_configurationRecorderName' - The name of the recorder object that records each configuration change
-- made to the resources.
newStartConfigurationRecorder ::
  -- | 'configurationRecorderName'
  Prelude.Text ->
  StartConfigurationRecorder
newStartConfigurationRecorder
  pConfigurationRecorderName_ =
    StartConfigurationRecorder'
      { configurationRecorderName =
          pConfigurationRecorderName_
      }

-- | The name of the recorder object that records each configuration change
-- made to the resources.
startConfigurationRecorder_configurationRecorderName :: Lens.Lens' StartConfigurationRecorder Prelude.Text
startConfigurationRecorder_configurationRecorderName = Lens.lens (\StartConfigurationRecorder' {configurationRecorderName} -> configurationRecorderName) (\s@StartConfigurationRecorder' {} a -> s {configurationRecorderName = a} :: StartConfigurationRecorder)

instance
  Prelude.AWSRequest
    StartConfigurationRecorder
  where
  type
    Rs StartConfigurationRecorder =
      StartConfigurationRecorderResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      StartConfigurationRecorderResponse'

instance Prelude.Hashable StartConfigurationRecorder

instance Prelude.NFData StartConfigurationRecorder

instance Prelude.ToHeaders StartConfigurationRecorder where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "StarlingDoveService.StartConfigurationRecorder" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON StartConfigurationRecorder where
  toJSON StartConfigurationRecorder' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "ConfigurationRecorderName"
                  Prelude..= configurationRecorderName
              )
          ]
      )

instance Prelude.ToPath StartConfigurationRecorder where
  toPath = Prelude.const "/"

instance Prelude.ToQuery StartConfigurationRecorder where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartConfigurationRecorderResponse' smart constructor.
data StartConfigurationRecorderResponse = StartConfigurationRecorderResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StartConfigurationRecorderResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newStartConfigurationRecorderResponse ::
  StartConfigurationRecorderResponse
newStartConfigurationRecorderResponse =
  StartConfigurationRecorderResponse'

instance
  Prelude.NFData
    StartConfigurationRecorderResponse
