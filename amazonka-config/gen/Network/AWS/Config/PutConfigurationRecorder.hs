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
-- Module      : Network.AWS.Config.PutConfigurationRecorder
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new configuration recorder to record the selected resource
-- configurations.
--
-- You can use this action to change the role @roleARN@ or the
-- @recordingGroup@ of an existing recorder. To change the role, call the
-- action on the existing configuration recorder and specify a role.
--
-- Currently, you can specify only one configuration recorder per region in
-- your account.
--
-- If @ConfigurationRecorder@ does not have the __recordingGroup__
-- parameter specified, the default is to record all supported resource
-- types.
module Network.AWS.Config.PutConfigurationRecorder
  ( -- * Creating a Request
    PutConfigurationRecorder (..),
    newPutConfigurationRecorder,

    -- * Request Lenses
    putConfigurationRecorder_configurationRecorder,

    -- * Destructuring the Response
    PutConfigurationRecorderResponse (..),
    newPutConfigurationRecorderResponse,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the PutConfigurationRecorder action.
--
-- /See:/ 'newPutConfigurationRecorder' smart constructor.
data PutConfigurationRecorder = PutConfigurationRecorder'
  { -- | The configuration recorder object that records each configuration change
    -- made to the resources.
    configurationRecorder :: ConfigurationRecorder
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PutConfigurationRecorder' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configurationRecorder', 'putConfigurationRecorder_configurationRecorder' - The configuration recorder object that records each configuration change
-- made to the resources.
newPutConfigurationRecorder ::
  -- | 'configurationRecorder'
  ConfigurationRecorder ->
  PutConfigurationRecorder
newPutConfigurationRecorder pConfigurationRecorder_ =
  PutConfigurationRecorder'
    { configurationRecorder =
        pConfigurationRecorder_
    }

-- | The configuration recorder object that records each configuration change
-- made to the resources.
putConfigurationRecorder_configurationRecorder :: Lens.Lens' PutConfigurationRecorder ConfigurationRecorder
putConfigurationRecorder_configurationRecorder = Lens.lens (\PutConfigurationRecorder' {configurationRecorder} -> configurationRecorder) (\s@PutConfigurationRecorder' {} a -> s {configurationRecorder = a} :: PutConfigurationRecorder)

instance Prelude.AWSRequest PutConfigurationRecorder where
  type
    Rs PutConfigurationRecorder =
      PutConfigurationRecorderResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      PutConfigurationRecorderResponse'

instance Prelude.Hashable PutConfigurationRecorder

instance Prelude.NFData PutConfigurationRecorder

instance Prelude.ToHeaders PutConfigurationRecorder where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "StarlingDoveService.PutConfigurationRecorder" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON PutConfigurationRecorder where
  toJSON PutConfigurationRecorder' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "ConfigurationRecorder"
                  Prelude..= configurationRecorder
              )
          ]
      )

instance Prelude.ToPath PutConfigurationRecorder where
  toPath = Prelude.const "/"

instance Prelude.ToQuery PutConfigurationRecorder where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutConfigurationRecorderResponse' smart constructor.
data PutConfigurationRecorderResponse = PutConfigurationRecorderResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PutConfigurationRecorderResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newPutConfigurationRecorderResponse ::
  PutConfigurationRecorderResponse
newPutConfigurationRecorderResponse =
  PutConfigurationRecorderResponse'

instance
  Prelude.NFData
    PutConfigurationRecorderResponse
