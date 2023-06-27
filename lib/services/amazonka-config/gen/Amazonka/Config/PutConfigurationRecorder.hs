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
-- Module      : Amazonka.Config.PutConfigurationRecorder
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new configuration recorder to record configuration changes for
-- specified resource types.
--
-- You can also use this action to change the @roleARN@ or the
-- @recordingGroup@ of an existing recorder. For more information, see
-- <https://docs.aws.amazon.com/config/latest/developerguide/stop-start-recorder.html Managing the Configuration Recorder>
-- in the /Config Developer Guide/.
--
-- You can specify only one configuration recorder for each Amazon Web
-- Services Region for each account.
--
-- If the configuration recorder does not have the @recordingGroup@ field
-- specified, the default is to record all supported resource types.
module Amazonka.Config.PutConfigurationRecorder
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

import Amazonka.Config.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The input for the PutConfigurationRecorder action.
--
-- /See:/ 'newPutConfigurationRecorder' smart constructor.
data PutConfigurationRecorder = PutConfigurationRecorder'
  { -- | An object for the configuration recorder to record configuration changes
    -- for specified resource types.
    configurationRecorder :: ConfigurationRecorder
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutConfigurationRecorder' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configurationRecorder', 'putConfigurationRecorder_configurationRecorder' - An object for the configuration recorder to record configuration changes
-- for specified resource types.
newPutConfigurationRecorder ::
  -- | 'configurationRecorder'
  ConfigurationRecorder ->
  PutConfigurationRecorder
newPutConfigurationRecorder pConfigurationRecorder_ =
  PutConfigurationRecorder'
    { configurationRecorder =
        pConfigurationRecorder_
    }

-- | An object for the configuration recorder to record configuration changes
-- for specified resource types.
putConfigurationRecorder_configurationRecorder :: Lens.Lens' PutConfigurationRecorder ConfigurationRecorder
putConfigurationRecorder_configurationRecorder = Lens.lens (\PutConfigurationRecorder' {configurationRecorder} -> configurationRecorder) (\s@PutConfigurationRecorder' {} a -> s {configurationRecorder = a} :: PutConfigurationRecorder)

instance Core.AWSRequest PutConfigurationRecorder where
  type
    AWSResponse PutConfigurationRecorder =
      PutConfigurationRecorderResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      PutConfigurationRecorderResponse'

instance Prelude.Hashable PutConfigurationRecorder where
  hashWithSalt _salt PutConfigurationRecorder' {..} =
    _salt `Prelude.hashWithSalt` configurationRecorder

instance Prelude.NFData PutConfigurationRecorder where
  rnf PutConfigurationRecorder' {..} =
    Prelude.rnf configurationRecorder

instance Data.ToHeaders PutConfigurationRecorder where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StarlingDoveService.PutConfigurationRecorder" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutConfigurationRecorder where
  toJSON PutConfigurationRecorder' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "ConfigurationRecorder"
                  Data..= configurationRecorder
              )
          ]
      )

instance Data.ToPath PutConfigurationRecorder where
  toPath = Prelude.const "/"

instance Data.ToQuery PutConfigurationRecorder where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutConfigurationRecorderResponse' smart constructor.
data PutConfigurationRecorderResponse = PutConfigurationRecorderResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf _ = ()
