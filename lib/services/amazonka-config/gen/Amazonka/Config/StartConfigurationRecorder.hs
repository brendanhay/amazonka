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
-- Module      : Amazonka.Config.StartConfigurationRecorder
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts recording configurations of the Amazon Web Services resources you
-- have selected to record in your Amazon Web Services account.
--
-- You must have created at least one delivery channel to successfully
-- start the configuration recorder.
module Amazonka.Config.StartConfigurationRecorder
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

import Amazonka.Config.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The input for the StartConfigurationRecorder action.
--
-- /See:/ 'newStartConfigurationRecorder' smart constructor.
data StartConfigurationRecorder = StartConfigurationRecorder'
  { -- | The name of the recorder object that records each configuration change
    -- made to the resources.
    configurationRecorderName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest StartConfigurationRecorder where
  type
    AWSResponse StartConfigurationRecorder =
      StartConfigurationRecorderResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      StartConfigurationRecorderResponse'

instance Prelude.Hashable StartConfigurationRecorder where
  hashWithSalt _salt StartConfigurationRecorder' {..} =
    _salt
      `Prelude.hashWithSalt` configurationRecorderName

instance Prelude.NFData StartConfigurationRecorder where
  rnf StartConfigurationRecorder' {..} =
    Prelude.rnf configurationRecorderName

instance Data.ToHeaders StartConfigurationRecorder where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StarlingDoveService.StartConfigurationRecorder" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartConfigurationRecorder where
  toJSON StartConfigurationRecorder' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "ConfigurationRecorderName"
                  Data..= configurationRecorderName
              )
          ]
      )

instance Data.ToPath StartConfigurationRecorder where
  toPath = Prelude.const "/"

instance Data.ToQuery StartConfigurationRecorder where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartConfigurationRecorderResponse' smart constructor.
data StartConfigurationRecorderResponse = StartConfigurationRecorderResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf _ = ()
