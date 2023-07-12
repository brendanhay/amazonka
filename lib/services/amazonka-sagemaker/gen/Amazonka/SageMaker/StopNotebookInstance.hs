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
-- Module      : Amazonka.SageMaker.StopNotebookInstance
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Terminates the ML compute instance. Before terminating the instance,
-- SageMaker disconnects the ML storage volume from it. SageMaker preserves
-- the ML storage volume. SageMaker stops charging you for the ML compute
-- instance when you call @StopNotebookInstance@.
--
-- To access data on the ML storage volume for a notebook instance that has
-- been terminated, call the @StartNotebookInstance@ API.
-- @StartNotebookInstance@ launches another ML compute instance, configures
-- it, and attaches the preserved ML storage volume so you can continue
-- your work.
module Amazonka.SageMaker.StopNotebookInstance
  ( -- * Creating a Request
    StopNotebookInstance (..),
    newStopNotebookInstance,

    -- * Request Lenses
    stopNotebookInstance_notebookInstanceName,

    -- * Destructuring the Response
    StopNotebookInstanceResponse (..),
    newStopNotebookInstanceResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newStopNotebookInstance' smart constructor.
data StopNotebookInstance = StopNotebookInstance'
  { -- | The name of the notebook instance to terminate.
    notebookInstanceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopNotebookInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'notebookInstanceName', 'stopNotebookInstance_notebookInstanceName' - The name of the notebook instance to terminate.
newStopNotebookInstance ::
  -- | 'notebookInstanceName'
  Prelude.Text ->
  StopNotebookInstance
newStopNotebookInstance pNotebookInstanceName_ =
  StopNotebookInstance'
    { notebookInstanceName =
        pNotebookInstanceName_
    }

-- | The name of the notebook instance to terminate.
stopNotebookInstance_notebookInstanceName :: Lens.Lens' StopNotebookInstance Prelude.Text
stopNotebookInstance_notebookInstanceName = Lens.lens (\StopNotebookInstance' {notebookInstanceName} -> notebookInstanceName) (\s@StopNotebookInstance' {} a -> s {notebookInstanceName = a} :: StopNotebookInstance)

instance Core.AWSRequest StopNotebookInstance where
  type
    AWSResponse StopNotebookInstance =
      StopNotebookInstanceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull StopNotebookInstanceResponse'

instance Prelude.Hashable StopNotebookInstance where
  hashWithSalt _salt StopNotebookInstance' {..} =
    _salt `Prelude.hashWithSalt` notebookInstanceName

instance Prelude.NFData StopNotebookInstance where
  rnf StopNotebookInstance' {..} =
    Prelude.rnf notebookInstanceName

instance Data.ToHeaders StopNotebookInstance where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.StopNotebookInstance" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StopNotebookInstance where
  toJSON StopNotebookInstance' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "NotebookInstanceName"
                  Data..= notebookInstanceName
              )
          ]
      )

instance Data.ToPath StopNotebookInstance where
  toPath = Prelude.const "/"

instance Data.ToQuery StopNotebookInstance where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopNotebookInstanceResponse' smart constructor.
data StopNotebookInstanceResponse = StopNotebookInstanceResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopNotebookInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newStopNotebookInstanceResponse ::
  StopNotebookInstanceResponse
newStopNotebookInstanceResponse =
  StopNotebookInstanceResponse'

instance Prelude.NFData StopNotebookInstanceResponse where
  rnf _ = ()
