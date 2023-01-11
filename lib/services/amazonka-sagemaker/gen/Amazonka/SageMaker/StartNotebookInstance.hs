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
-- Module      : Amazonka.SageMaker.StartNotebookInstance
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Launches an ML compute instance with the latest version of the libraries
-- and attaches your ML storage volume. After configuring the notebook
-- instance, SageMaker sets the notebook instance status to @InService@. A
-- notebook instance\'s status must be @InService@ before you can connect
-- to your Jupyter notebook.
module Amazonka.SageMaker.StartNotebookInstance
  ( -- * Creating a Request
    StartNotebookInstance (..),
    newStartNotebookInstance,

    -- * Request Lenses
    startNotebookInstance_notebookInstanceName,

    -- * Destructuring the Response
    StartNotebookInstanceResponse (..),
    newStartNotebookInstanceResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newStartNotebookInstance' smart constructor.
data StartNotebookInstance = StartNotebookInstance'
  { -- | The name of the notebook instance to start.
    notebookInstanceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartNotebookInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'notebookInstanceName', 'startNotebookInstance_notebookInstanceName' - The name of the notebook instance to start.
newStartNotebookInstance ::
  -- | 'notebookInstanceName'
  Prelude.Text ->
  StartNotebookInstance
newStartNotebookInstance pNotebookInstanceName_ =
  StartNotebookInstance'
    { notebookInstanceName =
        pNotebookInstanceName_
    }

-- | The name of the notebook instance to start.
startNotebookInstance_notebookInstanceName :: Lens.Lens' StartNotebookInstance Prelude.Text
startNotebookInstance_notebookInstanceName = Lens.lens (\StartNotebookInstance' {notebookInstanceName} -> notebookInstanceName) (\s@StartNotebookInstance' {} a -> s {notebookInstanceName = a} :: StartNotebookInstance)

instance Core.AWSRequest StartNotebookInstance where
  type
    AWSResponse StartNotebookInstance =
      StartNotebookInstanceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull StartNotebookInstanceResponse'

instance Prelude.Hashable StartNotebookInstance where
  hashWithSalt _salt StartNotebookInstance' {..} =
    _salt `Prelude.hashWithSalt` notebookInstanceName

instance Prelude.NFData StartNotebookInstance where
  rnf StartNotebookInstance' {..} =
    Prelude.rnf notebookInstanceName

instance Data.ToHeaders StartNotebookInstance where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.StartNotebookInstance" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartNotebookInstance where
  toJSON StartNotebookInstance' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "NotebookInstanceName"
                  Data..= notebookInstanceName
              )
          ]
      )

instance Data.ToPath StartNotebookInstance where
  toPath = Prelude.const "/"

instance Data.ToQuery StartNotebookInstance where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartNotebookInstanceResponse' smart constructor.
data StartNotebookInstanceResponse = StartNotebookInstanceResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartNotebookInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newStartNotebookInstanceResponse ::
  StartNotebookInstanceResponse
newStartNotebookInstanceResponse =
  StartNotebookInstanceResponse'

instance Prelude.NFData StartNotebookInstanceResponse where
  rnf _ = ()
