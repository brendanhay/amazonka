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
-- Module      : Amazonka.SageMaker.DeleteNotebookInstance
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an SageMaker notebook instance. Before you can delete a notebook
-- instance, you must call the @StopNotebookInstance@ API.
--
-- When you delete a notebook instance, you lose all of your data.
-- SageMaker removes the ML compute instance, and deletes the ML storage
-- volume and the network interface associated with the notebook instance.
module Amazonka.SageMaker.DeleteNotebookInstance
  ( -- * Creating a Request
    DeleteNotebookInstance (..),
    newDeleteNotebookInstance,

    -- * Request Lenses
    deleteNotebookInstance_notebookInstanceName,

    -- * Destructuring the Response
    DeleteNotebookInstanceResponse (..),
    newDeleteNotebookInstanceResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newDeleteNotebookInstance' smart constructor.
data DeleteNotebookInstance = DeleteNotebookInstance'
  { -- | The name of the SageMaker notebook instance to delete.
    notebookInstanceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteNotebookInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'notebookInstanceName', 'deleteNotebookInstance_notebookInstanceName' - The name of the SageMaker notebook instance to delete.
newDeleteNotebookInstance ::
  -- | 'notebookInstanceName'
  Prelude.Text ->
  DeleteNotebookInstance
newDeleteNotebookInstance pNotebookInstanceName_ =
  DeleteNotebookInstance'
    { notebookInstanceName =
        pNotebookInstanceName_
    }

-- | The name of the SageMaker notebook instance to delete.
deleteNotebookInstance_notebookInstanceName :: Lens.Lens' DeleteNotebookInstance Prelude.Text
deleteNotebookInstance_notebookInstanceName = Lens.lens (\DeleteNotebookInstance' {notebookInstanceName} -> notebookInstanceName) (\s@DeleteNotebookInstance' {} a -> s {notebookInstanceName = a} :: DeleteNotebookInstance)

instance Core.AWSRequest DeleteNotebookInstance where
  type
    AWSResponse DeleteNotebookInstance =
      DeleteNotebookInstanceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      DeleteNotebookInstanceResponse'

instance Prelude.Hashable DeleteNotebookInstance where
  hashWithSalt _salt DeleteNotebookInstance' {..} =
    _salt `Prelude.hashWithSalt` notebookInstanceName

instance Prelude.NFData DeleteNotebookInstance where
  rnf DeleteNotebookInstance' {..} =
    Prelude.rnf notebookInstanceName

instance Data.ToHeaders DeleteNotebookInstance where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.DeleteNotebookInstance" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteNotebookInstance where
  toJSON DeleteNotebookInstance' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "NotebookInstanceName"
                  Data..= notebookInstanceName
              )
          ]
      )

instance Data.ToPath DeleteNotebookInstance where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteNotebookInstance where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteNotebookInstanceResponse' smart constructor.
data DeleteNotebookInstanceResponse = DeleteNotebookInstanceResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteNotebookInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteNotebookInstanceResponse ::
  DeleteNotebookInstanceResponse
newDeleteNotebookInstanceResponse =
  DeleteNotebookInstanceResponse'

instance
  Prelude.NFData
    DeleteNotebookInstanceResponse
  where
  rnf _ = ()
