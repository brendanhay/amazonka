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
-- Module      : Network.AWS.SageMaker.DeleteNotebookInstance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an Amazon SageMaker notebook instance. Before you can delete a
-- notebook instance, you must call the @StopNotebookInstance@ API.
--
-- When you delete a notebook instance, you lose all of your data. Amazon
-- SageMaker removes the ML compute instance, and deletes the ML storage
-- volume and the network interface associated with the notebook instance.
module Network.AWS.SageMaker.DeleteNotebookInstance
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newDeleteNotebookInstance' smart constructor.
data DeleteNotebookInstance = DeleteNotebookInstance'
  { -- | The name of the Amazon SageMaker notebook instance to delete.
    notebookInstanceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteNotebookInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'notebookInstanceName', 'deleteNotebookInstance_notebookInstanceName' - The name of the Amazon SageMaker notebook instance to delete.
newDeleteNotebookInstance ::
  -- | 'notebookInstanceName'
  Prelude.Text ->
  DeleteNotebookInstance
newDeleteNotebookInstance pNotebookInstanceName_ =
  DeleteNotebookInstance'
    { notebookInstanceName =
        pNotebookInstanceName_
    }

-- | The name of the Amazon SageMaker notebook instance to delete.
deleteNotebookInstance_notebookInstanceName :: Lens.Lens' DeleteNotebookInstance Prelude.Text
deleteNotebookInstance_notebookInstanceName = Lens.lens (\DeleteNotebookInstance' {notebookInstanceName} -> notebookInstanceName) (\s@DeleteNotebookInstance' {} a -> s {notebookInstanceName = a} :: DeleteNotebookInstance)

instance Prelude.AWSRequest DeleteNotebookInstance where
  type
    Rs DeleteNotebookInstance =
      DeleteNotebookInstanceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      DeleteNotebookInstanceResponse'

instance Prelude.Hashable DeleteNotebookInstance

instance Prelude.NFData DeleteNotebookInstance

instance Prelude.ToHeaders DeleteNotebookInstance where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "SageMaker.DeleteNotebookInstance" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteNotebookInstance where
  toJSON DeleteNotebookInstance' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "NotebookInstanceName"
                  Prelude..= notebookInstanceName
              )
          ]
      )

instance Prelude.ToPath DeleteNotebookInstance where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteNotebookInstance where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteNotebookInstanceResponse' smart constructor.
data DeleteNotebookInstanceResponse = DeleteNotebookInstanceResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
