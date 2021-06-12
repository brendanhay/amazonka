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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newDeleteNotebookInstance' smart constructor.
data DeleteNotebookInstance = DeleteNotebookInstance'
  { -- | The name of the Amazon SageMaker notebook instance to delete.
    notebookInstanceName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  DeleteNotebookInstance
newDeleteNotebookInstance pNotebookInstanceName_ =
  DeleteNotebookInstance'
    { notebookInstanceName =
        pNotebookInstanceName_
    }

-- | The name of the Amazon SageMaker notebook instance to delete.
deleteNotebookInstance_notebookInstanceName :: Lens.Lens' DeleteNotebookInstance Core.Text
deleteNotebookInstance_notebookInstanceName = Lens.lens (\DeleteNotebookInstance' {notebookInstanceName} -> notebookInstanceName) (\s@DeleteNotebookInstance' {} a -> s {notebookInstanceName = a} :: DeleteNotebookInstance)

instance Core.AWSRequest DeleteNotebookInstance where
  type
    AWSResponse DeleteNotebookInstance =
      DeleteNotebookInstanceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      DeleteNotebookInstanceResponse'

instance Core.Hashable DeleteNotebookInstance

instance Core.NFData DeleteNotebookInstance

instance Core.ToHeaders DeleteNotebookInstance where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.DeleteNotebookInstance" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteNotebookInstance where
  toJSON DeleteNotebookInstance' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "NotebookInstanceName"
                  Core..= notebookInstanceName
              )
          ]
      )

instance Core.ToPath DeleteNotebookInstance where
  toPath = Core.const "/"

instance Core.ToQuery DeleteNotebookInstance where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteNotebookInstanceResponse' smart constructor.
data DeleteNotebookInstanceResponse = DeleteNotebookInstanceResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteNotebookInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteNotebookInstanceResponse ::
  DeleteNotebookInstanceResponse
newDeleteNotebookInstanceResponse =
  DeleteNotebookInstanceResponse'

instance Core.NFData DeleteNotebookInstanceResponse
