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
-- Module      : Network.AWS.SageMaker.DeleteNotebookInstanceLifecycleConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a notebook instance lifecycle configuration.
module Network.AWS.SageMaker.DeleteNotebookInstanceLifecycleConfig
  ( -- * Creating a Request
    DeleteNotebookInstanceLifecycleConfig (..),
    newDeleteNotebookInstanceLifecycleConfig,

    -- * Request Lenses
    deleteNotebookInstanceLifecycleConfig_notebookInstanceLifecycleConfigName,

    -- * Destructuring the Response
    DeleteNotebookInstanceLifecycleConfigResponse (..),
    newDeleteNotebookInstanceLifecycleConfigResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newDeleteNotebookInstanceLifecycleConfig' smart constructor.
data DeleteNotebookInstanceLifecycleConfig = DeleteNotebookInstanceLifecycleConfig'
  { -- | The name of the lifecycle configuration to delete.
    notebookInstanceLifecycleConfigName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteNotebookInstanceLifecycleConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'notebookInstanceLifecycleConfigName', 'deleteNotebookInstanceLifecycleConfig_notebookInstanceLifecycleConfigName' - The name of the lifecycle configuration to delete.
newDeleteNotebookInstanceLifecycleConfig ::
  -- | 'notebookInstanceLifecycleConfigName'
  Prelude.Text ->
  DeleteNotebookInstanceLifecycleConfig
newDeleteNotebookInstanceLifecycleConfig
  pNotebookInstanceLifecycleConfigName_ =
    DeleteNotebookInstanceLifecycleConfig'
      { notebookInstanceLifecycleConfigName =
          pNotebookInstanceLifecycleConfigName_
      }

-- | The name of the lifecycle configuration to delete.
deleteNotebookInstanceLifecycleConfig_notebookInstanceLifecycleConfigName :: Lens.Lens' DeleteNotebookInstanceLifecycleConfig Prelude.Text
deleteNotebookInstanceLifecycleConfig_notebookInstanceLifecycleConfigName = Lens.lens (\DeleteNotebookInstanceLifecycleConfig' {notebookInstanceLifecycleConfigName} -> notebookInstanceLifecycleConfigName) (\s@DeleteNotebookInstanceLifecycleConfig' {} a -> s {notebookInstanceLifecycleConfigName = a} :: DeleteNotebookInstanceLifecycleConfig)

instance
  Prelude.AWSRequest
    DeleteNotebookInstanceLifecycleConfig
  where
  type
    Rs DeleteNotebookInstanceLifecycleConfig =
      DeleteNotebookInstanceLifecycleConfigResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      DeleteNotebookInstanceLifecycleConfigResponse'

instance
  Prelude.Hashable
    DeleteNotebookInstanceLifecycleConfig

instance
  Prelude.NFData
    DeleteNotebookInstanceLifecycleConfig

instance
  Prelude.ToHeaders
    DeleteNotebookInstanceLifecycleConfig
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "SageMaker.DeleteNotebookInstanceLifecycleConfig" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance
  Prelude.ToJSON
    DeleteNotebookInstanceLifecycleConfig
  where
  toJSON DeleteNotebookInstanceLifecycleConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "NotebookInstanceLifecycleConfigName"
                  Prelude..= notebookInstanceLifecycleConfigName
              )
          ]
      )

instance
  Prelude.ToPath
    DeleteNotebookInstanceLifecycleConfig
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    DeleteNotebookInstanceLifecycleConfig
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteNotebookInstanceLifecycleConfigResponse' smart constructor.
data DeleteNotebookInstanceLifecycleConfigResponse = DeleteNotebookInstanceLifecycleConfigResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteNotebookInstanceLifecycleConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteNotebookInstanceLifecycleConfigResponse ::
  DeleteNotebookInstanceLifecycleConfigResponse
newDeleteNotebookInstanceLifecycleConfigResponse =
  DeleteNotebookInstanceLifecycleConfigResponse'

instance
  Prelude.NFData
    DeleteNotebookInstanceLifecycleConfigResponse
