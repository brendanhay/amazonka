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
-- Module      : Amazonka.SageMaker.DeleteNotebookInstanceLifecycleConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a notebook instance lifecycle configuration.
module Amazonka.SageMaker.DeleteNotebookInstanceLifecycleConfig
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newDeleteNotebookInstanceLifecycleConfig' smart constructor.
data DeleteNotebookInstanceLifecycleConfig = DeleteNotebookInstanceLifecycleConfig'
  { -- | The name of the lifecycle configuration to delete.
    notebookInstanceLifecycleConfigName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Core.AWSRequest
    DeleteNotebookInstanceLifecycleConfig
  where
  type
    AWSResponse
      DeleteNotebookInstanceLifecycleConfig =
      DeleteNotebookInstanceLifecycleConfigResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      DeleteNotebookInstanceLifecycleConfigResponse'

instance
  Prelude.Hashable
    DeleteNotebookInstanceLifecycleConfig
  where
  hashWithSalt
    _salt
    DeleteNotebookInstanceLifecycleConfig' {..} =
      _salt
        `Prelude.hashWithSalt` notebookInstanceLifecycleConfigName

instance
  Prelude.NFData
    DeleteNotebookInstanceLifecycleConfig
  where
  rnf DeleteNotebookInstanceLifecycleConfig' {..} =
    Prelude.rnf notebookInstanceLifecycleConfigName

instance
  Data.ToHeaders
    DeleteNotebookInstanceLifecycleConfig
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.DeleteNotebookInstanceLifecycleConfig" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    DeleteNotebookInstanceLifecycleConfig
  where
  toJSON DeleteNotebookInstanceLifecycleConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "NotebookInstanceLifecycleConfigName"
                  Data..= notebookInstanceLifecycleConfigName
              )
          ]
      )

instance
  Data.ToPath
    DeleteNotebookInstanceLifecycleConfig
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DeleteNotebookInstanceLifecycleConfig
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteNotebookInstanceLifecycleConfigResponse' smart constructor.
data DeleteNotebookInstanceLifecycleConfigResponse = DeleteNotebookInstanceLifecycleConfigResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf _ = ()
