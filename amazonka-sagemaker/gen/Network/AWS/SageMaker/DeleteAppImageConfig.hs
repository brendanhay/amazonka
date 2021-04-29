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
-- Module      : Network.AWS.SageMaker.DeleteAppImageConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an AppImageConfig.
module Network.AWS.SageMaker.DeleteAppImageConfig
  ( -- * Creating a Request
    DeleteAppImageConfig (..),
    newDeleteAppImageConfig,

    -- * Request Lenses
    deleteAppImageConfig_appImageConfigName,

    -- * Destructuring the Response
    DeleteAppImageConfigResponse (..),
    newDeleteAppImageConfigResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newDeleteAppImageConfig' smart constructor.
data DeleteAppImageConfig = DeleteAppImageConfig'
  { -- | The name of the AppImageConfig to delete.
    appImageConfigName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteAppImageConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appImageConfigName', 'deleteAppImageConfig_appImageConfigName' - The name of the AppImageConfig to delete.
newDeleteAppImageConfig ::
  -- | 'appImageConfigName'
  Prelude.Text ->
  DeleteAppImageConfig
newDeleteAppImageConfig pAppImageConfigName_ =
  DeleteAppImageConfig'
    { appImageConfigName =
        pAppImageConfigName_
    }

-- | The name of the AppImageConfig to delete.
deleteAppImageConfig_appImageConfigName :: Lens.Lens' DeleteAppImageConfig Prelude.Text
deleteAppImageConfig_appImageConfigName = Lens.lens (\DeleteAppImageConfig' {appImageConfigName} -> appImageConfigName) (\s@DeleteAppImageConfig' {} a -> s {appImageConfigName = a} :: DeleteAppImageConfig)

instance Prelude.AWSRequest DeleteAppImageConfig where
  type
    Rs DeleteAppImageConfig =
      DeleteAppImageConfigResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull DeleteAppImageConfigResponse'

instance Prelude.Hashable DeleteAppImageConfig

instance Prelude.NFData DeleteAppImageConfig

instance Prelude.ToHeaders DeleteAppImageConfig where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "SageMaker.DeleteAppImageConfig" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteAppImageConfig where
  toJSON DeleteAppImageConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "AppImageConfigName"
                  Prelude..= appImageConfigName
              )
          ]
      )

instance Prelude.ToPath DeleteAppImageConfig where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteAppImageConfig where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteAppImageConfigResponse' smart constructor.
data DeleteAppImageConfigResponse = DeleteAppImageConfigResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteAppImageConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteAppImageConfigResponse ::
  DeleteAppImageConfigResponse
newDeleteAppImageConfigResponse =
  DeleteAppImageConfigResponse'

instance Prelude.NFData DeleteAppImageConfigResponse
