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
-- Module      : Amazonka.SageMaker.DeleteAppImageConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an AppImageConfig.
module Amazonka.SageMaker.DeleteAppImageConfig
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newDeleteAppImageConfig' smart constructor.
data DeleteAppImageConfig = DeleteAppImageConfig'
  { -- | The name of the AppImageConfig to delete.
    appImageConfigName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest DeleteAppImageConfig where
  type
    AWSResponse DeleteAppImageConfig =
      DeleteAppImageConfigResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull DeleteAppImageConfigResponse'

instance Prelude.Hashable DeleteAppImageConfig where
  hashWithSalt _salt DeleteAppImageConfig' {..} =
    _salt `Prelude.hashWithSalt` appImageConfigName

instance Prelude.NFData DeleteAppImageConfig where
  rnf DeleteAppImageConfig' {..} =
    Prelude.rnf appImageConfigName

instance Core.ToHeaders DeleteAppImageConfig where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.DeleteAppImageConfig" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteAppImageConfig where
  toJSON DeleteAppImageConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("AppImageConfigName" Core..= appImageConfigName)
          ]
      )

instance Core.ToPath DeleteAppImageConfig where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteAppImageConfig where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteAppImageConfigResponse' smart constructor.
data DeleteAppImageConfigResponse = DeleteAppImageConfigResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAppImageConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteAppImageConfigResponse ::
  DeleteAppImageConfigResponse
newDeleteAppImageConfigResponse =
  DeleteAppImageConfigResponse'

instance Prelude.NFData DeleteAppImageConfigResponse where
  rnf _ = ()
