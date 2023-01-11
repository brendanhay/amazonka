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
-- Module      : Amazonka.SageMaker.DeleteStudioLifecycleConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the Studio Lifecycle Configuration. In order to delete the
-- Lifecycle Configuration, there must be no running apps using the
-- Lifecycle Configuration. You must also remove the Lifecycle
-- Configuration from UserSettings in all Domains and UserProfiles.
module Amazonka.SageMaker.DeleteStudioLifecycleConfig
  ( -- * Creating a Request
    DeleteStudioLifecycleConfig (..),
    newDeleteStudioLifecycleConfig,

    -- * Request Lenses
    deleteStudioLifecycleConfig_studioLifecycleConfigName,

    -- * Destructuring the Response
    DeleteStudioLifecycleConfigResponse (..),
    newDeleteStudioLifecycleConfigResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newDeleteStudioLifecycleConfig' smart constructor.
data DeleteStudioLifecycleConfig = DeleteStudioLifecycleConfig'
  { -- | The name of the Studio Lifecycle Configuration to delete.
    studioLifecycleConfigName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteStudioLifecycleConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'studioLifecycleConfigName', 'deleteStudioLifecycleConfig_studioLifecycleConfigName' - The name of the Studio Lifecycle Configuration to delete.
newDeleteStudioLifecycleConfig ::
  -- | 'studioLifecycleConfigName'
  Prelude.Text ->
  DeleteStudioLifecycleConfig
newDeleteStudioLifecycleConfig
  pStudioLifecycleConfigName_ =
    DeleteStudioLifecycleConfig'
      { studioLifecycleConfigName =
          pStudioLifecycleConfigName_
      }

-- | The name of the Studio Lifecycle Configuration to delete.
deleteStudioLifecycleConfig_studioLifecycleConfigName :: Lens.Lens' DeleteStudioLifecycleConfig Prelude.Text
deleteStudioLifecycleConfig_studioLifecycleConfigName = Lens.lens (\DeleteStudioLifecycleConfig' {studioLifecycleConfigName} -> studioLifecycleConfigName) (\s@DeleteStudioLifecycleConfig' {} a -> s {studioLifecycleConfigName = a} :: DeleteStudioLifecycleConfig)

instance Core.AWSRequest DeleteStudioLifecycleConfig where
  type
    AWSResponse DeleteStudioLifecycleConfig =
      DeleteStudioLifecycleConfigResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      DeleteStudioLifecycleConfigResponse'

instance Prelude.Hashable DeleteStudioLifecycleConfig where
  hashWithSalt _salt DeleteStudioLifecycleConfig' {..} =
    _salt
      `Prelude.hashWithSalt` studioLifecycleConfigName

instance Prelude.NFData DeleteStudioLifecycleConfig where
  rnf DeleteStudioLifecycleConfig' {..} =
    Prelude.rnf studioLifecycleConfigName

instance Data.ToHeaders DeleteStudioLifecycleConfig where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.DeleteStudioLifecycleConfig" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteStudioLifecycleConfig where
  toJSON DeleteStudioLifecycleConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "StudioLifecycleConfigName"
                  Data..= studioLifecycleConfigName
              )
          ]
      )

instance Data.ToPath DeleteStudioLifecycleConfig where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteStudioLifecycleConfig where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteStudioLifecycleConfigResponse' smart constructor.
data DeleteStudioLifecycleConfigResponse = DeleteStudioLifecycleConfigResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteStudioLifecycleConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteStudioLifecycleConfigResponse ::
  DeleteStudioLifecycleConfigResponse
newDeleteStudioLifecycleConfigResponse =
  DeleteStudioLifecycleConfigResponse'

instance
  Prelude.NFData
    DeleteStudioLifecycleConfigResponse
  where
  rnf _ = ()
