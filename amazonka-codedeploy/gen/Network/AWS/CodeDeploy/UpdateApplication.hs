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
-- Module      : Network.AWS.CodeDeploy.UpdateApplication
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the name of an application.
module Network.AWS.CodeDeploy.UpdateApplication
  ( -- * Creating a Request
    UpdateApplication (..),
    newUpdateApplication,

    -- * Request Lenses
    updateApplication_newApplicationName,
    updateApplication_applicationName,

    -- * Destructuring the Response
    UpdateApplicationResponse (..),
    newUpdateApplicationResponse,
  )
where

import Network.AWS.CodeDeploy.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of an @UpdateApplication@ operation.
--
-- /See:/ 'newUpdateApplication' smart constructor.
data UpdateApplication = UpdateApplication'
  { -- | The new name to give the application.
    newApplicationName' :: Prelude.Maybe Prelude.Text,
    -- | The current name of the application you want to change.
    applicationName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateApplication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'newApplicationName'', 'updateApplication_newApplicationName' - The new name to give the application.
--
-- 'applicationName', 'updateApplication_applicationName' - The current name of the application you want to change.
newUpdateApplication ::
  UpdateApplication
newUpdateApplication =
  UpdateApplication'
    { newApplicationName' =
        Prelude.Nothing,
      applicationName = Prelude.Nothing
    }

-- | The new name to give the application.
updateApplication_newApplicationName :: Lens.Lens' UpdateApplication (Prelude.Maybe Prelude.Text)
updateApplication_newApplicationName = Lens.lens (\UpdateApplication' {newApplicationName'} -> newApplicationName') (\s@UpdateApplication' {} a -> s {newApplicationName' = a} :: UpdateApplication)

-- | The current name of the application you want to change.
updateApplication_applicationName :: Lens.Lens' UpdateApplication (Prelude.Maybe Prelude.Text)
updateApplication_applicationName = Lens.lens (\UpdateApplication' {applicationName} -> applicationName) (\s@UpdateApplication' {} a -> s {applicationName = a} :: UpdateApplication)

instance Prelude.AWSRequest UpdateApplication where
  type Rs UpdateApplication = UpdateApplicationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull UpdateApplicationResponse'

instance Prelude.Hashable UpdateApplication

instance Prelude.NFData UpdateApplication

instance Prelude.ToHeaders UpdateApplication where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "CodeDeploy_20141006.UpdateApplication" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateApplication where
  toJSON UpdateApplication' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("newApplicationName" Prelude..=)
              Prelude.<$> newApplicationName',
            ("applicationName" Prelude..=)
              Prelude.<$> applicationName
          ]
      )

instance Prelude.ToPath UpdateApplication where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UpdateApplication where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateApplicationResponse' smart constructor.
data UpdateApplicationResponse = UpdateApplicationResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateApplicationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateApplicationResponse ::
  UpdateApplicationResponse
newUpdateApplicationResponse =
  UpdateApplicationResponse'

instance Prelude.NFData UpdateApplicationResponse
