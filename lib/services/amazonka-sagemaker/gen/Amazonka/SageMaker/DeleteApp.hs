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
-- Module      : Amazonka.SageMaker.DeleteApp
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Used to stop and delete an app.
module Amazonka.SageMaker.DeleteApp
  ( -- * Creating a Request
    DeleteApp (..),
    newDeleteApp,

    -- * Request Lenses
    deleteApp_spaceName,
    deleteApp_userProfileName,
    deleteApp_domainId,
    deleteApp_appType,
    deleteApp_appName,

    -- * Destructuring the Response
    DeleteAppResponse (..),
    newDeleteAppResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newDeleteApp' smart constructor.
data DeleteApp = DeleteApp'
  { -- | The name of the space. If this value is not set, then @UserProfileName@
    -- must be set.
    spaceName :: Prelude.Maybe Prelude.Text,
    -- | The user profile name. If this value is not set, then @SpaceName@ must
    -- be set.
    userProfileName :: Prelude.Maybe Prelude.Text,
    -- | The domain ID.
    domainId :: Prelude.Text,
    -- | The type of app.
    appType :: AppType,
    -- | The name of the app.
    appName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteApp' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'spaceName', 'deleteApp_spaceName' - The name of the space. If this value is not set, then @UserProfileName@
-- must be set.
--
-- 'userProfileName', 'deleteApp_userProfileName' - The user profile name. If this value is not set, then @SpaceName@ must
-- be set.
--
-- 'domainId', 'deleteApp_domainId' - The domain ID.
--
-- 'appType', 'deleteApp_appType' - The type of app.
--
-- 'appName', 'deleteApp_appName' - The name of the app.
newDeleteApp ::
  -- | 'domainId'
  Prelude.Text ->
  -- | 'appType'
  AppType ->
  -- | 'appName'
  Prelude.Text ->
  DeleteApp
newDeleteApp pDomainId_ pAppType_ pAppName_ =
  DeleteApp'
    { spaceName = Prelude.Nothing,
      userProfileName = Prelude.Nothing,
      domainId = pDomainId_,
      appType = pAppType_,
      appName = pAppName_
    }

-- | The name of the space. If this value is not set, then @UserProfileName@
-- must be set.
deleteApp_spaceName :: Lens.Lens' DeleteApp (Prelude.Maybe Prelude.Text)
deleteApp_spaceName = Lens.lens (\DeleteApp' {spaceName} -> spaceName) (\s@DeleteApp' {} a -> s {spaceName = a} :: DeleteApp)

-- | The user profile name. If this value is not set, then @SpaceName@ must
-- be set.
deleteApp_userProfileName :: Lens.Lens' DeleteApp (Prelude.Maybe Prelude.Text)
deleteApp_userProfileName = Lens.lens (\DeleteApp' {userProfileName} -> userProfileName) (\s@DeleteApp' {} a -> s {userProfileName = a} :: DeleteApp)

-- | The domain ID.
deleteApp_domainId :: Lens.Lens' DeleteApp Prelude.Text
deleteApp_domainId = Lens.lens (\DeleteApp' {domainId} -> domainId) (\s@DeleteApp' {} a -> s {domainId = a} :: DeleteApp)

-- | The type of app.
deleteApp_appType :: Lens.Lens' DeleteApp AppType
deleteApp_appType = Lens.lens (\DeleteApp' {appType} -> appType) (\s@DeleteApp' {} a -> s {appType = a} :: DeleteApp)

-- | The name of the app.
deleteApp_appName :: Lens.Lens' DeleteApp Prelude.Text
deleteApp_appName = Lens.lens (\DeleteApp' {appName} -> appName) (\s@DeleteApp' {} a -> s {appName = a} :: DeleteApp)

instance Core.AWSRequest DeleteApp where
  type AWSResponse DeleteApp = DeleteAppResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response = Response.receiveNull DeleteAppResponse'

instance Prelude.Hashable DeleteApp where
  hashWithSalt _salt DeleteApp' {..} =
    _salt `Prelude.hashWithSalt` spaceName
      `Prelude.hashWithSalt` userProfileName
      `Prelude.hashWithSalt` domainId
      `Prelude.hashWithSalt` appType
      `Prelude.hashWithSalt` appName

instance Prelude.NFData DeleteApp where
  rnf DeleteApp' {..} =
    Prelude.rnf spaceName
      `Prelude.seq` Prelude.rnf userProfileName
      `Prelude.seq` Prelude.rnf domainId
      `Prelude.seq` Prelude.rnf appType
      `Prelude.seq` Prelude.rnf appName

instance Data.ToHeaders DeleteApp where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("SageMaker.DeleteApp" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteApp where
  toJSON DeleteApp' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("SpaceName" Data..=) Prelude.<$> spaceName,
            ("UserProfileName" Data..=)
              Prelude.<$> userProfileName,
            Prelude.Just ("DomainId" Data..= domainId),
            Prelude.Just ("AppType" Data..= appType),
            Prelude.Just ("AppName" Data..= appName)
          ]
      )

instance Data.ToPath DeleteApp where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteApp where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteAppResponse' smart constructor.
data DeleteAppResponse = DeleteAppResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAppResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteAppResponse ::
  DeleteAppResponse
newDeleteAppResponse = DeleteAppResponse'

instance Prelude.NFData DeleteAppResponse where
  rnf _ = ()
