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
-- Module      : Network.AWS.SageMaker.DeleteApp
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Used to stop and delete an app.
module Network.AWS.SageMaker.DeleteApp
  ( -- * Creating a Request
    DeleteApp (..),
    newDeleteApp,

    -- * Request Lenses
    deleteApp_domainId,
    deleteApp_userProfileName,
    deleteApp_appType,
    deleteApp_appName,

    -- * Destructuring the Response
    DeleteAppResponse (..),
    newDeleteAppResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newDeleteApp' smart constructor.
data DeleteApp = DeleteApp'
  { -- | The domain ID.
    domainId :: Prelude.Text,
    -- | The user profile name.
    userProfileName :: Prelude.Text,
    -- | The type of app.
    appType :: AppType,
    -- | The name of the app.
    appName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteApp' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainId', 'deleteApp_domainId' - The domain ID.
--
-- 'userProfileName', 'deleteApp_userProfileName' - The user profile name.
--
-- 'appType', 'deleteApp_appType' - The type of app.
--
-- 'appName', 'deleteApp_appName' - The name of the app.
newDeleteApp ::
  -- | 'domainId'
  Prelude.Text ->
  -- | 'userProfileName'
  Prelude.Text ->
  -- | 'appType'
  AppType ->
  -- | 'appName'
  Prelude.Text ->
  DeleteApp
newDeleteApp
  pDomainId_
  pUserProfileName_
  pAppType_
  pAppName_ =
    DeleteApp'
      { domainId = pDomainId_,
        userProfileName = pUserProfileName_,
        appType = pAppType_,
        appName = pAppName_
      }

-- | The domain ID.
deleteApp_domainId :: Lens.Lens' DeleteApp Prelude.Text
deleteApp_domainId = Lens.lens (\DeleteApp' {domainId} -> domainId) (\s@DeleteApp' {} a -> s {domainId = a} :: DeleteApp)

-- | The user profile name.
deleteApp_userProfileName :: Lens.Lens' DeleteApp Prelude.Text
deleteApp_userProfileName = Lens.lens (\DeleteApp' {userProfileName} -> userProfileName) (\s@DeleteApp' {} a -> s {userProfileName = a} :: DeleteApp)

-- | The type of app.
deleteApp_appType :: Lens.Lens' DeleteApp AppType
deleteApp_appType = Lens.lens (\DeleteApp' {appType} -> appType) (\s@DeleteApp' {} a -> s {appType = a} :: DeleteApp)

-- | The name of the app.
deleteApp_appName :: Lens.Lens' DeleteApp Prelude.Text
deleteApp_appName = Lens.lens (\DeleteApp' {appName} -> appName) (\s@DeleteApp' {} a -> s {appName = a} :: DeleteApp)

instance Prelude.AWSRequest DeleteApp where
  type Rs DeleteApp = DeleteAppResponse
  request = Request.postJSON defaultService
  response = Response.receiveNull DeleteAppResponse'

instance Prelude.Hashable DeleteApp

instance Prelude.NFData DeleteApp

instance Prelude.ToHeaders DeleteApp where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("SageMaker.DeleteApp" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteApp where
  toJSON DeleteApp' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("DomainId" Prelude..= domainId),
            Prelude.Just
              ("UserProfileName" Prelude..= userProfileName),
            Prelude.Just ("AppType" Prelude..= appType),
            Prelude.Just ("AppName" Prelude..= appName)
          ]
      )

instance Prelude.ToPath DeleteApp where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteApp where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteAppResponse' smart constructor.
data DeleteAppResponse = DeleteAppResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteAppResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteAppResponse ::
  DeleteAppResponse
newDeleteAppResponse = DeleteAppResponse'

instance Prelude.NFData DeleteAppResponse
