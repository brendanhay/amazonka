{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DeleteApp
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Used to stop and delete an app.
module Network.AWS.SageMaker.DeleteApp
  ( -- * Creating a Request
    deleteApp,
    DeleteApp,

    -- * Request Lenses
    dDomainId,
    dUserProfileName,
    dAppType,
    dAppName,

    -- * Destructuring the Response
    deleteAppResponse,
    DeleteAppResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'deleteApp' smart constructor.
data DeleteApp = DeleteApp'
  { _dDomainId :: !Text,
    _dUserProfileName :: !Text,
    _dAppType :: !AppType,
    _dAppName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteApp' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dDomainId' - The domain ID.
--
-- * 'dUserProfileName' - The user profile name.
--
-- * 'dAppType' - The type of app.
--
-- * 'dAppName' - The name of the app.
deleteApp ::
  -- | 'dDomainId'
  Text ->
  -- | 'dUserProfileName'
  Text ->
  -- | 'dAppType'
  AppType ->
  -- | 'dAppName'
  Text ->
  DeleteApp
deleteApp pDomainId_ pUserProfileName_ pAppType_ pAppName_ =
  DeleteApp'
    { _dDomainId = pDomainId_,
      _dUserProfileName = pUserProfileName_,
      _dAppType = pAppType_,
      _dAppName = pAppName_
    }

-- | The domain ID.
dDomainId :: Lens' DeleteApp Text
dDomainId = lens _dDomainId (\s a -> s {_dDomainId = a})

-- | The user profile name.
dUserProfileName :: Lens' DeleteApp Text
dUserProfileName = lens _dUserProfileName (\s a -> s {_dUserProfileName = a})

-- | The type of app.
dAppType :: Lens' DeleteApp AppType
dAppType = lens _dAppType (\s a -> s {_dAppType = a})

-- | The name of the app.
dAppName :: Lens' DeleteApp Text
dAppName = lens _dAppName (\s a -> s {_dAppName = a})

instance AWSRequest DeleteApp where
  type Rs DeleteApp = DeleteAppResponse
  request = postJSON sageMaker
  response = receiveNull DeleteAppResponse'

instance Hashable DeleteApp

instance NFData DeleteApp

instance ToHeaders DeleteApp where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("SageMaker.DeleteApp" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeleteApp where
  toJSON DeleteApp' {..} =
    object
      ( catMaybes
          [ Just ("DomainId" .= _dDomainId),
            Just ("UserProfileName" .= _dUserProfileName),
            Just ("AppType" .= _dAppType),
            Just ("AppName" .= _dAppName)
          ]
      )

instance ToPath DeleteApp where
  toPath = const "/"

instance ToQuery DeleteApp where
  toQuery = const mempty

-- | /See:/ 'deleteAppResponse' smart constructor.
data DeleteAppResponse = DeleteAppResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteAppResponse' with the minimum fields required to make a request.
deleteAppResponse ::
  DeleteAppResponse
deleteAppResponse = DeleteAppResponse'

instance NFData DeleteAppResponse
