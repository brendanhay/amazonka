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
-- Module      : Network.AWS.Lightsail.CreateContainerServiceRegistryLogin
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a temporary set of log in credentials that you can use to log in to the Docker process on your local machine. After you're logged in, you can use the native Docker commands to push your local container images to the container image registry of your Amazon Lightsail account so that you can use them with your Lightsail container service. The log in credentials expire 12 hours after they are created, at which point you will need to create a new set of log in credentials.
--
--
-- After you push your container images to the container image registry of your Lightsail account, use the @RegisterContainerImage@ action to register the pushed images to a specific Lightsail container service.
module Network.AWS.Lightsail.CreateContainerServiceRegistryLogin
  ( -- * Creating a Request
    createContainerServiceRegistryLogin,
    CreateContainerServiceRegistryLogin,

    -- * Destructuring the Response
    createContainerServiceRegistryLoginResponse,
    CreateContainerServiceRegistryLoginResponse,

    -- * Response Lenses
    ccsrlrsRegistryLogin,
    ccsrlrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createContainerServiceRegistryLogin' smart constructor.
data CreateContainerServiceRegistryLogin = CreateContainerServiceRegistryLogin'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateContainerServiceRegistryLogin' with the minimum fields required to make a request.
createContainerServiceRegistryLogin ::
  CreateContainerServiceRegistryLogin
createContainerServiceRegistryLogin =
  CreateContainerServiceRegistryLogin'

instance AWSRequest CreateContainerServiceRegistryLogin where
  type
    Rs CreateContainerServiceRegistryLogin =
      CreateContainerServiceRegistryLoginResponse
  request = postJSON lightsail
  response =
    receiveJSON
      ( \s h x ->
          CreateContainerServiceRegistryLoginResponse'
            <$> (x .?> "registryLogin") <*> (pure (fromEnum s))
      )

instance Hashable CreateContainerServiceRegistryLogin

instance NFData CreateContainerServiceRegistryLogin

instance ToHeaders CreateContainerServiceRegistryLogin where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "Lightsail_20161128.CreateContainerServiceRegistryLogin" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateContainerServiceRegistryLogin where
  toJSON = const (Object mempty)

instance ToPath CreateContainerServiceRegistryLogin where
  toPath = const "/"

instance ToQuery CreateContainerServiceRegistryLogin where
  toQuery = const mempty

-- | /See:/ 'createContainerServiceRegistryLoginResponse' smart constructor.
data CreateContainerServiceRegistryLoginResponse = CreateContainerServiceRegistryLoginResponse'
  { _ccsrlrsRegistryLogin ::
      !( Maybe
           ContainerServiceRegistryLogin
       ),
    _ccsrlrsResponseStatus ::
      !Int
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'CreateContainerServiceRegistryLoginResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccsrlrsRegistryLogin' - An object that describes the log in information for the container service registry of your Lightsail account.
--
-- * 'ccsrlrsResponseStatus' - -- | The response status code.
createContainerServiceRegistryLoginResponse ::
  -- | 'ccsrlrsResponseStatus'
  Int ->
  CreateContainerServiceRegistryLoginResponse
createContainerServiceRegistryLoginResponse pResponseStatus_ =
  CreateContainerServiceRegistryLoginResponse'
    { _ccsrlrsRegistryLogin =
        Nothing,
      _ccsrlrsResponseStatus = pResponseStatus_
    }

-- | An object that describes the log in information for the container service registry of your Lightsail account.
ccsrlrsRegistryLogin :: Lens' CreateContainerServiceRegistryLoginResponse (Maybe ContainerServiceRegistryLogin)
ccsrlrsRegistryLogin = lens _ccsrlrsRegistryLogin (\s a -> s {_ccsrlrsRegistryLogin = a})

-- | -- | The response status code.
ccsrlrsResponseStatus :: Lens' CreateContainerServiceRegistryLoginResponse Int
ccsrlrsResponseStatus = lens _ccsrlrsResponseStatus (\s a -> s {_ccsrlrsResponseStatus = a})

instance NFData CreateContainerServiceRegistryLoginResponse
