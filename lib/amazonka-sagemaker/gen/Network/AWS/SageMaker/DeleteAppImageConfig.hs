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
-- Module      : Network.AWS.SageMaker.DeleteAppImageConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an AppImageConfig.
module Network.AWS.SageMaker.DeleteAppImageConfig
  ( -- * Creating a Request
    deleteAppImageConfig,
    DeleteAppImageConfig,

    -- * Request Lenses
    daicAppImageConfigName,

    -- * Destructuring the Response
    deleteAppImageConfigResponse,
    DeleteAppImageConfigResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'deleteAppImageConfig' smart constructor.
newtype DeleteAppImageConfig = DeleteAppImageConfig'
  { _daicAppImageConfigName ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteAppImageConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daicAppImageConfigName' - The name of the AppImageConfig to delete.
deleteAppImageConfig ::
  -- | 'daicAppImageConfigName'
  Text ->
  DeleteAppImageConfig
deleteAppImageConfig pAppImageConfigName_ =
  DeleteAppImageConfig'
    { _daicAppImageConfigName =
        pAppImageConfigName_
    }

-- | The name of the AppImageConfig to delete.
daicAppImageConfigName :: Lens' DeleteAppImageConfig Text
daicAppImageConfigName = lens _daicAppImageConfigName (\s a -> s {_daicAppImageConfigName = a})

instance AWSRequest DeleteAppImageConfig where
  type Rs DeleteAppImageConfig = DeleteAppImageConfigResponse
  request = postJSON sageMaker
  response = receiveNull DeleteAppImageConfigResponse'

instance Hashable DeleteAppImageConfig

instance NFData DeleteAppImageConfig

instance ToHeaders DeleteAppImageConfig where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("SageMaker.DeleteAppImageConfig" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeleteAppImageConfig where
  toJSON DeleteAppImageConfig' {..} =
    object
      ( catMaybes
          [Just ("AppImageConfigName" .= _daicAppImageConfigName)]
      )

instance ToPath DeleteAppImageConfig where
  toPath = const "/"

instance ToQuery DeleteAppImageConfig where
  toQuery = const mempty

-- | /See:/ 'deleteAppImageConfigResponse' smart constructor.
data DeleteAppImageConfigResponse = DeleteAppImageConfigResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteAppImageConfigResponse' with the minimum fields required to make a request.
deleteAppImageConfigResponse ::
  DeleteAppImageConfigResponse
deleteAppImageConfigResponse = DeleteAppImageConfigResponse'

instance NFData DeleteAppImageConfigResponse
