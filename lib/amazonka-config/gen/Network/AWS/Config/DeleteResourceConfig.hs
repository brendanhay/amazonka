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
-- Module      : Network.AWS.Config.DeleteResourceConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Records the configuration state for a custom resource that has been deleted. This API records a new ConfigurationItem with a ResourceDeleted status. You can retrieve the ConfigurationItems recorded for this resource in your AWS Config History.
module Network.AWS.Config.DeleteResourceConfig
  ( -- * Creating a Request
    deleteResourceConfig,
    DeleteResourceConfig,

    -- * Request Lenses
    drcResourceType,
    drcResourceId,

    -- * Destructuring the Response
    deleteResourceConfigResponse,
    DeleteResourceConfigResponse,
  )
where

import Network.AWS.Config.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteResourceConfig' smart constructor.
data DeleteResourceConfig = DeleteResourceConfig'
  { _drcResourceType ::
      !Text,
    _drcResourceId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteResourceConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drcResourceType' - The type of the resource.
--
-- * 'drcResourceId' - Unique identifier of the resource.
deleteResourceConfig ::
  -- | 'drcResourceType'
  Text ->
  -- | 'drcResourceId'
  Text ->
  DeleteResourceConfig
deleteResourceConfig pResourceType_ pResourceId_ =
  DeleteResourceConfig'
    { _drcResourceType = pResourceType_,
      _drcResourceId = pResourceId_
    }

-- | The type of the resource.
drcResourceType :: Lens' DeleteResourceConfig Text
drcResourceType = lens _drcResourceType (\s a -> s {_drcResourceType = a})

-- | Unique identifier of the resource.
drcResourceId :: Lens' DeleteResourceConfig Text
drcResourceId = lens _drcResourceId (\s a -> s {_drcResourceId = a})

instance AWSRequest DeleteResourceConfig where
  type Rs DeleteResourceConfig = DeleteResourceConfigResponse
  request = postJSON config
  response = receiveNull DeleteResourceConfigResponse'

instance Hashable DeleteResourceConfig

instance NFData DeleteResourceConfig

instance ToHeaders DeleteResourceConfig where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("StarlingDoveService.DeleteResourceConfig" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeleteResourceConfig where
  toJSON DeleteResourceConfig' {..} =
    object
      ( catMaybes
          [ Just ("ResourceType" .= _drcResourceType),
            Just ("ResourceId" .= _drcResourceId)
          ]
      )

instance ToPath DeleteResourceConfig where
  toPath = const "/"

instance ToQuery DeleteResourceConfig where
  toQuery = const mempty

-- | /See:/ 'deleteResourceConfigResponse' smart constructor.
data DeleteResourceConfigResponse = DeleteResourceConfigResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteResourceConfigResponse' with the minimum fields required to make a request.
deleteResourceConfigResponse ::
  DeleteResourceConfigResponse
deleteResourceConfigResponse = DeleteResourceConfigResponse'

instance NFData DeleteResourceConfigResponse
