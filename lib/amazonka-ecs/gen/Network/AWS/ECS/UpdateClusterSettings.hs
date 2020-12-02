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
-- Module      : Network.AWS.ECS.UpdateClusterSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the settings to use for a cluster.
module Network.AWS.ECS.UpdateClusterSettings
  ( -- * Creating a Request
    updateClusterSettings,
    UpdateClusterSettings,

    -- * Request Lenses
    ucsCluster,
    ucsSettings,

    -- * Destructuring the Response
    updateClusterSettingsResponse,
    UpdateClusterSettingsResponse,

    -- * Response Lenses
    ucsrsCluster,
    ucsrsResponseStatus,
  )
where

import Network.AWS.ECS.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateClusterSettings' smart constructor.
data UpdateClusterSettings = UpdateClusterSettings'
  { _ucsCluster ::
      !Text,
    _ucsSettings :: ![ClusterSetting]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateClusterSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ucsCluster' - The name of the cluster to modify the settings for.
--
-- * 'ucsSettings' - The setting to use by default for a cluster. This parameter is used to enable CloudWatch Container Insights for a cluster. If this value is specified, it will override the @containerInsights@ value set with 'PutAccountSetting' or 'PutAccountSettingDefault' .
updateClusterSettings ::
  -- | 'ucsCluster'
  Text ->
  UpdateClusterSettings
updateClusterSettings pCluster_ =
  UpdateClusterSettings'
    { _ucsCluster = pCluster_,
      _ucsSettings = mempty
    }

-- | The name of the cluster to modify the settings for.
ucsCluster :: Lens' UpdateClusterSettings Text
ucsCluster = lens _ucsCluster (\s a -> s {_ucsCluster = a})

-- | The setting to use by default for a cluster. This parameter is used to enable CloudWatch Container Insights for a cluster. If this value is specified, it will override the @containerInsights@ value set with 'PutAccountSetting' or 'PutAccountSettingDefault' .
ucsSettings :: Lens' UpdateClusterSettings [ClusterSetting]
ucsSettings = lens _ucsSettings (\s a -> s {_ucsSettings = a}) . _Coerce

instance AWSRequest UpdateClusterSettings where
  type Rs UpdateClusterSettings = UpdateClusterSettingsResponse
  request = postJSON ecs
  response =
    receiveJSON
      ( \s h x ->
          UpdateClusterSettingsResponse'
            <$> (x .?> "cluster") <*> (pure (fromEnum s))
      )

instance Hashable UpdateClusterSettings

instance NFData UpdateClusterSettings

instance ToHeaders UpdateClusterSettings where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AmazonEC2ContainerServiceV20141113.UpdateClusterSettings" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON UpdateClusterSettings where
  toJSON UpdateClusterSettings' {..} =
    object
      ( catMaybes
          [ Just ("cluster" .= _ucsCluster),
            Just ("settings" .= _ucsSettings)
          ]
      )

instance ToPath UpdateClusterSettings where
  toPath = const "/"

instance ToQuery UpdateClusterSettings where
  toQuery = const mempty

-- | /See:/ 'updateClusterSettingsResponse' smart constructor.
data UpdateClusterSettingsResponse = UpdateClusterSettingsResponse'
  { _ucsrsCluster ::
      !(Maybe Cluster),
    _ucsrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateClusterSettingsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ucsrsCluster' - Undocumented member.
--
-- * 'ucsrsResponseStatus' - -- | The response status code.
updateClusterSettingsResponse ::
  -- | 'ucsrsResponseStatus'
  Int ->
  UpdateClusterSettingsResponse
updateClusterSettingsResponse pResponseStatus_ =
  UpdateClusterSettingsResponse'
    { _ucsrsCluster = Nothing,
      _ucsrsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
ucsrsCluster :: Lens' UpdateClusterSettingsResponse (Maybe Cluster)
ucsrsCluster = lens _ucsrsCluster (\s a -> s {_ucsrsCluster = a})

-- | -- | The response status code.
ucsrsResponseStatus :: Lens' UpdateClusterSettingsResponse Int
ucsrsResponseStatus = lens _ucsrsResponseStatus (\s a -> s {_ucsrsResponseStatus = a})

instance NFData UpdateClusterSettingsResponse
