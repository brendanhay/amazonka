{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.DeleteVPCEConfiguration
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a configuration for your Amazon Virtual Private Cloud (VPC) endpoint.
--
--
module Network.AWS.DeviceFarm.DeleteVPCEConfiguration
    (
    -- * Creating a Request
      deleteVPCEConfiguration
    , DeleteVPCEConfiguration
    -- * Request Lenses
    , dvecArn

    -- * Destructuring the Response
    , deleteVPCEConfigurationResponse
    , DeleteVPCEConfigurationResponse
    -- * Response Lenses
    , dvecrsResponseStatus
    ) where

import Network.AWS.DeviceFarm.Types
import Network.AWS.DeviceFarm.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteVPCEConfiguration' smart constructor.
newtype DeleteVPCEConfiguration = DeleteVPCEConfiguration'
  { _dvecArn :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteVPCEConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvecArn' - The Amazon Resource Name (ARN) of the VPC endpoint configuration you want to delete.
deleteVPCEConfiguration
    :: Text -- ^ 'dvecArn'
    -> DeleteVPCEConfiguration
deleteVPCEConfiguration pArn_ = DeleteVPCEConfiguration' {_dvecArn = pArn_}


-- | The Amazon Resource Name (ARN) of the VPC endpoint configuration you want to delete.
dvecArn :: Lens' DeleteVPCEConfiguration Text
dvecArn = lens _dvecArn (\ s a -> s{_dvecArn = a})

instance AWSRequest DeleteVPCEConfiguration where
        type Rs DeleteVPCEConfiguration =
             DeleteVPCEConfigurationResponse
        request = postJSON deviceFarm
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteVPCEConfigurationResponse' <$>
                   (pure (fromEnum s)))

instance Hashable DeleteVPCEConfiguration where

instance NFData DeleteVPCEConfiguration where

instance ToHeaders DeleteVPCEConfiguration where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DeviceFarm_20150623.DeleteVPCEConfiguration" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteVPCEConfiguration where
        toJSON DeleteVPCEConfiguration'{..}
          = object (catMaybes [Just ("arn" .= _dvecArn)])

instance ToPath DeleteVPCEConfiguration where
        toPath = const "/"

instance ToQuery DeleteVPCEConfiguration where
        toQuery = const mempty

-- | /See:/ 'deleteVPCEConfigurationResponse' smart constructor.
newtype DeleteVPCEConfigurationResponse = DeleteVPCEConfigurationResponse'
  { _dvecrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteVPCEConfigurationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvecrsResponseStatus' - -- | The response status code.
deleteVPCEConfigurationResponse
    :: Int -- ^ 'dvecrsResponseStatus'
    -> DeleteVPCEConfigurationResponse
deleteVPCEConfigurationResponse pResponseStatus_ =
  DeleteVPCEConfigurationResponse' {_dvecrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dvecrsResponseStatus :: Lens' DeleteVPCEConfigurationResponse Int
dvecrsResponseStatus = lens _dvecrsResponseStatus (\ s a -> s{_dvecrsResponseStatus = a})

instance NFData DeleteVPCEConfigurationResponse where
