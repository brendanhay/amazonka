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
-- Module      : Network.AWS.DeviceFarm.GetVPCEConfiguration
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the configuration settings for your Amazon Virtual Private Cloud (VPC) endpoint.
--
--
module Network.AWS.DeviceFarm.GetVPCEConfiguration
    (
    -- * Creating a Request
      getVPCEConfiguration
    , GetVPCEConfiguration
    -- * Request Lenses
    , gvecArn

    -- * Destructuring the Response
    , getVPCEConfigurationResponse
    , GetVPCEConfigurationResponse
    -- * Response Lenses
    , gvecrsVpceConfiguration
    , gvecrsResponseStatus
    ) where

import Network.AWS.DeviceFarm.Types
import Network.AWS.DeviceFarm.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getVPCEConfiguration' smart constructor.
newtype GetVPCEConfiguration = GetVPCEConfiguration'
  { _gvecArn :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetVPCEConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gvecArn' - The Amazon Resource Name (ARN) of the VPC endpoint configuration you want to describe.
getVPCEConfiguration
    :: Text -- ^ 'gvecArn'
    -> GetVPCEConfiguration
getVPCEConfiguration pArn_ = GetVPCEConfiguration' {_gvecArn = pArn_}


-- | The Amazon Resource Name (ARN) of the VPC endpoint configuration you want to describe.
gvecArn :: Lens' GetVPCEConfiguration Text
gvecArn = lens _gvecArn (\ s a -> s{_gvecArn = a})

instance AWSRequest GetVPCEConfiguration where
        type Rs GetVPCEConfiguration =
             GetVPCEConfigurationResponse
        request = postJSON deviceFarm
        response
          = receiveJSON
              (\ s h x ->
                 GetVPCEConfigurationResponse' <$>
                   (x .?> "vpceConfiguration") <*> (pure (fromEnum s)))

instance Hashable GetVPCEConfiguration where

instance NFData GetVPCEConfiguration where

instance ToHeaders GetVPCEConfiguration where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DeviceFarm_20150623.GetVPCEConfiguration" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetVPCEConfiguration where
        toJSON GetVPCEConfiguration'{..}
          = object (catMaybes [Just ("arn" .= _gvecArn)])

instance ToPath GetVPCEConfiguration where
        toPath = const "/"

instance ToQuery GetVPCEConfiguration where
        toQuery = const mempty

-- | /See:/ 'getVPCEConfigurationResponse' smart constructor.
data GetVPCEConfigurationResponse = GetVPCEConfigurationResponse'
  { _gvecrsVpceConfiguration :: !(Maybe VPCEConfiguration)
  , _gvecrsResponseStatus    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetVPCEConfigurationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gvecrsVpceConfiguration' - An object containing information about your VPC endpoint configuration.
--
-- * 'gvecrsResponseStatus' - -- | The response status code.
getVPCEConfigurationResponse
    :: Int -- ^ 'gvecrsResponseStatus'
    -> GetVPCEConfigurationResponse
getVPCEConfigurationResponse pResponseStatus_ =
  GetVPCEConfigurationResponse'
    { _gvecrsVpceConfiguration = Nothing
    , _gvecrsResponseStatus = pResponseStatus_
    }


-- | An object containing information about your VPC endpoint configuration.
gvecrsVpceConfiguration :: Lens' GetVPCEConfigurationResponse (Maybe VPCEConfiguration)
gvecrsVpceConfiguration = lens _gvecrsVpceConfiguration (\ s a -> s{_gvecrsVpceConfiguration = a})

-- | -- | The response status code.
gvecrsResponseStatus :: Lens' GetVPCEConfigurationResponse Int
gvecrsResponseStatus = lens _gvecrsResponseStatus (\ s a -> s{_gvecrsResponseStatus = a})

instance NFData GetVPCEConfigurationResponse where
