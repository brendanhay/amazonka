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
-- Module      : Network.AWS.EC2.GetLaunchTemplateData
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the configuration data of the specified instance. You can use this data to create a launch template.
--
--
module Network.AWS.EC2.GetLaunchTemplateData
    (
    -- * Creating a Request
      getLaunchTemplateData
    , GetLaunchTemplateData
    -- * Request Lenses
    , gltdDryRun
    , gltdInstanceId

    -- * Destructuring the Response
    , getLaunchTemplateDataResponse
    , GetLaunchTemplateDataResponse
    -- * Response Lenses
    , gltdrsLaunchTemplateData
    , gltdrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getLaunchTemplateData' smart constructor.
data GetLaunchTemplateData = GetLaunchTemplateData'
  { _gltdDryRun     :: !(Maybe Bool)
  , _gltdInstanceId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetLaunchTemplateData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gltdDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'gltdInstanceId' - The ID of the instance.
getLaunchTemplateData
    :: Text -- ^ 'gltdInstanceId'
    -> GetLaunchTemplateData
getLaunchTemplateData pInstanceId_ =
  GetLaunchTemplateData' {_gltdDryRun = Nothing, _gltdInstanceId = pInstanceId_}


-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
gltdDryRun :: Lens' GetLaunchTemplateData (Maybe Bool)
gltdDryRun = lens _gltdDryRun (\ s a -> s{_gltdDryRun = a})

-- | The ID of the instance.
gltdInstanceId :: Lens' GetLaunchTemplateData Text
gltdInstanceId = lens _gltdInstanceId (\ s a -> s{_gltdInstanceId = a})

instance AWSRequest GetLaunchTemplateData where
        type Rs GetLaunchTemplateData =
             GetLaunchTemplateDataResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 GetLaunchTemplateDataResponse' <$>
                   (x .@? "launchTemplateData") <*> (pure (fromEnum s)))

instance Hashable GetLaunchTemplateData where

instance NFData GetLaunchTemplateData where

instance ToHeaders GetLaunchTemplateData where
        toHeaders = const mempty

instance ToPath GetLaunchTemplateData where
        toPath = const "/"

instance ToQuery GetLaunchTemplateData where
        toQuery GetLaunchTemplateData'{..}
          = mconcat
              ["Action" =: ("GetLaunchTemplateData" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "DryRun" =: _gltdDryRun,
               "InstanceId" =: _gltdInstanceId]

-- | /See:/ 'getLaunchTemplateDataResponse' smart constructor.
data GetLaunchTemplateDataResponse = GetLaunchTemplateDataResponse'
  { _gltdrsLaunchTemplateData :: !(Maybe ResponseLaunchTemplateData)
  , _gltdrsResponseStatus     :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetLaunchTemplateDataResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gltdrsLaunchTemplateData' - The instance data.
--
-- * 'gltdrsResponseStatus' - -- | The response status code.
getLaunchTemplateDataResponse
    :: Int -- ^ 'gltdrsResponseStatus'
    -> GetLaunchTemplateDataResponse
getLaunchTemplateDataResponse pResponseStatus_ =
  GetLaunchTemplateDataResponse'
    { _gltdrsLaunchTemplateData = Nothing
    , _gltdrsResponseStatus = pResponseStatus_
    }


-- | The instance data.
gltdrsLaunchTemplateData :: Lens' GetLaunchTemplateDataResponse (Maybe ResponseLaunchTemplateData)
gltdrsLaunchTemplateData = lens _gltdrsLaunchTemplateData (\ s a -> s{_gltdrsLaunchTemplateData = a})

-- | -- | The response status code.
gltdrsResponseStatus :: Lens' GetLaunchTemplateDataResponse Int
gltdrsResponseStatus = lens _gltdrsResponseStatus (\ s a -> s{_gltdrsResponseStatus = a})

instance NFData GetLaunchTemplateDataResponse where
