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
-- Module      : Network.AWS.CodeDeploy.GetDeploymentConfig
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a deployment configuration.
--
--
module Network.AWS.CodeDeploy.GetDeploymentConfig
    (
    -- * Creating a Request
      getDeploymentConfig
    , GetDeploymentConfig
    -- * Request Lenses
    , gdcDeploymentConfigName

    -- * Destructuring the Response
    , getDeploymentConfigResponse
    , GetDeploymentConfigResponse
    -- * Response Lenses
    , gdcrsDeploymentConfigInfo
    , gdcrsResponseStatus
    ) where

import Network.AWS.CodeDeploy.Types
import Network.AWS.CodeDeploy.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of a GetDeploymentConfig operation.
--
--
--
-- /See:/ 'getDeploymentConfig' smart constructor.
newtype GetDeploymentConfig = GetDeploymentConfig'
  { _gdcDeploymentConfigName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetDeploymentConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdcDeploymentConfigName' - The name of a deployment configuration associated with the applicable IAM user or AWS account.
getDeploymentConfig
    :: Text -- ^ 'gdcDeploymentConfigName'
    -> GetDeploymentConfig
getDeploymentConfig pDeploymentConfigName_ =
  GetDeploymentConfig' {_gdcDeploymentConfigName = pDeploymentConfigName_}


-- | The name of a deployment configuration associated with the applicable IAM user or AWS account.
gdcDeploymentConfigName :: Lens' GetDeploymentConfig Text
gdcDeploymentConfigName = lens _gdcDeploymentConfigName (\ s a -> s{_gdcDeploymentConfigName = a})

instance AWSRequest GetDeploymentConfig where
        type Rs GetDeploymentConfig =
             GetDeploymentConfigResponse
        request = postJSON codeDeploy
        response
          = receiveJSON
              (\ s h x ->
                 GetDeploymentConfigResponse' <$>
                   (x .?> "deploymentConfigInfo") <*>
                     (pure (fromEnum s)))

instance Hashable GetDeploymentConfig where

instance NFData GetDeploymentConfig where

instance ToHeaders GetDeploymentConfig where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeDeploy_20141006.GetDeploymentConfig" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetDeploymentConfig where
        toJSON GetDeploymentConfig'{..}
          = object
              (catMaybes
                 [Just
                    ("deploymentConfigName" .=
                       _gdcDeploymentConfigName)])

instance ToPath GetDeploymentConfig where
        toPath = const "/"

instance ToQuery GetDeploymentConfig where
        toQuery = const mempty

-- | Represents the output of a GetDeploymentConfig operation.
--
--
--
-- /See:/ 'getDeploymentConfigResponse' smart constructor.
data GetDeploymentConfigResponse = GetDeploymentConfigResponse'
  { _gdcrsDeploymentConfigInfo :: !(Maybe DeploymentConfigInfo)
  , _gdcrsResponseStatus       :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetDeploymentConfigResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdcrsDeploymentConfigInfo' - Information about the deployment configuration.
--
-- * 'gdcrsResponseStatus' - -- | The response status code.
getDeploymentConfigResponse
    :: Int -- ^ 'gdcrsResponseStatus'
    -> GetDeploymentConfigResponse
getDeploymentConfigResponse pResponseStatus_ =
  GetDeploymentConfigResponse'
    { _gdcrsDeploymentConfigInfo = Nothing
    , _gdcrsResponseStatus = pResponseStatus_
    }


-- | Information about the deployment configuration.
gdcrsDeploymentConfigInfo :: Lens' GetDeploymentConfigResponse (Maybe DeploymentConfigInfo)
gdcrsDeploymentConfigInfo = lens _gdcrsDeploymentConfigInfo (\ s a -> s{_gdcrsDeploymentConfigInfo = a})

-- | -- | The response status code.
gdcrsResponseStatus :: Lens' GetDeploymentConfigResponse Int
gdcrsResponseStatus = lens _gdcrsResponseStatus (\ s a -> s{_gdcrsResponseStatus = a})

instance NFData GetDeploymentConfigResponse where
