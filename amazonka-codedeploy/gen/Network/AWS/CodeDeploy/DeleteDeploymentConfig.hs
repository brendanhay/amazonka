{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CodeDeploy.DeleteDeploymentConfig
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Deletes a deployment configuration.
--
-- A deployment configuration cannot be deleted if it is currently in use.
-- Also, predefined configurations cannot be deleted.
--
-- <http://docs.aws.amazon.com/codedeploy/latest/APIReference/API_DeleteDeploymentConfig.html>
module Network.AWS.CodeDeploy.DeleteDeploymentConfig
    (
    -- * Request
      DeleteDeploymentConfig
    -- ** Request constructor
    , deleteDeploymentConfig
    -- ** Request lenses
    , ddcDeploymentConfigName

    -- * Response
    , DeleteDeploymentConfigResponse
    -- ** Response constructor
    , deleteDeploymentConfigResponse
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.CodeDeploy.Types

-- | /See:/ 'deleteDeploymentConfig' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddcDeploymentConfigName'
newtype DeleteDeploymentConfig = DeleteDeploymentConfig'{_ddcDeploymentConfigName :: Text} deriving (Eq, Read, Show)

-- | 'DeleteDeploymentConfig' smart constructor.
deleteDeploymentConfig :: Text -> DeleteDeploymentConfig
deleteDeploymentConfig pDeploymentConfigName = DeleteDeploymentConfig'{_ddcDeploymentConfigName = pDeploymentConfigName};

-- | The name of an existing deployment configuration associated with the
-- applicable IAM user or AWS account.
ddcDeploymentConfigName :: Lens' DeleteDeploymentConfig Text
ddcDeploymentConfigName = lens _ddcDeploymentConfigName (\ s a -> s{_ddcDeploymentConfigName = a});

instance AWSRequest DeleteDeploymentConfig where
        type Sv DeleteDeploymentConfig = CodeDeploy
        type Rs DeleteDeploymentConfig =
             DeleteDeploymentConfigResponse
        request = postJSON
        response
          = receiveNull DeleteDeploymentConfigResponse'

instance ToHeaders DeleteDeploymentConfig where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeDeploy_20141006.DeleteDeploymentConfig" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteDeploymentConfig where
        toJSON DeleteDeploymentConfig'{..}
          = object
              ["deploymentConfigName" .= _ddcDeploymentConfigName]

instance ToPath DeleteDeploymentConfig where
        toPath = const "/"

instance ToQuery DeleteDeploymentConfig where
        toQuery = const mempty

-- | /See:/ 'deleteDeploymentConfigResponse' smart constructor.
data DeleteDeploymentConfigResponse = DeleteDeploymentConfigResponse' deriving (Eq, Read, Show)

-- | 'DeleteDeploymentConfigResponse' smart constructor.
deleteDeploymentConfigResponse :: DeleteDeploymentConfigResponse
deleteDeploymentConfigResponse = DeleteDeploymentConfigResponse';
