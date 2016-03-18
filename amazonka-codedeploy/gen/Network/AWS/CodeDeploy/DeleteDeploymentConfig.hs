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
-- Module      : Network.AWS.CodeDeploy.DeleteDeploymentConfig
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a deployment configuration.
--
-- A deployment configuration cannot be deleted if it is currently in use.
-- Predefined configurations cannot be deleted.
--
-- /See:/ <http://docs.aws.amazon.com/codedeploy/latest/APIReference/API_DeleteDeploymentConfig.html AWS API Reference> for DeleteDeploymentConfig.
module Network.AWS.CodeDeploy.DeleteDeploymentConfig
    (
    -- * Creating a Request
      deleteDeploymentConfig
    , DeleteDeploymentConfig
    -- * Request Lenses
    , ddcDeploymentConfigName

    -- * Destructuring the Response
    , deleteDeploymentConfigResponse
    , DeleteDeploymentConfigResponse
    ) where

import           Network.AWS.CodeDeploy.Types
import           Network.AWS.CodeDeploy.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a delete deployment configuration operation.
--
-- /See:/ 'deleteDeploymentConfig' smart constructor.
newtype DeleteDeploymentConfig = DeleteDeploymentConfig'
    { _ddcDeploymentConfigName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteDeploymentConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddcDeploymentConfigName'
deleteDeploymentConfig
    :: Text -- ^ 'ddcDeploymentConfigName'
    -> DeleteDeploymentConfig
deleteDeploymentConfig pDeploymentConfigName_ =
    DeleteDeploymentConfig'
    { _ddcDeploymentConfigName = pDeploymentConfigName_
    }

-- | The name of a deployment configuration associated with the applicable
-- IAM user or AWS account.
ddcDeploymentConfigName :: Lens' DeleteDeploymentConfig Text
ddcDeploymentConfigName = lens _ddcDeploymentConfigName (\ s a -> s{_ddcDeploymentConfigName = a});

instance AWSRequest DeleteDeploymentConfig where
        type Rs DeleteDeploymentConfig =
             DeleteDeploymentConfigResponse
        request = postJSON codeDeploy
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
              (catMaybes
                 [Just
                    ("deploymentConfigName" .=
                       _ddcDeploymentConfigName)])

instance ToPath DeleteDeploymentConfig where
        toPath = const "/"

instance ToQuery DeleteDeploymentConfig where
        toQuery = const mempty

-- | /See:/ 'deleteDeploymentConfigResponse' smart constructor.
data DeleteDeploymentConfigResponse =
    DeleteDeploymentConfigResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteDeploymentConfigResponse' with the minimum fields required to make a request.
--
deleteDeploymentConfigResponse
    :: DeleteDeploymentConfigResponse
deleteDeploymentConfigResponse = DeleteDeploymentConfigResponse'
