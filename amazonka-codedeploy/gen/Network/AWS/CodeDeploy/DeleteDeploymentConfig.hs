{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CodeDeploy.DeleteDeploymentConfig
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes a deployment configuration. A deployment configuration cannot be
-- deleted if it is currently in use. Also, predefined configurations cannot
-- be deleted.
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

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.CodeDeploy.Types
import qualified GHC.Exts

newtype DeleteDeploymentConfig = DeleteDeploymentConfig
    { _ddcDeploymentConfigName :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'DeleteDeploymentConfig' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddcDeploymentConfigName' @::@ 'Text'
--
deleteDeploymentConfig :: Text -- ^ 'ddcDeploymentConfigName'
                       -> DeleteDeploymentConfig
deleteDeploymentConfig p1 = DeleteDeploymentConfig
    { _ddcDeploymentConfigName = p1
    }

-- | The name of an existing deployment configuration within the AWS user
-- account.
ddcDeploymentConfigName :: Lens' DeleteDeploymentConfig Text
ddcDeploymentConfigName =
    lens _ddcDeploymentConfigName (\s a -> s { _ddcDeploymentConfigName = a })

data DeleteDeploymentConfigResponse = DeleteDeploymentConfigResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeleteDeploymentConfigResponse' constructor.
deleteDeploymentConfigResponse :: DeleteDeploymentConfigResponse
deleteDeploymentConfigResponse = DeleteDeploymentConfigResponse

instance ToPath DeleteDeploymentConfig where
    toPath = const "/"

instance ToQuery DeleteDeploymentConfig where
    toQuery = const mempty

instance ToHeaders DeleteDeploymentConfig
instance ToJSON DeleteDeploymentConfig where
    toJSON = genericToJSON jsonOptions

instance AWSRequest DeleteDeploymentConfig where
    type Sv DeleteDeploymentConfig = CodeDeploy
    type Rs DeleteDeploymentConfig = DeleteDeploymentConfigResponse

    request  = post "DeleteDeploymentConfig"
    response = nullResponse DeleteDeploymentConfigResponse
