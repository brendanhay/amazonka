{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -w                      #-}

-- Module      : Network.AWS.CodeDeploy.ListDeploymentConfigs
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Lists the deployment configurations within the AWS user account.
module Network.AWS.CodeDeploy.ListDeploymentConfigs
    (
    -- * Request
      ListDeploymentConfigs
    -- ** Request constructor
    , listDeploymentConfigs
    -- ** Request lenses
    , ldcNextToken

    -- * Response
    , ListDeploymentConfigsResponse
    -- ** Response constructor
    , listDeploymentConfigsResponse
    -- ** Response lenses
    , ldcrDeploymentConfigsList
    , ldcrNextToken
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.CodeDeploy.Types
import qualified GHC.Exts

newtype ListDeploymentConfigs = ListDeploymentConfigs
    { _ldcNextToken :: Maybe Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'ListDeploymentConfigs' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ldcNextToken' @::@ 'Maybe' 'Text'
--
listDeploymentConfigs :: ListDeploymentConfigs
listDeploymentConfigs = ListDeploymentConfigs
    { _ldcNextToken = Nothing
    }

-- | An identifier that was returned from the previous list deployment
-- configurations call, which can be used to return the next set of
-- deployment configurations in the list.
ldcNextToken :: Lens' ListDeploymentConfigs (Maybe Text)
ldcNextToken = lens _ldcNextToken (\s a -> s { _ldcNextToken = a })

instance ToPath ListDeploymentConfigs where
    toPath = const "/"

instance ToQuery ListDeploymentConfigs where
    toQuery = const mempty

instance ToHeaders ListDeploymentConfigs

instance ToBody ListDeploymentConfigs where
    toBody = toBody . encode . _ldcNextToken

data ListDeploymentConfigsResponse = ListDeploymentConfigsResponse
    { _ldcrDeploymentConfigsList :: [Text]
    , _ldcrNextToken             :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'ListDeploymentConfigsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ldcrDeploymentConfigsList' @::@ ['Text']
--
-- * 'ldcrNextToken' @::@ 'Maybe' 'Text'
--
listDeploymentConfigsResponse :: ListDeploymentConfigsResponse
listDeploymentConfigsResponse = ListDeploymentConfigsResponse
    { _ldcrDeploymentConfigsList = mempty
    , _ldcrNextToken             = Nothing
    }

-- | A list of deployment configurations, including the built-in
-- configurations such as CodeDeployDefault.OneAtATime.
ldcrDeploymentConfigsList :: Lens' ListDeploymentConfigsResponse [Text]
ldcrDeploymentConfigsList =
    lens _ldcrDeploymentConfigsList
        (\s a -> s { _ldcrDeploymentConfigsList = a })

-- | If the amount of information that is returned is significantly large, an
-- identifier will also be returned, which can be used in a subsequent list
-- deployment configurations call to return the next set of deployment
-- configurations in the list.
ldcrNextToken :: Lens' ListDeploymentConfigsResponse (Maybe Text)
ldcrNextToken = lens _ldcrNextToken (\s a -> s { _ldcrNextToken = a })

instance AWSRequest ListDeploymentConfigs where
    type Sv ListDeploymentConfigs = CodeDeploy
    type Rs ListDeploymentConfigs = ListDeploymentConfigsResponse

    request  = post
    response = jsonResponse $ \h o -> ListDeploymentConfigsResponse
        <$> o .: "deploymentConfigsList"
        <*> o .: "nextToken"
