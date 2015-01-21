{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CodeDeploy.ListDeploymentConfigs
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Lists the deployment configurations within the AWS user account.
--
-- <http://docs.aws.amazon.com/codedeploy/latest/APIReference/API_ListDeploymentConfigs.html>
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
import Network.AWS.Request.JSON
import Network.AWS.CodeDeploy.Types
import qualified GHC.Exts

newtype ListDeploymentConfigs = ListDeploymentConfigs
    { _ldcNextToken :: Maybe Text
    } deriving (Eq, Ord, Read, Show, Monoid)

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
-- configurations call, which can be used to return the next set of deployment
-- configurations in the list.
ldcNextToken :: Lens' ListDeploymentConfigs (Maybe Text)
ldcNextToken = lens _ldcNextToken (\s a -> s { _ldcNextToken = a })

data ListDeploymentConfigsResponse = ListDeploymentConfigsResponse
    { _ldcrDeploymentConfigsList :: List "deploymentConfigsList" Text
    , _ldcrNextToken             :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

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

-- | A list of deployment configurations, including the built-in configurations
-- such as CodeDeployDefault.OneAtATime.
ldcrDeploymentConfigsList :: Lens' ListDeploymentConfigsResponse [Text]
ldcrDeploymentConfigsList =
    lens _ldcrDeploymentConfigsList
        (\s a -> s { _ldcrDeploymentConfigsList = a })
            . _List

-- | If the amount of information that is returned is significantly large, an
-- identifier will also be returned, which can be used in a subsequent list
-- deployment configurations call to return the next set of deployment
-- configurations in the list.
ldcrNextToken :: Lens' ListDeploymentConfigsResponse (Maybe Text)
ldcrNextToken = lens _ldcrNextToken (\s a -> s { _ldcrNextToken = a })

instance ToPath ListDeploymentConfigs where
    toPath = const "/"

instance ToQuery ListDeploymentConfigs where
    toQuery = const mempty

instance ToHeaders ListDeploymentConfigs

instance ToJSON ListDeploymentConfigs where
    toJSON ListDeploymentConfigs{..} = object
        [ "nextToken" .= _ldcNextToken
        ]

instance AWSRequest ListDeploymentConfigs where
    type Sv ListDeploymentConfigs = CodeDeploy
    type Rs ListDeploymentConfigs = ListDeploymentConfigsResponse

    request  = post "ListDeploymentConfigs"
    response = jsonResponse

instance FromJSON ListDeploymentConfigsResponse where
    parseJSON = withObject "ListDeploymentConfigsResponse" $ \o -> ListDeploymentConfigsResponse
        <$> o .:? "deploymentConfigsList" .!= mempty
        <*> o .:? "nextToken"
