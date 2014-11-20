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

-- Module      : Network.AWS.CodeDeploy.ListDeployments
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Lists the deployments under a deployment group for an application
-- registered within the AWS user account.
--
-- <http://docs.aws.amazon.com/codedeploy/latest/APIReference/API_ListDeployments.html>
module Network.AWS.CodeDeploy.ListDeployments
    (
    -- * Request
      ListDeployments
    -- ** Request constructor
    , listDeployments
    -- ** Request lenses
    , ldApplicationName
    , ldCreateTimeRange
    , ldDeploymentGroupName
    , ldIncludeOnlyStatuses
    , ldNextToken

    -- * Response
    , ListDeploymentsResponse
    -- ** Response constructor
    , listDeploymentsResponse
    -- ** Response lenses
    , ldrDeployments
    , ldrNextToken
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.CodeDeploy.Types
import qualified GHC.Exts

data ListDeployments = ListDeployments
    { _ldApplicationName     :: Maybe Text
    , _ldCreateTimeRange     :: Maybe TimeRange
    , _ldDeploymentGroupName :: Maybe Text
    , _ldIncludeOnlyStatuses :: List "includeOnlyStatuses" Text
    , _ldNextToken           :: Maybe Text
    } deriving (Eq, Show)

-- | 'ListDeployments' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ldApplicationName' @::@ 'Maybe' 'Text'
--
-- * 'ldCreateTimeRange' @::@ 'Maybe' 'TimeRange'
--
-- * 'ldDeploymentGroupName' @::@ 'Maybe' 'Text'
--
-- * 'ldIncludeOnlyStatuses' @::@ ['Text']
--
-- * 'ldNextToken' @::@ 'Maybe' 'Text'
--
listDeployments :: ListDeployments
listDeployments = ListDeployments
    { _ldApplicationName     = Nothing
    , _ldDeploymentGroupName = Nothing
    , _ldIncludeOnlyStatuses = mempty
    , _ldCreateTimeRange     = Nothing
    , _ldNextToken           = Nothing
    }

-- | The name of an existing AWS CodeDeploy application within the AWS user
-- account.
ldApplicationName :: Lens' ListDeployments (Maybe Text)
ldApplicationName =
    lens _ldApplicationName (\s a -> s { _ldApplicationName = a })

-- | A deployment creation start- and end-time range for returning a subset of
-- the list of deployments.
ldCreateTimeRange :: Lens' ListDeployments (Maybe TimeRange)
ldCreateTimeRange =
    lens _ldCreateTimeRange (\s a -> s { _ldCreateTimeRange = a })

-- | The name of an existing deployment group for the specified application.
ldDeploymentGroupName :: Lens' ListDeployments (Maybe Text)
ldDeploymentGroupName =
    lens _ldDeploymentGroupName (\s a -> s { _ldDeploymentGroupName = a })

-- | A subset of deployments to list, by status: Created: Include in the
-- resulting list created deployments. Queued: Include in the resulting list
-- queued deployments. In Progress: Include in the resulting list
-- in-progress deployments. Succeeded: Include in the resulting list
-- succeeded deployments. Failed: Include in the resulting list failed
-- deployments. Aborted: Include in the resulting list aborted deployments.
ldIncludeOnlyStatuses :: Lens' ListDeployments [Text]
ldIncludeOnlyStatuses =
    lens _ldIncludeOnlyStatuses (\s a -> s { _ldIncludeOnlyStatuses = a })
        . _List

-- | An identifier that was returned from the previous list deployments call,
-- which can be used to return the next set of deployments in the list.
ldNextToken :: Lens' ListDeployments (Maybe Text)
ldNextToken = lens _ldNextToken (\s a -> s { _ldNextToken = a })

data ListDeploymentsResponse = ListDeploymentsResponse
    { _ldrDeployments :: List "deployments" Text
    , _ldrNextToken   :: Maybe Text
    } deriving (Eq, Ord, Show)

-- | 'ListDeploymentsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ldrDeployments' @::@ ['Text']
--
-- * 'ldrNextToken' @::@ 'Maybe' 'Text'
--
listDeploymentsResponse :: ListDeploymentsResponse
listDeploymentsResponse = ListDeploymentsResponse
    { _ldrDeployments = mempty
    , _ldrNextToken   = Nothing
    }

-- | A list of deployment IDs.
ldrDeployments :: Lens' ListDeploymentsResponse [Text]
ldrDeployments = lens _ldrDeployments (\s a -> s { _ldrDeployments = a }) . _List

-- | If the amount of information that is returned is significantly large, an
-- identifier will also be returned, which can be used in a subsequent list
-- deployments call to return the next set of deployments in the list.
ldrNextToken :: Lens' ListDeploymentsResponse (Maybe Text)
ldrNextToken = lens _ldrNextToken (\s a -> s { _ldrNextToken = a })

instance ToPath ListDeployments where
    toPath = const "/"

instance ToQuery ListDeployments where
    toQuery = const mempty

instance ToHeaders ListDeployments

instance ToJSON ListDeployments where
    toJSON ListDeployments{..} = object
        [ "applicationName"     .= _ldApplicationName
        , "deploymentGroupName" .= _ldDeploymentGroupName
        , "includeOnlyStatuses" .= _ldIncludeOnlyStatuses
        , "createTimeRange"     .= _ldCreateTimeRange
        , "nextToken"           .= _ldNextToken
        ]

instance AWSRequest ListDeployments where
    type Sv ListDeployments = CodeDeploy
    type Rs ListDeployments = ListDeploymentsResponse

    request  = post "ListDeployments"
    response = jsonResponse

instance FromJSON ListDeploymentsResponse where
    parseJSON = withObject "ListDeploymentsResponse" $ \o -> ListDeploymentsResponse
        <$> o .:  "deployments"
        <*> o .:? "nextToken"


Some kind of operator / class to check the types whether to continue?
