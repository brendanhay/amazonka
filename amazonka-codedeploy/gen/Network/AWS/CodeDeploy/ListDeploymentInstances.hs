{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -w                      #-}

-- Module      : Network.AWS.CodeDeploy.ListDeploymentInstances
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Lists the Amazon EC2 instances for a deployment within the AWS user
-- account.
module Network.AWS.CodeDeploy.ListDeploymentInstances
    (
    -- * Request
      ListDeploymentInstances
    -- ** Request constructor
    , listDeploymentInstances
    -- ** Request lenses
    , ldiDeploymentId
    , ldiInstanceStatusFilter
    , ldiNextToken

    -- * Response
    , ListDeploymentInstancesResponse
    -- ** Response constructor
    , listDeploymentInstancesResponse
    -- ** Response lenses
    , ldirInstancesList
    , ldirNextToken
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.CodeDeploy.Types
import qualified GHC.Exts

data ListDeploymentInstances = ListDeploymentInstances
    { _ldiDeploymentId         :: Text
    , _ldiInstanceStatusFilter :: [Text]
    , _ldiNextToken            :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'ListDeploymentInstances' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ldiDeploymentId' @::@ 'Text'
--
-- * 'ldiInstanceStatusFilter' @::@ ['Text']
--
-- * 'ldiNextToken' @::@ 'Maybe' 'Text'
--
listDeploymentInstances :: Text -- ^ 'ldiDeploymentId'
                        -> ListDeploymentInstances
listDeploymentInstances p1 = ListDeploymentInstances
    { _ldiDeploymentId         = p1
    , _ldiNextToken            = Nothing
    , _ldiInstanceStatusFilter = mempty
    }

-- | The unique ID of a deployment.
ldiDeploymentId :: Lens' ListDeploymentInstances Text
ldiDeploymentId = lens _ldiDeploymentId (\s a -> s { _ldiDeploymentId = a })

-- | A subset of instances to list, by status: Pending: Include in the
-- resulting list those instances with pending deployments. InProgress:
-- Include in the resulting list those instances with in-progress
-- deployments. Succeeded: Include in the resulting list those instances
-- with succeeded deployments. Failed: Include in the resulting list those
-- instances with failed deployments. Skipped: Include in the resulting list
-- those instances with skipped deployments. Unknown: Include in the
-- resulting list those instances with deployments in an unknown state.
ldiInstanceStatusFilter :: Lens' ListDeploymentInstances [Text]
ldiInstanceStatusFilter =
    lens _ldiInstanceStatusFilter (\s a -> s { _ldiInstanceStatusFilter = a })

-- | An identifier that was returned from the previous list deployment
-- instances call, which can be used to return the next set of deployment
-- instances in the list.
ldiNextToken :: Lens' ListDeploymentInstances (Maybe Text)
ldiNextToken = lens _ldiNextToken (\s a -> s { _ldiNextToken = a })

instance ToPath ListDeploymentInstances where
    toPath = const "/"

instance ToQuery ListDeploymentInstances where
    toQuery = const mempty

instance ToHeaders ListDeploymentInstances

instance ToBody ListDeploymentInstances where
    toBody = toBody . encode . _ldiDeploymentId

data ListDeploymentInstancesResponse = ListDeploymentInstancesResponse
    { _ldirInstancesList :: [Text]
    , _ldirNextToken     :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'ListDeploymentInstancesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ldirInstancesList' @::@ ['Text']
--
-- * 'ldirNextToken' @::@ 'Maybe' 'Text'
--
listDeploymentInstancesResponse :: ListDeploymentInstancesResponse
listDeploymentInstancesResponse = ListDeploymentInstancesResponse
    { _ldirInstancesList = mempty
    , _ldirNextToken     = Nothing
    }

-- | A list of instance IDs.
ldirInstancesList :: Lens' ListDeploymentInstancesResponse [Text]
ldirInstancesList =
    lens _ldirInstancesList (\s a -> s { _ldirInstancesList = a })

-- | If the amount of information that is returned is significantly large, an
-- identifier will also be returned, which can be used in a subsequent list
-- deployment instances call to return the next set of deployment instances
-- in the list.
ldirNextToken :: Lens' ListDeploymentInstancesResponse (Maybe Text)
ldirNextToken = lens _ldirNextToken (\s a -> s { _ldirNextToken = a })

instance AWSRequest ListDeploymentInstances where
    type Sv ListDeploymentInstances = CodeDeploy
    type Rs ListDeploymentInstances = ListDeploymentInstancesResponse

    request  = post
    response = jsonResponse $ \h o -> ListDeploymentInstancesResponse
        <$> o .: "instancesList"
        <*> o .: "nextToken"
