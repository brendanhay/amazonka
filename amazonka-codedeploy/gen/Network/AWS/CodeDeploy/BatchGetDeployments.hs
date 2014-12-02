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

-- Module      : Network.AWS.CodeDeploy.BatchGetDeployments
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

-- | Gets information about one or more deployments.
--
-- <http://docs.aws.amazon.com/codedeploy/latest/APIReference/API_BatchGetDeployments.html>
module Network.AWS.CodeDeploy.BatchGetDeployments
    (
    -- * Request
      BatchGetDeployments
    -- ** Request constructor
    , batchGetDeployments
    -- ** Request lenses
    , bgdDeploymentIds

    -- * Response
    , BatchGetDeploymentsResponse
    -- ** Response constructor
    , batchGetDeploymentsResponse
    -- ** Response lenses
    , bgdrDeploymentsInfo
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.CodeDeploy.Types
import qualified GHC.Exts

newtype BatchGetDeployments = BatchGetDeployments
    { _bgdDeploymentIds :: List "deployments" Text
    } deriving (Eq, Ord, Show, Monoid, Semigroup)

instance GHC.Exts.IsList BatchGetDeployments where
    type Item BatchGetDeployments = Text

    fromList = BatchGetDeployments . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _bgdDeploymentIds

-- | 'BatchGetDeployments' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'bgdDeploymentIds' @::@ ['Text']
--
batchGetDeployments :: BatchGetDeployments
batchGetDeployments = BatchGetDeployments
    { _bgdDeploymentIds = mempty
    }

-- | A list of deployment IDs, with multiple deployment IDs separated by spaces.
bgdDeploymentIds :: Lens' BatchGetDeployments [Text]
bgdDeploymentIds = lens _bgdDeploymentIds (\s a -> s { _bgdDeploymentIds = a }) . _List

newtype BatchGetDeploymentsResponse = BatchGetDeploymentsResponse
    { _bgdrDeploymentsInfo :: List "deploymentsInfo" DeploymentInfo
    } deriving (Eq, Show, Monoid, Semigroup)

instance GHC.Exts.IsList BatchGetDeploymentsResponse where
    type Item BatchGetDeploymentsResponse = DeploymentInfo

    fromList = BatchGetDeploymentsResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _bgdrDeploymentsInfo

-- | 'BatchGetDeploymentsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'bgdrDeploymentsInfo' @::@ ['DeploymentInfo']
--
batchGetDeploymentsResponse :: BatchGetDeploymentsResponse
batchGetDeploymentsResponse = BatchGetDeploymentsResponse
    { _bgdrDeploymentsInfo = mempty
    }

-- | Information about the deployments.
bgdrDeploymentsInfo :: Lens' BatchGetDeploymentsResponse [DeploymentInfo]
bgdrDeploymentsInfo =
    lens _bgdrDeploymentsInfo (\s a -> s { _bgdrDeploymentsInfo = a })
        . _List

instance ToPath BatchGetDeployments where
    toPath = const "/"

instance ToQuery BatchGetDeployments where
    toQuery = const mempty

instance ToHeaders BatchGetDeployments

instance ToJSON BatchGetDeployments where
    toJSON BatchGetDeployments{..} = object
        [ "deploymentIds" .= _bgdDeploymentIds
        ]

instance AWSRequest BatchGetDeployments where
    type Sv BatchGetDeployments = CodeDeploy
    type Rs BatchGetDeployments = BatchGetDeploymentsResponse

    request  = post "BatchGetDeployments"
    response = jsonResponse

instance FromJSON BatchGetDeploymentsResponse where
    parseJSON = withObject "BatchGetDeploymentsResponse" $ \o -> BatchGetDeploymentsResponse
        <$> o .:? "deploymentsInfo" .!= mempty
