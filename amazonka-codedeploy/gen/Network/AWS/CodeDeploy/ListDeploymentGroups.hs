{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CodeDeploy.ListDeploymentGroups
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Lists the deployment groups for an application registered within the AWS
-- user account.
--
-- <http://docs.aws.amazon.com/codedeploy/latest/APIReference/API_ListDeploymentGroups.html>
module Network.AWS.CodeDeploy.ListDeploymentGroups
    (
    -- * Request
      ListDeploymentGroups
    -- ** Request constructor
    , listDeploymentGroups
    -- ** Request lenses
    , ldgApplicationName
    , ldgNextToken

    -- * Response
    , ListDeploymentGroupsResponse
    -- ** Response constructor
    , listDeploymentGroupsResponse
    -- ** Response lenses
    , ldgrApplicationName
    , ldgrDeploymentGroups
    , ldgrNextToken
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.CodeDeploy.Types
import qualified GHC.Exts

data ListDeploymentGroups = ListDeploymentGroups
    { _ldgApplicationName :: Text
    , _ldgNextToken       :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'ListDeploymentGroups' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ldgApplicationName' @::@ 'Text'
--
-- * 'ldgNextToken' @::@ 'Maybe' 'Text'
--
listDeploymentGroups :: Text -- ^ 'ldgApplicationName'
                     -> ListDeploymentGroups
listDeploymentGroups p1 = ListDeploymentGroups
    { _ldgApplicationName = p1
    , _ldgNextToken       = Nothing
    }

-- | The name of an existing AWS CodeDeploy application within the AWS user
-- account.
ldgApplicationName :: Lens' ListDeploymentGroups Text
ldgApplicationName =
    lens _ldgApplicationName (\s a -> s { _ldgApplicationName = a })

-- | An identifier that was returned from the previous list deployment groups
-- call, which can be used to return the next set of deployment groups in
-- the list.
ldgNextToken :: Lens' ListDeploymentGroups (Maybe Text)
ldgNextToken = lens _ldgNextToken (\s a -> s { _ldgNextToken = a })

data ListDeploymentGroupsResponse = ListDeploymentGroupsResponse
    { _ldgrApplicationName  :: Maybe Text
    , _ldgrDeploymentGroups :: [Text]
    , _ldgrNextToken        :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'ListDeploymentGroupsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ldgrApplicationName' @::@ 'Maybe' 'Text'
--
-- * 'ldgrDeploymentGroups' @::@ ['Text']
--
-- * 'ldgrNextToken' @::@ 'Maybe' 'Text'
--
listDeploymentGroupsResponse :: ListDeploymentGroupsResponse
listDeploymentGroupsResponse = ListDeploymentGroupsResponse
    { _ldgrApplicationName  = Nothing
    , _ldgrDeploymentGroups = mempty
    , _ldgrNextToken        = Nothing
    }

-- | The application name.
ldgrApplicationName :: Lens' ListDeploymentGroupsResponse (Maybe Text)
ldgrApplicationName =
    lens _ldgrApplicationName (\s a -> s { _ldgrApplicationName = a })

-- | A list of corresponding deployment group names.
ldgrDeploymentGroups :: Lens' ListDeploymentGroupsResponse [Text]
ldgrDeploymentGroups =
    lens _ldgrDeploymentGroups (\s a -> s { _ldgrDeploymentGroups = a })

-- | If the amount of information that is returned is significantly large, an
-- identifier will also be returned, which can be used in a subsequent list
-- deployment groups call to return the next set of deployment groups in the
-- list.
ldgrNextToken :: Lens' ListDeploymentGroupsResponse (Maybe Text)
ldgrNextToken = lens _ldgrNextToken (\s a -> s { _ldgrNextToken = a })

instance AWSRequest ListDeploymentGroups where
    type Sv ListDeploymentGroups = CodeDeploy
    type Rs ListDeploymentGroups = ListDeploymentGroupsResponse

    request  = post
    response = jsonResponse

instance FromJSON ListDeploymentGroupsResponse where
    parseJSON = genericParseJSON jsonOptions

instance ToPath ListDeploymentGroups where
    toPath = const "/"

instance ToHeaders ListDeploymentGroups

instance ToQuery ListDeploymentGroups where
    toQuery = const mempty

instance ToJSON ListDeploymentGroups where
    toJSON = genericToJSON jsonOptions
