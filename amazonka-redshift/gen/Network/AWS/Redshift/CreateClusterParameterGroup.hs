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

-- Module      : Network.AWS.Redshift.CreateClusterParameterGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates an Amazon Redshift parameter group.
--
-- Creating parameter groups is independent of creating clusters. You can
-- associate a cluster with a parameter group when you create the cluster. You
-- can also associate an existing cluster with a parameter group after the
-- cluster is created by using 'ModifyCluster'.
--
-- Parameters in the parameter group define specific behavior that applies to
-- the databases you create on the cluster. For more information about managing
-- parameter groups, go to <http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-parameter-groups.html Amazon Redshift Parameter Groups> in the /AmazonRedshift Cluster Management Guide/.
--
-- <http://docs.aws.amazon.com/redshift/latest/APIReference/API_CreateClusterParameterGroup.html>
module Network.AWS.Redshift.CreateClusterParameterGroup
    (
    -- * Request
      CreateClusterParameterGroup
    -- ** Request constructor
    , createClusterParameterGroup
    -- ** Request lenses
    , ccpgDescription
    , ccpgParameterGroupFamily
    , ccpgParameterGroupName
    , ccpgTags

    -- * Response
    , CreateClusterParameterGroupResponse
    -- ** Response constructor
    , createClusterParameterGroupResponse
    -- ** Response lenses
    , ccpgrClusterParameterGroup
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.Redshift.Types
import qualified GHC.Exts

data CreateClusterParameterGroup = CreateClusterParameterGroup
    { _ccpgDescription          :: Text
    , _ccpgParameterGroupFamily :: Text
    , _ccpgParameterGroupName   :: Text
    , _ccpgTags                 :: List "Tag" Tag
    } deriving (Eq, Show)

-- | 'CreateClusterParameterGroup' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccpgDescription' @::@ 'Text'
--
-- * 'ccpgParameterGroupFamily' @::@ 'Text'
--
-- * 'ccpgParameterGroupName' @::@ 'Text'
--
-- * 'ccpgTags' @::@ ['Tag']
--
createClusterParameterGroup :: Text -- ^ 'ccpgParameterGroupName'
                            -> Text -- ^ 'ccpgParameterGroupFamily'
                            -> Text -- ^ 'ccpgDescription'
                            -> CreateClusterParameterGroup
createClusterParameterGroup p1 p2 p3 = CreateClusterParameterGroup
    { _ccpgParameterGroupName   = p1
    , _ccpgParameterGroupFamily = p2
    , _ccpgDescription          = p3
    , _ccpgTags                 = mempty
    }

-- | A description of the parameter group.
--
ccpgDescription :: Lens' CreateClusterParameterGroup Text
ccpgDescription = lens _ccpgDescription (\s a -> s { _ccpgDescription = a })

-- | The Amazon Redshift engine version to which the cluster parameter group
-- applies. The cluster engine version determines the set of parameters.
--
-- To get a list of valid parameter group family names, you can call 'DescribeClusterParameterGroups'. By default, Amazon Redshift returns a list of all the parameter groups that
-- are owned by your AWS account, including the default parameter groups for
-- each Amazon Redshift engine version. The parameter group family names
-- associated with the default parameter groups provide you the valid values.
-- For example, a valid family name is "redshift-1.0".
--
ccpgParameterGroupFamily :: Lens' CreateClusterParameterGroup Text
ccpgParameterGroupFamily =
    lens _ccpgParameterGroupFamily
        (\s a -> s { _ccpgParameterGroupFamily = a })

-- | The name of the cluster parameter group.
--
-- Constraints:
--
-- Must be 1 to 255 alphanumeric characters or hyphens First character must be
-- a letter. Cannot end with a hyphen or contain two consecutive hyphens. Must
-- be unique withing your AWS account.  This value is stored as a lower-case
-- string.
ccpgParameterGroupName :: Lens' CreateClusterParameterGroup Text
ccpgParameterGroupName =
    lens _ccpgParameterGroupName (\s a -> s { _ccpgParameterGroupName = a })

-- | A list of tag instances.
--
ccpgTags :: Lens' CreateClusterParameterGroup [Tag]
ccpgTags = lens _ccpgTags (\s a -> s { _ccpgTags = a }) . _List

newtype CreateClusterParameterGroupResponse = CreateClusterParameterGroupResponse
    { _ccpgrClusterParameterGroup :: Maybe ClusterParameterGroup
    } deriving (Eq, Show)

-- | 'CreateClusterParameterGroupResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccpgrClusterParameterGroup' @::@ 'Maybe' 'ClusterParameterGroup'
--
createClusterParameterGroupResponse :: CreateClusterParameterGroupResponse
createClusterParameterGroupResponse = CreateClusterParameterGroupResponse
    { _ccpgrClusterParameterGroup = Nothing
    }

ccpgrClusterParameterGroup :: Lens' CreateClusterParameterGroupResponse (Maybe ClusterParameterGroup)
ccpgrClusterParameterGroup =
    lens _ccpgrClusterParameterGroup
        (\s a -> s { _ccpgrClusterParameterGroup = a })

instance ToPath CreateClusterParameterGroup where
    toPath = const "/"

instance ToQuery CreateClusterParameterGroup where
    toQuery CreateClusterParameterGroup{..} = mconcat
        [ "Description"          =? _ccpgDescription
        , "ParameterGroupFamily" =? _ccpgParameterGroupFamily
        , "ParameterGroupName"   =? _ccpgParameterGroupName
        , "Tags"                 =? _ccpgTags
        ]

instance ToHeaders CreateClusterParameterGroup

instance AWSRequest CreateClusterParameterGroup where
    type Sv CreateClusterParameterGroup = Redshift
    type Rs CreateClusterParameterGroup = CreateClusterParameterGroupResponse

    request  = post "CreateClusterParameterGroup"
    response = xmlResponse

instance FromXML CreateClusterParameterGroupResponse where
    parseXML = withElement "CreateClusterParameterGroupResult" $ \x -> CreateClusterParameterGroupResponse
        <$> x .@? "ClusterParameterGroup"
