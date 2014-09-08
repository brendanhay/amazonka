{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.V2012_12_01.CreateClusterParameterGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates an Amazon Redshift parameter group. Creating parameter groups is
-- independent of creating clusters. You can associate a cluster with a
-- parameter group when you create the cluster. You can also associate an
-- existing cluster with a parameter group after the cluster is created by
-- using ModifyCluster. Parameters in the parameter group define specific
-- behavior that applies to the databases you create on the cluster. For more
-- information about managing parameter groups, go to Amazon Redshift
-- Parameter Groups in the Amazon Redshift Management Guide.
-- https://redshift.us-east-1.amazonaws.com/
-- ?Action=CreateClusterParameterGroup &Description=description my parameter
-- group &ParameterGroupFamily=redshift-1.0
-- &ParameterGroupName=parametergroup1 &Version=2012-12-01
-- &x-amz-algorithm=AWS4-HMAC-SHA256
-- &x-amz-credential=AKIAIOSFODNN7EXAMPLE/20130123/us-east-1/redshift/aws4_request
-- &x-amz-date=20130123T002544Z
-- &x-amz-signedheaders=content-type;host;x-amz-date redshift-1.0 description
-- my parameter group parametergroup1 6d6df847-64f3-11e2-bea9-49e0ce183f07.
module Network.AWS.Redshift.V2012_12_01.CreateClusterParameterGroup
    (
    -- * Request
      CreateClusterParameterGroup
    -- ** Request constructor
    , mkCreateClusterParameterGroup
    -- ** Request lenses
    , ccpgParameterGroupName
    , ccpgParameterGroupFamily
    , ccpgDescription

    -- * Response
    , CreateClusterParameterGroupResponse
    -- ** Response lenses
    , ccpgrClusterParameterGroup
    ) where

import Network.AWS.Request.Query
import Network.AWS.Redshift.V2012_12_01.Types
import Network.AWS.Prelude

-- | 
data CreateClusterParameterGroup = CreateClusterParameterGroup
    { _ccpgParameterGroupName :: Text
    , _ccpgParameterGroupFamily :: Text
    , _ccpgDescription :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateClusterParameterGroup' request.
mkCreateClusterParameterGroup :: Text -- ^ 'ccpgParameterGroupName'
                              -> Text -- ^ 'ccpgParameterGroupFamily'
                              -> Text -- ^ 'ccpgDescription'
                              -> CreateClusterParameterGroup
mkCreateClusterParameterGroup p1 p2 p3 = CreateClusterParameterGroup
    { _ccpgParameterGroupName = p1
    , _ccpgParameterGroupFamily = p2
    , _ccpgDescription = p3
    }

-- | The name of the cluster parameter group. Constraints: Must be 1 to 255
-- alphanumeric characters or hyphens First character must be a letter. Cannot
-- end with a hyphen or contain two consecutive hyphens. Must be unique
-- withing your AWS account. This value is stored as a lower-case string.
ccpgParameterGroupName :: Lens' CreateClusterParameterGroup Text
ccpgParameterGroupName =
    lens _ccpgParameterGroupName (\s a -> s { _ccpgParameterGroupName = a })

-- | The Amazon Redshift engine version to which the cluster parameter group
-- applies. The cluster engine version determines the set of parameters. To
-- get a list of valid parameter group family names, you can call
-- DescribeClusterParameterGroups. By default, Amazon Redshift returns a list
-- of all the parameter groups that are owned by your AWS account, including
-- the default parameter groups for each Amazon Redshift engine version. The
-- parameter group family names associated with the default parameter groups
-- provide you the valid values. For example, a valid family name is
-- "redshift-1.0".
ccpgParameterGroupFamily :: Lens' CreateClusterParameterGroup Text
ccpgParameterGroupFamily =
    lens _ccpgParameterGroupFamily
         (\s a -> s { _ccpgParameterGroupFamily = a })

-- | A description of the parameter group.
ccpgDescription :: Lens' CreateClusterParameterGroup Text
ccpgDescription = lens _ccpgDescription (\s a -> s { _ccpgDescription = a })

instance ToQuery CreateClusterParameterGroup where
    toQuery = genericQuery def

newtype CreateClusterParameterGroupResponse = CreateClusterParameterGroupResponse
    { _ccpgrClusterParameterGroup :: Maybe ClusterParameterGroup
    } deriving (Show, Generic)

-- | Describes a parameter group.
ccpgrClusterParameterGroup :: Lens' CreateClusterParameterGroupResponse (Maybe ClusterParameterGroup)
ccpgrClusterParameterGroup =
    lens _ccpgrClusterParameterGroup
         (\s a -> s { _ccpgrClusterParameterGroup = a })

instance FromXML CreateClusterParameterGroupResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreateClusterParameterGroup where
    type Sv CreateClusterParameterGroup = Redshift
    type Rs CreateClusterParameterGroup = CreateClusterParameterGroupResponse

    request = post "CreateClusterParameterGroup"
    response _ = xmlResponse
