{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

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
module Network.AWS.Redshift.V2012_12_01.CreateClusterParameterGroup where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.Redshift.V2012_12_01.Types
import Network.AWS.Prelude

data CreateClusterParameterGroup = CreateClusterParameterGroup
    { _ccpgmParameterGroupFamily :: Text
      -- ^ The Amazon Redshift engine version to which the cluster parameter
      -- group applies. The cluster engine version determines the set of
      -- parameters. To get a list of valid parameter group family names,
      -- you can call DescribeClusterParameterGroups. By default, Amazon
      -- Redshift returns a list of all the parameter groups that are
      -- owned by your AWS account, including the default parameter groups
      -- for each Amazon Redshift engine version. The parameter group
      -- family names associated with the default parameter groups provide
      -- you the valid values. For example, a valid family name is
      -- "redshift-1.0".
    , _ccpgmDescription :: Text
      -- ^ A description of the parameter group.
    , _ccpgmParameterGroupName :: Text
      -- ^ The name of the cluster parameter group. Constraints: Must be 1
      -- to 255 alphanumeric characters or hyphens First character must be
      -- a letter. Cannot end with a hyphen or contain two consecutive
      -- hyphens. Must be unique withing your AWS account. This value is
      -- stored as a lower-case string.
    } deriving (Show, Generic)

makeLenses ''CreateClusterParameterGroup

instance ToQuery CreateClusterParameterGroup where
    toQuery = genericToQuery def

data CreateClusterParameterGroupResponse = CreateClusterParameterGroupResponse
    { _cpgcrClusterParameterGroup :: Maybe ClusterParameterGroup
      -- ^ Describes a parameter group.
    } deriving (Show, Generic)

makeLenses ''CreateClusterParameterGroupResponse

instance AWSRequest CreateClusterParameterGroup where
    type Sv CreateClusterParameterGroup = Redshift
    type Rs CreateClusterParameterGroup = CreateClusterParameterGroupResponse

    request = post "CreateClusterParameterGroup"
    response _ = cursorResponse $ \hs xml ->
        pure CreateClusterParameterGroupResponse
            <*> xml %|? "ClusterParameterGroup"
