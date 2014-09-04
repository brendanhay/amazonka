{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.V2012_12_01.ModifyClusterParameterGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Modifies the parameters of a parameter group. For more information about
-- managing parameter groups, go to Amazon Redshift Parameter Groups in the
-- Amazon Redshift Management Guide. https://redshift.us-east-1.amazonaws.com/
-- ?Action=ModifyClusterParameterGroup &ParameterGroupName=parametergroup1
-- &Parameters.member.1.ParameterName=extra_float_digits
-- &Parameters.member.1.ParameterValue=2
-- &Parameters.member.2.ParameterName=wlm_json_configuration
-- &Parameters.member.2.ParameterValue=[{"user_group":["example_user_group1"],"query_group":["example_query_group1"],"query_concurrency":7},{"query_concurrency":5}]
-- &Version=2012-12-01 &x-amz-algorithm=AWS4-HMAC-SHA256
-- &x-amz-credential=AKIAIOSFODNN7EXAMPLE/20121208/us-east-1/redshift/aws4_request
-- &x-amz-date=20121208T022525Z
-- &x-amz-signedheaders=content-type;host;x-amz-date Your parameter group has
-- been updated but changes won't get applied until you reboot the associated
-- Clusters. parametergroup1 86e64043-40de-11e2-8a25-eb010998df4e.
module Network.AWS.Redshift.V2012_12_01.ModifyClusterParameterGroup
    (
    -- * Request
      ModifyClusterParameterGroup
    -- ** Request constructor
    , modifyClusterParameterGroup
    -- ** Request lenses
    , mcpgmParameters
    , mcpgmParameterGroupName

    -- * Response
    , ModifyClusterParameterGroupResponse
    -- ** Response lenses
    , cpgnmParameterGroupName
    , cpgnmParameterGroupStatus
    ) where

import Network.AWS.Request.Query
import Network.AWS.Redshift.V2012_12_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'ModifyClusterParameterGroup' request.
modifyClusterParameterGroup :: [Parameter] -- ^ 'mcpgmParameters'
                            -> Text -- ^ 'mcpgmParameterGroupName'
                            -> ModifyClusterParameterGroup
modifyClusterParameterGroup p1 p2 = ModifyClusterParameterGroup
    { _mcpgmParameters = p1
    , _mcpgmParameterGroupName = p2
    }
{-# INLINE modifyClusterParameterGroup #-}

data ModifyClusterParameterGroup = ModifyClusterParameterGroup
    { _mcpgmParameters :: [Parameter]
      -- ^ An array of parameters to be modified. A maximum of 20 parameters
      -- can be modified in a single request. For each parameter to be
      -- modified, you must supply at least the parameter name and
      -- parameter value; other name-value pairs of the parameter are
      -- optional. For the workload management (WLM) configuration, you
      -- must supply all the name-value pairs in the
      -- wlm_json_configuration parameter.
    , _mcpgmParameterGroupName :: Text
      -- ^ The name of the parameter group to be modified.
    } deriving (Show, Generic)

-- | An array of parameters to be modified. A maximum of 20 parameters can be
-- modified in a single request. For each parameter to be modified, you must
-- supply at least the parameter name and parameter value; other name-value
-- pairs of the parameter are optional. For the workload management (WLM)
-- configuration, you must supply all the name-value pairs in the
-- wlm_json_configuration parameter.
mcpgmParameters :: Lens' ModifyClusterParameterGroup ([Parameter])
mcpgmParameters f x =
    f (_mcpgmParameters x)
        <&> \y -> x { _mcpgmParameters = y }
{-# INLINE mcpgmParameters #-}

-- | The name of the parameter group to be modified.
mcpgmParameterGroupName :: Lens' ModifyClusterParameterGroup (Text)
mcpgmParameterGroupName f x =
    f (_mcpgmParameterGroupName x)
        <&> \y -> x { _mcpgmParameterGroupName = y }
{-# INLINE mcpgmParameterGroupName #-}

instance ToQuery ModifyClusterParameterGroup where
    toQuery = genericQuery def

data ModifyClusterParameterGroupResponse = ModifyClusterParameterGroupResponse
    { _cpgnmParameterGroupName :: Maybe Text
      -- ^ The name of the cluster parameter group.
    , _cpgnmParameterGroupStatus :: Maybe Text
      -- ^ The status of the parameter group. For example, if you made a
      -- change to a parameter group name-value pair, then the change
      -- could be pending a reboot of an associated cluster.
    } deriving (Show, Generic)

-- | The name of the cluster parameter group.
cpgnmParameterGroupName :: Lens' ModifyClusterParameterGroupResponse (Maybe Text)
cpgnmParameterGroupName f x =
    f (_cpgnmParameterGroupName x)
        <&> \y -> x { _cpgnmParameterGroupName = y }
{-# INLINE cpgnmParameterGroupName #-}

-- | The status of the parameter group. For example, if you made a change to a
-- parameter group name-value pair, then the change could be pending a reboot
-- of an associated cluster.
cpgnmParameterGroupStatus :: Lens' ModifyClusterParameterGroupResponse (Maybe Text)
cpgnmParameterGroupStatus f x =
    f (_cpgnmParameterGroupStatus x)
        <&> \y -> x { _cpgnmParameterGroupStatus = y }
{-# INLINE cpgnmParameterGroupStatus #-}

instance FromXML ModifyClusterParameterGroupResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ModifyClusterParameterGroup where
    type Sv ModifyClusterParameterGroup = Redshift
    type Rs ModifyClusterParameterGroup = ModifyClusterParameterGroupResponse

    request = post "ModifyClusterParameterGroup"
    response _ = xmlResponse
