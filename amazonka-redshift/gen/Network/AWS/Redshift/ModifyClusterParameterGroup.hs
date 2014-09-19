{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.ModifyClusterParameterGroup
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
module Network.AWS.Redshift.ModifyClusterParameterGroup
    (
    -- * Request
      ModifyClusterParameterGroup
    -- ** Request constructor
    , modifyClusterParameterGroup
    -- ** Request lenses
    , mcpgParameterGroupName
    , mcpgParameters

    -- * Response
    , ModifyClusterParameterGroupResponse
    -- ** Response constructor
    , modifyClusterParameterGroupResponse
    -- ** Response lenses
    , mcpgrParameterGroupName
    , mcpgrParameterGroupStatus
    ) where

import Network.AWS.Request.Query
import Network.AWS.Redshift.Types
import Network.AWS.Prelude

-- | 
data ModifyClusterParameterGroup = ModifyClusterParameterGroup
    { _mcpgParameterGroupName :: Text
    , _mcpgParameters :: [Parameter]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ModifyClusterParameterGroup' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ParameterGroupName ::@ @Text@
--
-- * @Parameters ::@ @[Parameter]@
--
modifyClusterParameterGroup :: Text -- ^ 'mcpgParameterGroupName'
                            -> [Parameter] -- ^ 'mcpgParameters'
                            -> ModifyClusterParameterGroup
modifyClusterParameterGroup p1 p2 = ModifyClusterParameterGroup
    { _mcpgParameterGroupName = p1
    , _mcpgParameters = p2
    }

-- | The name of the parameter group to be modified.
mcpgParameterGroupName :: Lens' ModifyClusterParameterGroup Text
mcpgParameterGroupName =
    lens _mcpgParameterGroupName (\s a -> s { _mcpgParameterGroupName = a })

-- | An array of parameters to be modified. A maximum of 20 parameters can be
-- modified in a single request. For each parameter to be modified, you must
-- supply at least the parameter name and parameter value; other name-value
-- pairs of the parameter are optional. For the workload management (WLM)
-- configuration, you must supply all the name-value pairs in the
-- wlm_json_configuration parameter.
mcpgParameters :: Lens' ModifyClusterParameterGroup [Parameter]
mcpgParameters = lens _mcpgParameters (\s a -> s { _mcpgParameters = a })

instance ToQuery ModifyClusterParameterGroup where
    toQuery = genericQuery def

-- | Contains the output from the ModifyClusterParameterGroup and
-- ResetClusterParameterGroup actions and indicate the parameter group
-- involved and the status of the operation on the parameter group.
data ModifyClusterParameterGroupResponse = ModifyClusterParameterGroupResponse
    { _mcpgrParameterGroupName :: Maybe Text
    , _mcpgrParameterGroupStatus :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ModifyClusterParameterGroupResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ParameterGroupName ::@ @Maybe Text@
--
-- * @ParameterGroupStatus ::@ @Maybe Text@
--
modifyClusterParameterGroupResponse :: ModifyClusterParameterGroupResponse
modifyClusterParameterGroupResponse = ModifyClusterParameterGroupResponse
    { _mcpgrParameterGroupName = Nothing
    , _mcpgrParameterGroupStatus = Nothing
    }

-- | The name of the cluster parameter group.
mcpgrParameterGroupName :: Lens' ModifyClusterParameterGroupResponse (Maybe Text)
mcpgrParameterGroupName =
    lens _mcpgrParameterGroupName
         (\s a -> s { _mcpgrParameterGroupName = a })

-- | The status of the parameter group. For example, if you made a change to a
-- parameter group name-value pair, then the change could be pending a reboot
-- of an associated cluster.
mcpgrParameterGroupStatus :: Lens' ModifyClusterParameterGroupResponse (Maybe Text)
mcpgrParameterGroupStatus =
    lens _mcpgrParameterGroupStatus
         (\s a -> s { _mcpgrParameterGroupStatus = a })

instance FromXML ModifyClusterParameterGroupResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ModifyClusterParameterGroup where
    type Sv ModifyClusterParameterGroup = Redshift
    type Rs ModifyClusterParameterGroup = ModifyClusterParameterGroupResponse

    request = post "ModifyClusterParameterGroup"
    response _ = xmlResponse
