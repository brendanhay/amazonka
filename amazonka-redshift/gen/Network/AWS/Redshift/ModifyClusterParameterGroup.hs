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
-- Amazon Redshift Management Guide.
--
-- <http://docs.aws.amazon.com/redshift/latest/APIReference/API_ModifyClusterParameterGroup.html>
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

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.Redshift.Types
import qualified GHC.Exts

data ModifyClusterParameterGroup = ModifyClusterParameterGroup
    { _mcpgParameterGroupName :: Text
    , _mcpgParameters         :: List "Parameter" Parameter
    } deriving (Eq, Show)

-- | 'ModifyClusterParameterGroup' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mcpgParameterGroupName' @::@ 'Text'
--
-- * 'mcpgParameters' @::@ ['Parameter']
--
modifyClusterParameterGroup :: Text -- ^ 'mcpgParameterGroupName'
                            -> ModifyClusterParameterGroup
modifyClusterParameterGroup p1 = ModifyClusterParameterGroup
    { _mcpgParameterGroupName = p1
    , _mcpgParameters         = mempty
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
mcpgParameters = lens _mcpgParameters (\s a -> s { _mcpgParameters = a }) . _List

data ModifyClusterParameterGroupResponse = ModifyClusterParameterGroupResponse
    { _mcpgrParameterGroupName   :: Maybe Text
    , _mcpgrParameterGroupStatus :: Maybe Text
    } deriving (Eq, Ord, Show)

-- | 'ModifyClusterParameterGroupResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mcpgrParameterGroupName' @::@ 'Maybe' 'Text'
--
-- * 'mcpgrParameterGroupStatus' @::@ 'Maybe' 'Text'
--
modifyClusterParameterGroupResponse :: ModifyClusterParameterGroupResponse
modifyClusterParameterGroupResponse = ModifyClusterParameterGroupResponse
    { _mcpgrParameterGroupName   = Nothing
    , _mcpgrParameterGroupStatus = Nothing
    }

-- | The name of the cluster parameter group.
mcpgrParameterGroupName :: Lens' ModifyClusterParameterGroupResponse (Maybe Text)
mcpgrParameterGroupName =
    lens _mcpgrParameterGroupName (\s a -> s { _mcpgrParameterGroupName = a })

-- | The status of the parameter group. For example, if you made a change to a
-- parameter group name-value pair, then the change could be pending a
-- reboot of an associated cluster.
mcpgrParameterGroupStatus :: Lens' ModifyClusterParameterGroupResponse (Maybe Text)
mcpgrParameterGroupStatus =
    lens _mcpgrParameterGroupStatus
        (\s a -> s { _mcpgrParameterGroupStatus = a })

instance ToPath ModifyClusterParameterGroup where
    toPath = const "/"

instance ToQuery ModifyClusterParameterGroup where
    toQuery ModifyClusterParameterGroup{..} = mconcat
        [ "ParameterGroupName" =? _mcpgParameterGroupName
        , "Parameters"         =? _mcpgParameters
        ]

instance ToHeaders ModifyClusterParameterGroup

instance AWSRequest ModifyClusterParameterGroup where
    type Sv ModifyClusterParameterGroup = Redshift
    type Rs ModifyClusterParameterGroup = ModifyClusterParameterGroupResponse

    request  = post "ModifyClusterParameterGroup"
    response = xmlResponse

instance FromXML ModifyClusterParameterGroupResponse where
    parseXML = withElement "ModifyClusterParameterGroupResult" $ \x -> ModifyClusterParameterGroupResponse
        <$> x .@? "ParameterGroupName"
        <*> x .@? "ParameterGroupStatus"
