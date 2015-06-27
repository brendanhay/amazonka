{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.Redshift.ModifyClusterParameterGroup
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Modifies the parameters of a parameter group.
--
-- For more information about managing parameter groups, go to
-- <http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-parameter-groups.html Amazon Redshift Parameter Groups>
-- in the /Amazon Redshift Cluster Management Guide/.
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
    , ClusterParameterGroupNameMessage
    -- ** Response constructor
    , clusterParameterGroupNameMessage
    -- ** Response lenses
    , cpgnmParameterGroupStatus
    , cpgnmParameterGroupName
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Redshift.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'modifyClusterParameterGroup' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mcpgParameterGroupName'
--
-- * 'mcpgParameters'
data ModifyClusterParameterGroup = ModifyClusterParameterGroup'
    { _mcpgParameterGroupName :: Text
    , _mcpgParameters         :: [Parameter]
    } deriving (Eq,Read,Show)

-- | 'ModifyClusterParameterGroup' smart constructor.
modifyClusterParameterGroup :: Text -> ModifyClusterParameterGroup
modifyClusterParameterGroup pParameterGroupName =
    ModifyClusterParameterGroup'
    { _mcpgParameterGroupName = pParameterGroupName
    , _mcpgParameters = mempty
    }

-- | The name of the parameter group to be modified.
mcpgParameterGroupName :: Lens' ModifyClusterParameterGroup Text
mcpgParameterGroupName = lens _mcpgParameterGroupName (\ s a -> s{_mcpgParameterGroupName = a});

-- | An array of parameters to be modified. A maximum of 20 parameters can be
-- modified in a single request.
--
-- For each parameter to be modified, you must supply at least the
-- parameter name and parameter value; other name-value pairs of the
-- parameter are optional.
--
-- For the workload management (WLM) configuration, you must supply all the
-- name-value pairs in the wlm_json_configuration parameter.
mcpgParameters :: Lens' ModifyClusterParameterGroup [Parameter]
mcpgParameters = lens _mcpgParameters (\ s a -> s{_mcpgParameters = a});

instance AWSRequest ModifyClusterParameterGroup where
        type Sv ModifyClusterParameterGroup = Redshift
        type Rs ModifyClusterParameterGroup =
             ClusterParameterGroupNameMessage
        request = post
        response
          = receiveXMLWrapper
              "ModifyClusterParameterGroupResult"
              (\ s h x -> parseXML x)

instance ToHeaders ModifyClusterParameterGroup where
        toHeaders = const mempty

instance ToPath ModifyClusterParameterGroup where
        toPath = const "/"

instance ToQuery ModifyClusterParameterGroup where
        toQuery ModifyClusterParameterGroup'{..}
          = mconcat
              ["Action" =:
                 ("ModifyClusterParameterGroup" :: ByteString),
               "Version" =: ("2012-12-01" :: ByteString),
               "ParameterGroupName" =: _mcpgParameterGroupName,
               "Parameters" =:
                 toQueryList "Parameter" _mcpgParameters]
