{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.ModifyClusterParameterGroup
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Modifies the parameters of a parameter group.
--
-- For more information about parameters and parameter groups, go to
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
    , mcpgrqParameterGroupName
    , mcpgrqParameters

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
-- * 'mcpgrqParameterGroupName'
--
-- * 'mcpgrqParameters'
data ModifyClusterParameterGroup = ModifyClusterParameterGroup'
    { _mcpgrqParameterGroupName :: !Text
    , _mcpgrqParameters         :: ![Parameter]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ModifyClusterParameterGroup' smart constructor.
modifyClusterParameterGroup :: Text -> ModifyClusterParameterGroup
modifyClusterParameterGroup pParameterGroupName =
    ModifyClusterParameterGroup'
    { _mcpgrqParameterGroupName = pParameterGroupName
    , _mcpgrqParameters = mempty
    }

-- | The name of the parameter group to be modified.
mcpgrqParameterGroupName :: Lens' ModifyClusterParameterGroup Text
mcpgrqParameterGroupName = lens _mcpgrqParameterGroupName (\ s a -> s{_mcpgrqParameterGroupName = a});

-- | An array of parameters to be modified. A maximum of 20 parameters can be
-- modified in a single request.
--
-- For each parameter to be modified, you must supply at least the
-- parameter name and parameter value; other name-value pairs of the
-- parameter are optional.
--
-- For the workload management (WLM) configuration, you must supply all the
-- name-value pairs in the wlm_json_configuration parameter.
mcpgrqParameters :: Lens' ModifyClusterParameterGroup [Parameter]
mcpgrqParameters = lens _mcpgrqParameters (\ s a -> s{_mcpgrqParameters = a});

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
               "ParameterGroupName" =: _mcpgrqParameterGroupName,
               "Parameters" =:
                 toQueryList "Parameter" _mcpgrqParameters]
