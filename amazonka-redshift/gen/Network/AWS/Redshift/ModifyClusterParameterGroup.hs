{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

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
module Network.AWS.Redshift.ModifyClusterParameterGroup
    (
    -- * Request
      ModifyClusterParameterGroupMessage
    -- ** Request constructor
    , modifyClusterParameterGroup
    -- ** Request lenses
    , mcpgmParameterGroupName
    , mcpgmParameters

    -- * Response
    , ClusterParameterGroupNameMessage
    -- ** Response constructor
    , clusterParameterGroupNameMessage
    -- ** Response lenses
    , cpgnmParameterGroupName
    , cpgnmParameterGroupStatus
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.Redshift.Types

data ModifyClusterParameterGroupMessage = ModifyClusterParameterGroupMessage
    { _mcpgmParameterGroupName :: Text
    , _mcpgmParameters         :: [Parameter]
    } deriving (Eq, Show, Generic)

-- | 'ModifyClusterParameterGroupMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mcpgmParameterGroupName' @::@ 'Text'
--
-- * 'mcpgmParameters' @::@ ['Parameter']
--
modifyClusterParameterGroup :: Text -- ^ 'mcpgmParameterGroupName'
                            -> ModifyClusterParameterGroupMessage
modifyClusterParameterGroup p1 = ModifyClusterParameterGroupMessage
    { _mcpgmParameterGroupName = p1
    , _mcpgmParameters         = mempty
    }

-- | The name of the parameter group to be modified.
mcpgmParameterGroupName :: Lens' ModifyClusterParameterGroupMessage Text
mcpgmParameterGroupName =
    lens _mcpgmParameterGroupName (\s a -> s { _mcpgmParameterGroupName = a })

-- | An array of parameters to be modified. A maximum of 20 parameters can be
-- modified in a single request. For each parameter to be modified, you must
-- supply at least the parameter name and parameter value; other name-value
-- pairs of the parameter are optional. For the workload management (WLM)
-- configuration, you must supply all the name-value pairs in the
-- wlm_json_configuration parameter.
mcpgmParameters :: Lens' ModifyClusterParameterGroupMessage [Parameter]
mcpgmParameters = lens _mcpgmParameters (\s a -> s { _mcpgmParameters = a })

instance ToQuery ModifyClusterParameterGroupMessage

instance ToPath ModifyClusterParameterGroupMessage where
    toPath = const "/"

instance AWSRequest ModifyClusterParameterGroupMessage where
    type Sv ModifyClusterParameterGroupMessage = Redshift
    type Rs ModifyClusterParameterGroupMessage = ClusterParameterGroupNameMessage

    request  = post "ModifyClusterParameterGroup"
    response = xmlResponse $ const decodeCursor
