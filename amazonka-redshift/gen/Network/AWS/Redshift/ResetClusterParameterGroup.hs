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

-- Module      : Network.AWS.Redshift.ResetClusterParameterGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Sets one or more parameters of the specified parameter group to their
-- default values and sets the source values of the parameters to
-- "engine-default". To reset the entire parameter group specify the
-- ResetAllParameters parameter. For parameter changes to take effect you must
-- reboot any associated clusters.
module Network.AWS.Redshift.ResetClusterParameterGroup
    (
    -- * Request
      ResetClusterParameterGroupMessage
    -- ** Request constructor
    , resetClusterParameterGroupMessage
    -- ** Request lenses
    , rcpgmParameterGroupName
    , rcpgmParameters
    , rcpgmResetAllParameters

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

data ResetClusterParameterGroupMessage = ResetClusterParameterGroupMessage
    { _rcpgmParameterGroupName :: Text
    , _rcpgmParameters         :: [Parameter]
    , _rcpgmResetAllParameters :: Maybe Bool
    } deriving (Eq, Show, Generic)

-- | 'ResetClusterParameterGroupMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rcpgmParameterGroupName' @::@ 'Text'
--
-- * 'rcpgmParameters' @::@ ['Parameter']
--
-- * 'rcpgmResetAllParameters' @::@ 'Maybe' 'Bool'
--
resetClusterParameterGroupMessage :: Text -- ^ 'rcpgmParameterGroupName'
                                  -> ResetClusterParameterGroupMessage
resetClusterParameterGroupMessage p1 = ResetClusterParameterGroupMessage
    { _rcpgmParameterGroupName = p1
    , _rcpgmResetAllParameters = Nothing
    , _rcpgmParameters         = mempty
    }

-- | The name of the cluster parameter group to be reset.
rcpgmParameterGroupName :: Lens' ResetClusterParameterGroupMessage Text
rcpgmParameterGroupName =
    lens _rcpgmParameterGroupName (\s a -> s { _rcpgmParameterGroupName = a })

-- | An array of names of parameters to be reset. If ResetAllParameters option
-- is not used, then at least one parameter name must be supplied.
-- Constraints: A maximum of 20 parameters can be reset in a single request.
rcpgmParameters :: Lens' ResetClusterParameterGroupMessage [Parameter]
rcpgmParameters = lens _rcpgmParameters (\s a -> s { _rcpgmParameters = a })

-- | If true, all parameters in the specified parameter group will be reset to
-- their default values. Default: true.
rcpgmResetAllParameters :: Lens' ResetClusterParameterGroupMessage (Maybe Bool)
rcpgmResetAllParameters =
    lens _rcpgmResetAllParameters (\s a -> s { _rcpgmResetAllParameters = a })
instance ToQuery ResetClusterParameterGroupMessage

instance ToPath ResetClusterParameterGroupMessage where
    toPath = const "/"

instance AWSRequest ResetClusterParameterGroupMessage where
    type Sv ResetClusterParameterGroupMessage = Redshift
    type Rs ResetClusterParameterGroupMessage = ClusterParameterGroupNameMessage

    request  = post "ResetClusterParameterGroup"
    response = xmlResponse $ const decodeCursor
