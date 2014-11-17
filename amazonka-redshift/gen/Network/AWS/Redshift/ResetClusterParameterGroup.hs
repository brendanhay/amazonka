{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
--
-- <http://docs.aws.amazon.com/redshift/latest/APIReference/API_ResetClusterParameterGroup.html>
module Network.AWS.Redshift.ResetClusterParameterGroup
    (
    -- * Request
      ResetClusterParameterGroup
    -- ** Request constructor
    , resetClusterParameterGroup
    -- ** Request lenses
    , rcpgParameterGroupName
    , rcpgParameters
    , rcpgResetAllParameters

    -- * Response
    , ResetClusterParameterGroupResponse
    -- ** Response constructor
    , resetClusterParameterGroupResponse
    -- ** Response lenses
    , rcpgrParameterGroupName
    , rcpgrParameterGroupStatus
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.Redshift.Types
import qualified GHC.Exts

data ResetClusterParameterGroup = ResetClusterParameterGroup
    { _rcpgParameterGroupName :: Text
    , _rcpgParameters         :: [Parameter]
    , _rcpgResetAllParameters :: Maybe Bool
    } deriving (Eq, Show, Generic)

-- | 'ResetClusterParameterGroup' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rcpgParameterGroupName' @::@ 'Text'
--
-- * 'rcpgParameters' @::@ ['Parameter']
--
-- * 'rcpgResetAllParameters' @::@ 'Maybe' 'Bool'
--
resetClusterParameterGroup :: Text -- ^ 'rcpgParameterGroupName'
                           -> ResetClusterParameterGroup
resetClusterParameterGroup p1 = ResetClusterParameterGroup
    { _rcpgParameterGroupName = p1
    , _rcpgResetAllParameters = Nothing
    , _rcpgParameters         = mempty
    }

-- | The name of the cluster parameter group to be reset.
rcpgParameterGroupName :: Lens' ResetClusterParameterGroup Text
rcpgParameterGroupName =
    lens _rcpgParameterGroupName (\s a -> s { _rcpgParameterGroupName = a })

-- | An array of names of parameters to be reset. If ResetAllParameters option
-- is not used, then at least one parameter name must be supplied.
-- Constraints: A maximum of 20 parameters can be reset in a single request.
rcpgParameters :: Lens' ResetClusterParameterGroup [Parameter]
rcpgParameters = lens _rcpgParameters (\s a -> s { _rcpgParameters = a })

-- | If true, all parameters in the specified parameter group will be reset to
-- their default values. Default: true.
rcpgResetAllParameters :: Lens' ResetClusterParameterGroup (Maybe Bool)
rcpgResetAllParameters =
    lens _rcpgResetAllParameters (\s a -> s { _rcpgResetAllParameters = a })

data ResetClusterParameterGroupResponse = ResetClusterParameterGroupResponse
    { _rcpgrParameterGroupName   :: Maybe Text
    , _rcpgrParameterGroupStatus :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'ResetClusterParameterGroupResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rcpgrParameterGroupName' @::@ 'Maybe' 'Text'
--
-- * 'rcpgrParameterGroupStatus' @::@ 'Maybe' 'Text'
--
resetClusterParameterGroupResponse :: ResetClusterParameterGroupResponse
resetClusterParameterGroupResponse = ResetClusterParameterGroupResponse
    { _rcpgrParameterGroupName   = Nothing
    , _rcpgrParameterGroupStatus = Nothing
    }

-- | The name of the cluster parameter group.
rcpgrParameterGroupName :: Lens' ResetClusterParameterGroupResponse (Maybe Text)
rcpgrParameterGroupName =
    lens _rcpgrParameterGroupName (\s a -> s { _rcpgrParameterGroupName = a })

-- | The status of the parameter group. For example, if you made a change to a
-- parameter group name-value pair, then the change could be pending a
-- reboot of an associated cluster.
rcpgrParameterGroupStatus :: Lens' ResetClusterParameterGroupResponse (Maybe Text)
rcpgrParameterGroupStatus =
    lens _rcpgrParameterGroupStatus
        (\s a -> s { _rcpgrParameterGroupStatus = a })

instance ToPath ResetClusterParameterGroup where
    toPath = const "/"

instance ToQuery ResetClusterParameterGroup

instance ToHeaders ResetClusterParameterGroup

instance AWSRequest ResetClusterParameterGroup where
    type Sv ResetClusterParameterGroup = Redshift
    type Rs ResetClusterParameterGroup = ResetClusterParameterGroupResponse

    request  = post "ResetClusterParameterGroup"
    response = xmlResponse

instance FromXML ResetClusterParameterGroupResponse where
    parseXML c = ResetClusterParameterGroupResponse
        <$> c .:? "ParameterGroupName"
        <*> c .:? "ParameterGroupStatus"
