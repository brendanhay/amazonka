{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift
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
-- reboot any associated clusters. https://redshift.us-east-1.amazonaws.com/
-- ?Action=ResetClusterParameterGroup &ParameterGroupName=parametergroup1
-- &Parameters.member.1.ParameterName=extra_float_digits &Version=2012-12-01
-- &x-amz-algorithm=AWS4-HMAC-SHA256
-- &x-amz-credential=AKIAIOSFODNN7EXAMPLE/20121208/us-east-1/redshift/aws4_request
-- &x-amz-date=20121208T020847Z
-- &x-amz-signedheaders=content-type;host;x-amz-date Your parameter group has
-- been updated but changes won't get applied until you reboot the associated
-- Clusters. parametergroup1 625d23c1-40dc-11e2-8a25-eb010998df4e.
module Network.AWS.Redshift
    (
    -- * Request
      ResetClusterParameterGroup
    -- ** Request constructor
    , mkResetClusterParameterGroup
    -- ** Request lenses
    , rcpgParameterGroupName
    , rcpgResetAllParameters
    , rcpgParameters

    -- * Response
    , ResetClusterParameterGroupResponse
    -- ** Response constructor
    , mkResetClusterParameterGroupResponse
    -- ** Response lenses
    , rcpgrParameterGroupName
    , rcpgrParameterGroupStatus
    ) where

import Network.AWS.Request.Query
import Network.AWS.Redshift.Types
import Network.AWS.Prelude

-- | 
data ResetClusterParameterGroup = ResetClusterParameterGroup
    { _rcpgParameterGroupName :: Text
    , _rcpgResetAllParameters :: Maybe Bool
    , _rcpgParameters :: [Parameter]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ResetClusterParameterGroup' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ParameterGroupName ::@ @Text@
--
-- * @ResetAllParameters ::@ @Maybe Bool@
--
-- * @Parameters ::@ @[Parameter]@
--
mkResetClusterParameterGroup :: Text -- ^ 'rcpgParameterGroupName'
                             -> ResetClusterParameterGroup
mkResetClusterParameterGroup p1 = ResetClusterParameterGroup
    { _rcpgParameterGroupName = p1
    , _rcpgResetAllParameters = Nothing
    , _rcpgParameters = mempty
    }

-- | The name of the cluster parameter group to be reset.
rcpgParameterGroupName :: Lens' ResetClusterParameterGroup Text
rcpgParameterGroupName =
    lens _rcpgParameterGroupName (\s a -> s { _rcpgParameterGroupName = a })

-- | If true, all parameters in the specified parameter group will be reset to
-- their default values. Default: true.
rcpgResetAllParameters :: Lens' ResetClusterParameterGroup (Maybe Bool)
rcpgResetAllParameters =
    lens _rcpgResetAllParameters (\s a -> s { _rcpgResetAllParameters = a })

-- | An array of names of parameters to be reset. If ResetAllParameters option
-- is not used, then at least one parameter name must be supplied.
-- Constraints: A maximum of 20 parameters can be reset in a single request.
rcpgParameters :: Lens' ResetClusterParameterGroup [Parameter]
rcpgParameters = lens _rcpgParameters (\s a -> s { _rcpgParameters = a })

instance ToQuery ResetClusterParameterGroup where
    toQuery = genericQuery def

-- | Contains the output from the ModifyClusterParameterGroup and
-- ResetClusterParameterGroup actions and indicate the parameter group
-- involved and the status of the operation on the parameter group.
data ResetClusterParameterGroupResponse = ResetClusterParameterGroupResponse
    { _rcpgrParameterGroupName :: Maybe Text
    , _rcpgrParameterGroupStatus :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ResetClusterParameterGroupResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ParameterGroupName ::@ @Maybe Text@
--
-- * @ParameterGroupStatus ::@ @Maybe Text@
--
mkResetClusterParameterGroupResponse :: ResetClusterParameterGroupResponse
mkResetClusterParameterGroupResponse = ResetClusterParameterGroupResponse
    { _rcpgrParameterGroupName = Nothing
    , _rcpgrParameterGroupStatus = Nothing
    }

-- | The name of the cluster parameter group.
rcpgrParameterGroupName :: Lens' ResetClusterParameterGroupResponse (Maybe Text)
rcpgrParameterGroupName =
    lens _rcpgrParameterGroupName
         (\s a -> s { _rcpgrParameterGroupName = a })

-- | The status of the parameter group. For example, if you made a change to a
-- parameter group name-value pair, then the change could be pending a reboot
-- of an associated cluster.
rcpgrParameterGroupStatus :: Lens' ResetClusterParameterGroupResponse (Maybe Text)
rcpgrParameterGroupStatus =
    lens _rcpgrParameterGroupStatus
         (\s a -> s { _rcpgrParameterGroupStatus = a })

instance FromXML ResetClusterParameterGroupResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ResetClusterParameterGroup where
    type Sv ResetClusterParameterGroup = Redshift
    type Rs ResetClusterParameterGroup = ResetClusterParameterGroupResponse

    request = post "ResetClusterParameterGroup"
    response _ = xmlResponse
